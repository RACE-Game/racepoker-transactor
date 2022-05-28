(ns depk.transactor.chain.solana
  (:require
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.util :refer [go-try <!?]]
   [cljs.core.async :as a]
   [solana-clj.connection :as conn]
   [solana-clj.publickey :as pubkey]
   [solana-clj.keypair :as keypair]
   [solana-clj.spl-token :as spl-token]
   [solana-clj.system-program :as system-program]
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]
   [solana-clj.extra.instruction-builder :as ib]
   [solana-clj.transaction :as transaction]
   [depk.transactor.constant :as c]
   [depk.transactor.state.config :refer [config]]
   [depk.transactor.log :as log]
   [depk.transactor.chain.state :refer [parse-state-data set-winner-ix-id settle-ix-id
                                        parse-bonus-state-data]]
   [depk.transactor.util :as u]
   [depk.transactor.chain.sync-loop :as sync-loop]
   ["fs" :as fs]
   ["bn.js" :as bn]))

;;; Helpers

(def default-commitment "finalized")

(def settle-status-no-update 0)
(def settle-status-leave 1)

(def settle-type-chips-add 0)
(def settle-type-chips-sub 1)
(def settle-type-no-update 2)

(defn load-private-key
  []
  (if-let [key (some-> (:transactor-solana-keypair @config)
                       (fs/readFileSync)
                       (js/JSON.parse)
                       (js/Uint8Array.from)
                       (keypair/from-secret-key))]
    key
    (throw (ex-info "Can't load fee payer keypair" {}))))

(defn- build-settle-ix-body
  [players settle-map]
  (->>
   players
   (mapcat (fn [{:keys [pubkey]}]
             (let [{:keys [settle-type settle-status amount rake]} (get settle-map (str pubkey))]
               [[(case settle-status
                   :empty-seat settle-status-no-update
                   :leave      settle-status-leave
                   settle-status-no-update)
                 1]
                [(case settle-type
                   :chips-add settle-type-chips-add
                   :chips-sub settle-type-chips-sub
                   :no-update settle-type-no-update
                   settle-type-no-update)
                 1]
                [(or amount (js/BigInt 0)) 8]
                [(or rake (js/BigInt 0)) 8]])))
   (doall)))

(defn- find-player-ata-keys-by-settle-map
  [players mint-pubkey bonus-mint-pubkey settle-map]
  (go-try
   (let [ata-keys-ch (->>
                      players
                      (mapcat
                       (fn [{:keys [pubkey]}]
                         (when pubkey
                           (cond-> []

                             ;; When a player left, add his asset ata
                             (#{:leave} (get-in settle-map [(str pubkey) :settle-status]))
                             (conj (a/go
                                    (let [account-pubkey (pubkey/make-public-key pubkey)
                                          ata-pubkey     (<!?
                                                          (spl-token/get-associated-token-address
                                                           mint-pubkey
                                                           account-pubkey))]
                                      (log/infof "Use asset ATA %s from %s" ata-pubkey pubkey)
                                      {:pubkey     ata-pubkey,
                                       :isSigner   false,
                                       :isWritable true})))


                             ;; When a player pay rake, add his bonus ata
                             (and bonus-mint-pubkey
                                  (> (get-in settle-map [(str pubkey) :rake] (js/BigInt 0))
                                     (js/BigInt 0)))
                             (conj (a/go
                                    (let [account-pubkey (pubkey/make-public-key pubkey)
                                          ata-pubkey     (<!?
                                                          (spl-token/get-associated-token-address
                                                           bonus-mint-pubkey
                                                           account-pubkey))]
                                      (log/infof "Use bonus ATA %s from %s" ata-pubkey pubkey)
                                      {:pubkey     ata-pubkey,
                                       :isSigner   false,
                                       :isWritable true})))))))
                      (a/map vector))]
     (<!? ata-keys-ch))))

(defn- find-player-ata-keys-by-ranking-pos
  [players bonus-mint-pubkey ranking-pos]
  (go-try
   (let [ata-keys-ch (->>
                      ranking-pos
                      (map players)
                      (keep
                       (fn [{:keys [pubkey]}]
                         (when pubkey
                           (a/go
                            (let [account-pubkey (pubkey/make-public-key pubkey)
                                  ata-pubkey     (<!?
                                                  (spl-token/get-associated-token-address
                                                   bonus-mint-pubkey
                                                   account-pubkey))]
                              (log/infof "Use bonus ATA %s from %s" ata-pubkey pubkey)
                              {:pubkey     ata-pubkey,
                               :isSigner   false,
                               :isWritable true})))))
                      (a/map vector))]
     (<!? ata-keys-ch))))

;;; FIXME: what if players GC his ATA

(defn settle
  [game-id game-account-state settle-map]

  (log/infof "🚀Settle game result on Solana: game[%s]" game-id)

  (a/go
   (try
     (let [fee-payer (load-private-key)

           conn (conn/make-connection (get @config :solana-rpc-endpoint))
           game-account-pubkey (pubkey/make-public-key game-id)

           _ (log/infof "📝Settes: %s" settle-map)
           _ (log/infof "📝Current serial: %s" (:settle-serial game-account-state))
           _
           (doseq
             [i    (range c/max-player-num)
              :let [{:keys [pubkey]} (nth (:players game-account-state) i)
                    {:keys [settle-type settle-status amount rake]} (get settle-map (str pubkey))]]

             (log/info "📝-" (str pubkey) settle-type settle-status amount rake))

           dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

           {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey
                   settle-serial bonus-pubkey]}
           game-account-state

           bonus-state (when bonus-pubkey
                         (some-> (<!? (conn/get-account-info conn bonus-pubkey "finalized"))
                                 :data
                                 (parse-bonus-state-data)))

           transactor-ata-pubkey (<!?
                                  (spl-token/get-associated-token-address
                                   mint-pubkey
                                   transactor-pubkey))

           owner-ata-pubkey (<!?
                             (spl-token/get-associated-token-address
                              mint-pubkey
                              owner-pubkey))

           ix-body (build-settle-ix-body players settle-map)

           ix-data (apply ib/make-instruction-data
                          (concat [[settle-ix-id 1]
                                   ;; settle-serial
                                   [(:settle-serial game-account-state) 4]]
                                  ix-body))

           [pda] (<!? (pubkey/find-program-address #js [(buffer-from "stake")]
                                                   dealer-program-id))

           ata-keys
           (<!? (find-player-ata-keys-by-settle-map players
                                                    mint-pubkey
                                                    (:mint-pubkey bonus-state)
                                                    settle-map))

           _ (log/infof "Fee Payer: %s" (keypair/public-key fee-payer))
           _ (log/infof "Game Account: %s" game-account-pubkey)
           _ (log/infof "Stake Account: %s" stake-account-pubkey)
           _ (log/infof "Stake PDA: %s" pda)
           _ (log/infof "Transactor Rake Taker: %s" transactor-ata-pubkey)
           _ (log/infof "Owner Rake Taker: %s" owner-ata-pubkey)
           _ (log/infof "Token Program: %s" spl-token/token-program-id)

           ix-keys
           (cond->
             [{:pubkey     (keypair/public-key fee-payer),
               :isSigner   true,
               :isWritable false}
              {:pubkey     game-account-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     stake-account-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     pda,
               :isSigner   false,
               :isWritable false}
              {:pubkey     transactor-ata-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     owner-ata-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     spl-token/token-program-id,
               :isSigner   false,
               :isWritable false}]

             bonus-pubkey
             (into [{:pubkey     bonus-pubkey,
                     :isSigner   false,
                     :isWritable false}
                    {:pubkey     (:stake-pubkey bonus-state),
                     :isSigner   false,
                     :isWritable true}])

             true
             (into ata-keys))

           ix
           (transaction/make-transaction-instruction
            {:keys      ix-keys,
             :programId (pubkey/make-public-key dealer-program-id),
             :data      ix-data})
           tx
           (doto (transaction/make-transaction)
            (transaction/add ix))

           sig
           (<!? (conn/send-transaction conn tx [fee-payer]))

           ret
           (when sig (<!? (conn/confirm-transaction conn sig "finalized")))

           err
           (when ret (get-in ret [:value :err]))]

       (cond
         (and sig ret (nil? err))
         (do (log/infof "🎉Transaction succeed #%s %s" settle-serial sig)
             :ok)

         :else
         (do (log/errorf "🚨Transaction failed: #%s %s" settle-serial err)
             :err)))
     (catch js/Error e
       (.error js/console e)
       :err))))

(defn set-winner
  [game-id game-account-state ranking]
  (go-try
   (let [_ (log/infof "📝SNG finished, ranking: %s" ranking)
         fee-payer (load-private-key)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))
         game-account-pubkey (pubkey/make-public-key game-id)

         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey
                 settle-serial bonus-pubkey]}
         game-account-state

         _ (log/infof "📝On chain players: %s" players)

         player-id-to-pos (->> (map (comp str :pubkey) players)
                               (map-indexed (fn [idx pk] [pk idx]))
                               (into {}))

         ranking-pos-list (map player-id-to-pos ranking)

         _ (log/infof "📝Ranking positions: %s" ranking-pos-list)

         [pda] (<!? (pubkey/find-program-address #js [(buffer-from "stake")]
                                                 dealer-program-id))

         winner-pubkey (pubkey/make-public-key (first ranking))

         ata-pubkey (<!?
                     (spl-token/get-associated-token-address
                      mint-pubkey
                      winner-pubkey))

         transactor-ata-pubkey (<!?
                                (spl-token/get-associated-token-address
                                 mint-pubkey
                                 transactor-pubkey))

         owner-ata-pubkey (<!?
                           (spl-token/get-associated-token-address
                            mint-pubkey
                            owner-pubkey))

         bonus-state (when bonus-pubkey
                       (some-> (<!? (conn/get-account-info conn bonus-pubkey "finalized"))
                               :data
                               (parse-bonus-state-data)))

         ix-data (ib/make-instruction-data [set-winner-ix-id 1])

         ata-keys
         (if bonus-state
           (<!?
            (find-player-ata-keys-by-ranking-pos players
                                                 (:mint-pubkey bonus-state)
                                                 ranking-pos-list))
           [])

         ix-keys
         (cond->
           [{:pubkey     (keypair/public-key fee-payer),
             :isSigner   true,
             :isWritable false}
            {:pubkey     game-account-pubkey,
             :isSigner   false,
             :isWritable true}
            {:pubkey     stake-account-pubkey,
             :isSigner   false,
             :isWritable true}
            {:pubkey     pda,
             :isSigner   false,
             :isWritable false}
            {:pubkey     transactor-ata-pubkey,
             :isSigner   false,
             :isWritable true}
            {:pubkey     owner-ata-pubkey,
             :isSigner   false,
             :isWritable true}
            {:pubkey     ata-pubkey,
             :isSigner   false,
             :isWritable true}
            {:pubkey     spl-token/token-program-id,
             :isSigner   false,
             :isWritable false}]

           bonus-state
           (into [{:pubkey     bonus-pubkey,
                   :isSigner   false,
                   :isWritable false}
                  {:pubkey     (:stake-pubkey bonus-state),
                   :isSigner   false,
                   :isWritable true}])

           true
           (into ata-keys))

         ix
         (transaction/make-transaction-instruction
          {:keys      ix-keys,
           :programId (pubkey/make-public-key dealer-program-id),
           :data      ix-data})
         tx
         (doto (transaction/make-transaction)
          (transaction/add ix))

         sig
         (<!? (conn/send-transaction conn tx [fee-payer]))

         ret
         (when sig (<!? (conn/confirm-transaction conn sig "finalized")))

         err
         (when ret (get-in ret [:value :err]))]
     (cond
       (and sig ret (nil? err))
       (do (log/infof "🎉Transaction succeed #%s %s" settle-serial sig)
           :ok)

       :else
       (do (log/errorf "🚨Transaction failed: #%s %s" settle-serial err) :err)))))

;;; Implementations

(defrecord SolanaApi [input output pending confirming])

(defn make-solana-api
  []
  (log/info "🏁Use Solana chain api.")
  (let [input      (a/chan 30)
        output     (a/chan 30)
        pending    (atom [])
        confirming (atom [])]
    (->SolanaApi input output pending confirming)))

(defn run-with-retry-loop
  [chain-api game-id settle-serial f]
  (a/go-loop []
    (let [rs (a/<! (f))]
      (cond
        (= rs :ok)
        (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))

        (= rs :err)
        (let [_ (a/<! (a/timeout 5000))

              game-account-state
              (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))]
          ;; Only increased serial means succeed
          (cond
            (nil? game-account-state)
            (do (log/info "😱Failed to fetch game-account-state, retry")
                (recur))

            (>= settle-serial (:settle-serial game-account-state))
            (do (log/infof "😡Retry Settle transaction, current serial: %s"
                           (:settle-serial game-account-state))
                (recur))

            :else
            (do (log/infof "😃The settle-serial updated. serial: %s -> %s"
                           settle-serial
                           (:settle-serial game-account-state))
                game-account-state)))))))

(extend-type SolanaApi
 p/IChainApi

 ;; Send Settle transaction to Solana
 (p/-settle
   [this game-id game-account-state settle-serial settle-map]
   (run-with-retry-loop this
                        game-id
                        settle-serial
                        (fn [] (settle game-id game-account-state settle-map))))

 ;; Send SetWinner transaction to Solana
 (p/-set-winner
   [this game-id game-account-state settle-serial ranking]
   (run-with-retry-loop this
                        game-id
                        settle-serial
                        (fn [] (set-winner game-id game-account-state ranking))))

 (p/-fetch-game-account
   [_this game-id {:keys [commitment settle-serial], :or {commitment "finalized"}}]
   (a/go-loop []
     (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
           game-account-pubkey (pubkey/make-public-key game-id)
           game-account-state (some->
                               (a/<!
                                (conn/get-account-info conn game-account-pubkey commitment))
                               :data
                               (parse-state-data))]
       (if (and settle-serial (not= settle-serial (:settle-serial game-account-state)))
         (recur)
         game-account-state))))

 (p/-fetch-mint-info
   [_this mint-address]
   (go-try
    (let [conn        (conn/make-connection (get @config :solana-rpc-endpoint))
          mint-pubkey (pubkey/make-public-key mint-address)
          mint-state  (<!? (spl-token/get-mint conn mint-pubkey "finalized"))]
      mint-state)))

 ep/IAttachable
 (ep/-input
   [this]
   (:input this))

 (ep/-output
   [this]
   (:output this))

 (ep/-interest-event-types
   [_this]
   [:system/settle
    :system/set-winner])

 ep/IComponent
 (ep/-start [this opts]
   (let [{:keys [input output]}       this
         {:keys [game-id init-state]} opts]
     (sync-loop/start-sync-loop
      this
      game-id
      input
      output
      (:game-account-state init-state)))
   nil))
