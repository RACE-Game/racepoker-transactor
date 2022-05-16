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
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.chain.state :refer [parse-state-data set-winner-ix-id settle-ix-id]]
   [depk.transactor.util :as u]
   [depk.transactor.chain.sync-loop :as sync-loop]
   ["fs" :as fs]
   ["bn.js" :as bn]))

;;; Helpers

(def default-commitment "confirmed")

(def settle-status-empty-seat 0)
(def settle-status-leave 1)
(def settle-status-no-update 2)

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
  (->> players
       (mapcat (fn [{:keys [pubkey]}]
                 (let [{:keys [settle-type settle-status amount]} (get settle-map (str pubkey))]
                   [[(case settle-status
                       :empty-seat settle-status-empty-seat
                       :leave      settle-status-leave
                       :no-update  settle-status-no-update
                       settle-status-empty-seat)
                     1]
                    [(case settle-type
                       :chips-add settle-type-chips-add
                       :chips-sub settle-type-chips-sub
                       :no-update settle-type-no-update
                       settle-type-no-update)
                     1]
                    [(or amount (js/BigInt 0)) 8]])))
       (doall)))

(defn- find-player-ata-keys
  [players mint-pubkey settle-map]
  (go-try
   (let [ata-keys-ch (->>
                      players
                      (keep
                       (fn [{:keys [pubkey]}]
                         (when (and pubkey
                                    (#{:leave} (get-in settle-map [(str pubkey) :settle-status])))
                           (a/go
                            (let [account-pubkey (pubkey/make-public-key pubkey)
                                  ata-pubkey     (<!?
                                                  (spl-token/get-associated-token-address
                                                   mint-pubkey
                                                   account-pubkey))]
                              {:pubkey     ata-pubkey,
                               :isSigner   false,
                               :isWritable true})))))
                      (a/map vector))]
     (<!? ata-keys-ch))))

;;; FIXME: what if players GC his ATA

(defn settle
  [game-id rake settle-map]

  (log/infof "🚀Settle game result on Solana: game[%s]" game-id)

  (a/go
   (try
     (let [fee-payer (load-private-key)

           conn (conn/make-connection (get @config :solana-rpc-endpoint))
           game-account-pubkey (pubkey/make-public-key game-id)
           game-account-state (some-> (<!?
                                       (conn/get-account-info conn
                                                              game-account-pubkey
                                                              default-commitment))
                                      :data
                                      (parse-state-data))

           _ (when-not (some? game-account-state)
               (log/errorf "🚨game account not found: game[%s]" game-id))

           _ (log/infof "📝Settes: %s" settle-map)
           _ (doseq
               [i    (range c/max-player-num)
                :let [{:keys [pubkey]} (nth (:players game-account-state) i)
                      {:keys [settle-type settle-status amount]} (get settle-map (str pubkey))]]

               (log/info "📝-" (str pubkey) settle-type settle-status amount))
           _ (log/infof "📝Rake: %s" rake)

           dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

           {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey]}
           game-account-state

           transactor-ata-pubkey (<!?
                                  (spl-token/get-associated-token-address
                                   mint-pubkey
                                   transactor-pubkey))

           owner-ata-pubkey (<!?
                             (spl-token/get-associated-token-address
                              mint-pubkey
                              owner-pubkey))

           ix-body (build-settle-ix-body players settle-map)

           ;; _ (println "ix-body" ix-body)

           ix-data (apply ib/make-instruction-data
                          (concat [[settle-ix-id 1]
                                   ;; settle-serial
                                   [(:settle-serial game-account-state) 4]
                                   ;; rake
                                   [rake 8]]
                                  ix-body))

           [pda] (<!? (pubkey/find-program-address #js [(buffer-from "stake")]
                                                   dealer-program-id))

           ata-keys (<!? (find-player-ata-keys players mint-pubkey settle-map))

           _ (log/infof "Fee Payer: %s" (keypair/public-key fee-payer))
           _ (log/infof "Game Account: %s" game-account-pubkey)
           _ (log/infof "Stake Account: %s" stake-account-pubkey)
           _ (log/infof "Stake PDA: %s" pda)
           _ (log/infof "Transactor Rake Taker: %s" transactor-ata-pubkey)
           _ (log/infof "Owner Rake Taker: %s" owner-ata-pubkey)
           _ (log/infof "Token Program: %s" spl-token/token-program-id)

           ix-keys
           (into
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
            ata-keys)

           ix
           (transaction/make-transaction-instruction
            {:keys      ix-keys,
             :programId (pubkey/make-public-key dealer-program-id),
             :data      ix-data})
           tx
           (doto (transaction/make-transaction)
            (transaction/add ix))

           sig (<!? (conn/send-transaction conn tx [fee-payer]))

           ret (<!? (conn/confirm-transaction conn sig "finalized"))

           no-err? (and ret (nil? (get-in ret [:value :err])))]

       (cond
         no-err?
         (do (log/info "🎉Transaction succeed")
             :ok)

         :else
         (do (log/errorf "🚨Transaction failed")
             :err)))
     (catch js/Error e
       (.error js/console e)
       :err))))

(defn set-winner
  [game-id winner-id]
  (go-try
   (let [_ (log/infof "📝SNG winner: %s" winner-id)
         fee-payer (load-private-key)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))
         game-account-pubkey (pubkey/make-public-key game-id)
         game-account-state (some-> (<!?
                                     (conn/get-account-info conn game-account-pubkey "finalized"))
                                    :data
                                    (parse-state-data))

         _ (when-not (some? game-account-state)
             (log/errorf "🚨game account not found: game[%s]" game-id))

         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey]}
         game-account-state

         _ (log/infof "📝On chain players: %s" players)

         ix-data (ib/make-instruction-data [set-winner-ix-id 1])

         [pda] (<!? (pubkey/find-program-address #js [(buffer-from "stake")]
                                                 dealer-program-id))

         winner-pubkey (pubkey/make-public-key winner-id)

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

         ix-keys [{:pubkey     (keypair/public-key fee-payer),
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
                  {:pubkey     winner-pubkey,
                   :isSigner   false,
                   :isWritable false}
                  {:pubkey     ata-pubkey,
                   :isSigner   false,
                   :isWritable true}
                  {:pubkey     transactor-ata-pubkey,
                   :isSigner   false,
                   :isWritable true}
                  {:pubkey     owner-ata-pubkey,
                   :isSigner   false,
                   :isWritable true}
                  {:pubkey     spl-token/token-program-id,
                   :isSigner   false,
                   :isWritable false}]

         ix
         (transaction/make-transaction-instruction
          {:keys      ix-keys,
           :programId (pubkey/make-public-key dealer-program-id),
           :data      ix-data})
         tx
         (doto (transaction/make-transaction)
          (transaction/add ix))

         sig (<!? (conn/send-transaction conn tx [fee-payer]))

         ret (<!? (conn/confirm-transaction conn sig "finalized"))

         no-err? (and ret (nil? (get-in ret [:value :err])))]
     (cond
       no-err?
       (do (log/info "🎉Transaction succeed") :ok)

       :else
       (do (log/error "🚨Transaction failed") :err)))))

(defn force-sync
  [game-id output]
  (a/go
   (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
         game-account-pubkey (pubkey/make-public-key game-id)
         game-account-state (some-> (a/<!
                                     (conn/get-account-info conn game-account-pubkey "finalized"))
                                    :data
                                    (parse-state-data))]
     (a/>! output
           {:type :system/recover-state,
            :data {:game-account-state game-account-state}}))))

(comment
  (set-winner "6R1A1mddhrJw5CKQfXCsq2gkPvvotr3xoCHCV2tpnF4r"
              "F6JoJgWrVZEUaRVpA2uQyRQDNdZXyhyiD8KqdfXcjXQN"))

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

(extend-type SolanaApi
 p/IChainApi

 ;; Send Settle transaction to Solana
 ;; Keep retrying until it's finalized
 (p/-settle
   [this game-id settle-serial rake settle-map]
   (a/go-loop []
     (let [rs (a/<! (settle game-id rake settle-map))]
       (cond
         (= rs :ok)
         :ok

         (= rs :err)
         (let [game-account-state (a/<! (p/-fetch-game-account this
                                                               game-id
                                                               {:commitment "finalized"}))]
           (log/infof "💪Retry Settle transaction")
           (a/<! (a/timeout 5000))
           ;; Unchanged serial means failed transaction
           (when (= settle-serial (:settle-serial game-account-state))
             (recur)))))))

 ;; Send SetWinner transaction to Solana
 ;; Keep retrying until it's finalized
 (p/-set-winner
   [this game-id settle-serial winner-id]
   (a/go-loop []
     (let [rs (a/<! (set-winner game-id winner-id))]
       (cond
         (= rs :ok)
         :ok

         (= rs :err)
         (let [game-account-state (a/<! (p/-fetch-game-account this
                                                               game-id
                                                               {:commitment "finalized"}))]
           ;; Unchanged serial means failed transaction
           (log/infof "☢️Retry SetWinner transaction")
           (a/<! (a/timeout 5000))
           (when (= settle-serial (:settle-serial game-account-state))
             (recur)))))))

 (p/-fetch-game-account
   [_this game-id {:keys [commitment], :or {commitment "finalized"}}]
   (go-try
    (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some->
                              (<!?
                               (conn/get-account-info conn game-account-pubkey commitment))
                              :data
                              (parse-state-data))]
      game-account-state)))

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
