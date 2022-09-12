(ns depk.transactor.chain.solana
  (:require
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.util :refer [go-try <!?]]
   [cljs.core.async :as a]
   [solana-clj.connection :as conn]
   [solana-clj.publickey :as pubkey]
   [solana-clj.keypair :as keypair]
   [solana-clj.spl-token :as spl-token]
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]
   [solana-clj.extra.instruction-builder :as ib]
   [solana-clj.transaction :as transaction]
   [depk.transactor.constant :as c]
   [depk.transactor.state.config :refer [config]]
   [depk.transactor.log :as log]
   [depk.transactor.chain.state :refer [parse-state-data set-winner-ix-id settle-ix-id
                                        start-tournament-ix-id
                                        settle-tournament-ix-id
                                        parse-bonus-state-data
                                        parse-tournament-state-data
                                        registration-layout]]
   ["fs" :as fs]
   ["bn.js" :as bn]))

;;; Helpers

(def default-commitment "finalized")

(def settle-status-no-update 0)
(def settle-status-leave 1)

(def settle-type-chips-add 0)
(def settle-type-chips-sub 1)
(def settle-type-no-update 2)


(defn handle-result
  [id sig ret err serial]
  (cond
    (and sig ret (nil? err))
    (do (log/log "🪁" id "Transaction succeed, serial: %s, signature: %s" serial sig)
        :ok)

    :else
    (do (log/log "🚨" id "Transaction failed, serial: %s, error: %s" serial err)
        :err)))

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

(defn- get-receiver-pubkey
  [account-pubkey mint-pubkey]
  (go-try
   (if (= spl-token/native-mint mint-pubkey)
     account-pubkey

     (<!?
      (spl-token/get-associated-token-address
       mint-pubkey
       account-pubkey)))))

(defn- find-player-ata-keys-by-settle-map
  [players mint-pubkey bonus-mint-pubkey settle-map]
  (go-try
   (let [ata-keys-ch
         (->>
          players
          (mapcat
           (fn [{:keys [pubkey]}]
             (when pubkey
               (cond-> []

                 ;; When a player left, add his asset ata
                 (#{:leave} (get-in settle-map [(str pubkey) :settle-status]))
                 (conj (a/go
                        (let [account-pubkey (pubkey/make-public-key pubkey)
                              ata-pubkey     (<!? (get-receiver-pubkey account-pubkey mint-pubkey))]
                          {:pubkey     ata-pubkey,
                           :isSigner   false,
                           :isWritable true})))


                 ;; When a player pay rake, add his bonus ata
                 (and bonus-mint-pubkey
                      (> (get-in settle-map [(str pubkey) :rake] (js/BigInt 0))
                         (js/BigInt 0)))
                 (conj (a/go
                        (let [account-pubkey (pubkey/make-public-key pubkey)
                              ata-pubkey     (<!? (get-receiver-pubkey account-pubkey
                                                                       bonus-mint-pubkey))]
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
                                  ata-pubkey     (<!? (get-receiver-pubkey account-pubkey
                                                                           bonus-mint-pubkey))]
                              {:pubkey     ata-pubkey,
                               :isSigner   false,
                               :isWritable true})))))
                      (a/map vector))]
     (<!? ata-keys-ch))))

;;; FIXME: what if players GC his ATA

(defn settle
  [game-id game-account-state settle-map]

  (log/log "🚀" game-id "Settle game result on Solana")

  (a/go
   (try
     (let [fee-payer (load-private-key)

           conn (conn/make-connection (get @config :solana-rpc-endpoint))
           game-account-pubkey (pubkey/make-public-key game-id)

           _ (log/log "🚀" game-id "Settes: %s" (prn-str settle-map))
           _ (log/log "🚀" game-id "Current serial: %s" (:settle-serial game-account-state))

           _
           (doseq
             [i    (range c/max-player-num)
              :let [{:keys [pubkey]} (nth (:players game-account-state) i)
                    {:keys [settle-type settle-status amount rake]} (get settle-map (str pubkey))]]

             (log/log "🚀"
                      game-id
                      "-%s %s %s %s %s"
                      (str pubkey)
                      settle-type
                      settle-status
                      amount
                      rake))

           dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

           {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey
                   settle-serial bonus-pubkey]}
           game-account-state

           bonus-state (when bonus-pubkey
                         (some-> (<!? (conn/get-account-info conn bonus-pubkey "finalized"))
                                 :data
                                 (parse-bonus-state-data)))

           transactor-ata-pubkey (<!? (get-receiver-pubkey transactor-pubkey mint-pubkey))

           owner-ata-pubkey (<!? (get-receiver-pubkey owner-pubkey mint-pubkey))

           ix-body (build-settle-ix-body players settle-map)

           ix-data (apply ib/make-instruction-data
                          (concat [[settle-ix-id 1]
                                   ;; settle-serial
                                   [(:settle-serial game-account-state) 4]]
                                  ix-body))

           [pda] (<!? (pubkey/find-program-address #js [(pubkey/to-buffer game-account-pubkey)]
                                                   dealer-program-id))

           ata-keys
           (<!? (find-player-ata-keys-by-settle-map players
                                                    mint-pubkey
                                                    (:mint-pubkey bonus-state)
                                                    settle-map))

           _ (log/log "🚀" game-id "Fee Payer: %s" (keypair/public-key fee-payer))
           _ (log/log "🚀" game-id "Game Account: %s" game-account-pubkey)
           _ (log/log "🚀" game-id "Stake Account: %s" stake-account-pubkey)
           _ (log/log "🚀" game-id "Stake PDA: %s" pda)
           _ (log/log "🚀" game-id "Transactor Rake Taker: %s" transactor-ata-pubkey)
           _ (log/log "🚀" game-id "Owner Rake Taker: %s" owner-ata-pubkey)
           _ (log/log "🚀" game-id "Token Program: %s" spl-token/token-program-id)

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
               :isWritable true}
              {:pubkey     transactor-ata-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     owner-ata-pubkey,
               :isSigner   false,
               :isWritable true}
              {:pubkey     spl-token/token-program-id,
               :isSigner   false,
               :isWritable false}
              {:pubkey     pubkey/system-program,
               :isSigner   false,
               :isWritable false}]

             bonus-pubkey
             (into [{:pubkey     bonus-pubkey,
                     :isSigner   false,
                     :isWritable false}
                    (let [[bonus-pda] (<!? (pubkey/find-program-address #js
                                                                         [(pubkey/to-buffer
                                                                           bonus-pubkey)]
                                                                        dealer-program-id))]
                      {:pubkey     bonus-pda,
                       :isSigner   false,
                       :isWritable false})
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
       (handle-result game-id sig ret err settle-serial))
     (catch js/Error e
       (.error js/console e)
       :err))))

(defn set-winner
  [game-id game-account-state ranking]
  (go-try
   (let [_ (log/log "🚀" game-id "SNG finished, ranking: %s" ranking)
         fee-payer (load-private-key)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))
         game-account-pubkey (pubkey/make-public-key game-id)

         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         {:keys [players stake-account-pubkey mint-pubkey transactor-pubkey owner-pubkey
                 settle-serial bonus-pubkey]}
         game-account-state

         _ (log/log "🚀" game-id "On chain players: %s" players)

         player-id-to-pos (->> (map (comp str :pubkey) players)
                               (map-indexed (fn [idx pk] [pk idx]))
                               (into {}))

         ranking-pos-list (map player-id-to-pos ranking)

         _ (log/log "🚀" game-id "Ranking: %s" ranking-pos-list)

         [pda] (<!? (pubkey/find-program-address #js [(pubkey/to-buffer game-account-pubkey)]
                                                 dealer-program-id))

         winner-pubkey (pubkey/make-public-key (first ranking))

         ata-pubkey (<!? (get-receiver-pubkey winner-pubkey mint-pubkey))

         transactor-ata-pubkey (<!? (get-receiver-pubkey transactor-pubkey mint-pubkey))

         owner-ata-pubkey (<!? (get-receiver-pubkey owner-pubkey mint-pubkey))

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
             :isWritable true}
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
             :isWritable false}
            {:pubkey     pubkey/system-program,
             :isSigner   false,
             :isWritable false}]

           bonus-state
           (into [{:pubkey     bonus-pubkey,
                   :isSigner   false,
                   :isWritable false}
                  (let [[bonus-pda] (<!? (pubkey/find-program-address #js
                                                                       [(pubkey/to-buffer
                                                                         bonus-pubkey)]
                                                                      dealer-program-id))]
                    {:pubkey     bonus-pda,
                     :isSigner   false,
                     :isWritable false})
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
     (handle-result game-id sig ret err settle-serial))))


(defn start-tournament
  [tournament-id tournament-account-state]
  (go-try
   (let [_ (log/log "🚀" tournament-id "Start tournament")

         fee-payer (load-private-key)

         fee-payer-pubkey (keypair/public-key fee-payer)

         settle-serial (:settle-serial tournament-account-state)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))

         tournament-account-pubkey (pubkey/make-public-key tournament-id)

         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         ix-data (ib/make-instruction-data
                  [start-tournament-ix-id 1])

         ix (transaction/make-transaction-instruction
             {:programId dealer-program-id,
              :keys      [{:pubkey     fee-payer-pubkey,
                           :isSigner   true,
                           :isWritable false}
                          {:pubkey     tournament-account-pubkey,
                           :isSigner   false,
                           :isWritable true}],
              :data      ix-data})

         tx (doto (transaction/make-transaction)
             (transaction/add ix))

         sig (<!? (conn/send-transaction conn tx [fee-payer]))

         ret
         (when sig (<!? (conn/confirm-transaction conn sig "finalized")))

         err
         (when ret (get-in ret [:value :err]))]
     (handle-result tournament-id sig ret err settle-serial))))

(defn settle-tournament
  "Settle the result of tournament."
  [tournament-id tournament-account-state ranks]
  (go-try
   (let [_ (log/log "🚀" tournament-id "Settle tournament")
         _ (doseq [r ranks]
             (log/log "🚀" tournament-id "-%s" r))

         settle-size 100

         fee-payer (load-private-key)
         fee-payer-pubkey (keypair/public-key fee-payer)
         settle-serial (:settle-serial tournament-account-state)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))

         registration-pubkey (:registration-pubkey tournament-account-state)
         tournament-account-pubkey (pubkey/make-public-key tournament-id)
         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         pubkey-to-pos (->> (:ranks tournament-account-state)
                            (map-indexed #(vector (str (:pubkey %2)) (inc %1)))
                            (into {}))

         ranks-data (->> (concat (mapv #(get pubkey-to-pos % 0) ranks)
                                 (repeat 0))
                         (take settle-size)
                         (mapv #(vector % 2)))

         ix-data (apply
                  ib/make-instruction-data
                  [settle-tournament-ix-id 1]
                  [settle-serial 4]
                  ranks-data)

         ix-keys [{:pubkey     fee-payer-pubkey,
                   :isSigner   true,
                   :isWritable false}
                  {:pubkey     tournament-account-pubkey,
                   :isSigner   false,
                   :isWritable true}
                  {:pubkey     registration-pubkey,
                   :isSigner   false,
                   :isWritable true}]

         ix (transaction/make-transaction-instruction
             {:programId dealer-program-id,
              :keys      ix-keys,
              :data      ix-data})

         tx (doto (transaction/make-transaction)
             (transaction/add ix))

         sig (<!? (conn/send-transaction conn tx [fee-payer]))

         _ (log/log "🚀" tournament-id "Sig: %s" sig)

         ret
         (when sig (<!? (conn/confirm-transaction conn sig "finalized")))

         err
         (when ret (get-in ret [:value :err]))]

     (handle-result tournament-id sig ret err settle-serial))))

;;; Implementations

(defrecord SolanaApi [input output pending confirming])

(defn make-solana-api
  []
  (log/log "🎉" nil "Use Solana chain")
  (let [input      (a/chan 30)
        output     (a/chan 30)
        pending    (atom [])
        confirming (atom [])]
    (->SolanaApi input output pending confirming)))

(defn run-with-retry-loop
  [id settle-serial fetch-fn submit-fn]
  (a/go-loop []
    (let [rs (a/<! (submit-fn))]
      (cond
        (= rs :ok)
        (do (log/log "😃" id
                     "Transaction succeed without tries. new settle-serial: %s"
                     (inc settle-serial))
            (inc settle-serial))

        (= rs :err)
        (let [_ (a/<! (a/timeout 5000))

              state
              (a/<! (fetch-fn))]
          ;; Only increased serial means succeed
          (cond
            (nil? state)
            (do (log/log "😱"
                         id
                         "Fetch error, retry")
                (recur))

            (>= settle-serial (:settle-serial state))
            (do (log/log "😡"
                         id
                         "Retry Settle transaction, current serial: %s"
                         (:settle-serial state))
                (recur))

            :else
            (do (log/log "😃"
                         id
                         "The settle-serial updated. serial: %s -> %s"
                         settle-serial
                         (:settle-serial state))
                (:settle-serial state))))))))

(extend-type SolanaApi
 p/IChainApi

 ;; Send Settle transaction to Solana
 (p/-settle
   [this game-id game-account-state settle-serial settle-map]
   (run-with-retry-loop
    game-id
    settle-serial
    (fn [] (p/-fetch-game-account this game-id {:commitment "finalized"}))
    (fn [] (settle game-id game-account-state settle-map))))

 ;; Send SetWinner transaction to Solana
 (p/-set-winner
   [this game-id game-account-state settle-serial ranking]
   (run-with-retry-loop
    game-id
    settle-serial
    (fn [] (p/-fetch-game-account this game-id {:commitment "finalized"}))
    (fn [] (set-winner game-id game-account-state ranking))))

 (p/-start-tournament
   [this tournament-id tournament-account-state settle-serial]
   (run-with-retry-loop
    tournament-id
    settle-serial
    (fn []
      (p/-fetch-tournament-account this
                                   tournament-id
                                   {:commitment     "finalized",
                                    :without-ranks? true}))
    (fn []
      (start-tournament tournament-id tournament-account-state))))

 (p/-fetch-game-account
   [_this game-id {:keys [commitment settle-serial], :or {commitment "finalized"}}]
   (a/go-loop []
     (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
           game-account-pubkey (pubkey/make-public-key game-id)
           game-account-state (try (some->
                                    (a/<!
                                     (conn/get-account-info conn game-account-pubkey commitment))
                                    :data
                                    (parse-state-data))
                                   (catch js/Error _ nil))]
       (if (and settle-serial (not= settle-serial (:settle-serial game-account-state)))
         (do
           (log/log "🫠"
                    game-id
                    "Retry fetch game account, settle-serial mismatch, %s != %s"
                    settle-serial
                    (:settle-serial game-account-state))
           (recur))
         game-account-state))))

 (p/-fetch-mint-info
   [_this mint-address]
   (go-try
    (let [conn        (conn/make-connection (get @config :solana-rpc-endpoint))
          mint-pubkey (pubkey/make-public-key mint-address)
          mint-state  (<!? (spl-token/get-mint conn mint-pubkey "finalized"))]
      mint-state)))

 (p/-settle-tournament
   [this tournament-id tournament-account-state settle-serial ranks]
   (run-with-retry-loop
    tournament-id
    settle-serial
    (fn []
      (p/-fetch-tournament-account this
                                   tournament-id
                                   {:commitment     "finalized",
                                    :without-ranks? false}))
    (fn []
      (settle-tournament tournament-id tournament-account-state ranks))))

 (p/-fetch-tournament-account
   [_this tournament-id
    {:keys [commitment buyin-serial without-ranks?],
     :or   {commitment     "finalized",
            without-ranks? false}}]
   (a/go-loop []
     (let [conn    (conn/make-connection (get @config :solana-rpc-endpoint))
           address (pubkey/make-public-key tournament-id)
           tournament-state (try (some->
                                  (a/<! (conn/get-account-info conn address commitment))
                                  :data
                                  (parse-tournament-state-data))
                                 (catch js/Error _ nil))]
       (when tournament-state
         (if (and buyin-serial (not= buyin-serial (:buyin-serial tournament-state)))
           (do (a/<! (a/timeout 1000))
               (log/log "🫠"
                        tournament-id
                        "Retry fetch tournament, serial mismatch, %s != %s"
                        buyin-serial
                        (:buyin-serial tournament-state))
               (recur))
           (if without-ranks?
             tournament-state
             (let [{:keys [max-players registration-pubkey num-players]} tournament-state
                   reg-layout (bl/array max-players (bl/option registration-layout))
                   rank-state (loop []
                                (let [ranks (try (some->>
                                                  (a/<! (conn/get-account-info conn
                                                                               registration-pubkey
                                                                               commitment))
                                                  :data
                                                  (buffer-from)
                                                  (bl/unpack reg-layout)
                                                  (filter some?))
                                                 (catch js/Error e (println e)))]
                                  (if (and ranks (>= (count ranks) num-players))
                                    ranks
                                    (do
                                      (a/<! (a/timeout 1000))
                                      (log/log
                                       "🫠"
                                       tournament-id
                                       "Retry fetch tournament ranks, ranks data mismatch.")
                                      (recur)))))]
               (assoc tournament-state :ranks rank-state)))))))))
