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
   [depk.transactor.chain.state :refer [parse-state-data]]
   [depk.transactor.util :as u]
   [depk.transactor.chain.sync-loop :as sync-loop]
   [depk.transactor.chain.transact-loop :as transact-loop]
   ["fs" :as fs]
   ["bn.js" :as bn]))

;;; Helpers

(def settle-type-no-update 0)
(def settle-type-chip-add 1)
(def settle-type-chip-sub 2)

(def settle-status-normal 0)
(def settle-status-leave 1)
(def settle-status-empty 2)

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
  [player-ids chips-change-map player-status-map]
  (->> player-ids
       (keep (fn [pid]
               (if pid
                 (let [chip-change (get chips-change-map pid (js/BigInt 0))
                       status      (get player-status-map pid)]
                   [[(case status
                       :dropout settle-status-leave
                       :normal  settle-status-normal
                       :leave   settle-status-leave)
                     1]
                    [(cond
                       (zero? chip-change) settle-type-no-update
                       (pos? chip-change)  settle-type-chip-add
                       :else               settle-type-chip-sub)
                     1]
                    [(u/abs chip-change) 8]])
                 ;; empty entries
                 [[settle-status-empty 1] [settle-type-no-update 1] [0 8]])))
       (mapcat identity)))

(defn- find-player-ata-keys
  [player-ids mint-pubkey player-status-map]
  (go-try
   (let [ata-keys-ch (->>
                      player-ids
                      (keep (fn [id]
                              (when (and id (#{:leave :dropout} (get player-status-map id)))
                                (a/go
                                 (let [account-pubkey (pubkey/make-public-key id)
                                       ata-pubkey     (<!?
                                                       (spl-token/get-associated-token-address
                                                        mint-pubkey
                                                        account-pubkey))]
                                   {:pubkey     ata-pubkey,
                                    :isSigner   false,
                                    :isWritable true})))))
                      (a/map vector))]
     (<!? ata-keys-ch))))

(defn settle
  [game-id chips-change-map player-status-map]
  (when-not (transduce (map val) + (js/BigInt 0) chips-change-map)
    (throw (ex-info "Can't settel, invalid chips change!" {:chips-change-map chips-change-map})))

  (log/infof "Settle game result on Solana: game[%s]" game-id)
  (log/infof "Chips-change-map: %s" (prn-str chips-change-map))
  (log/infof "Player-status-map: %s" (prn-str player-status-map))
  (go-try
   (let [fee-payer (load-private-key)

         conn (conn/make-connection (get @config :solana-rpc-endpoint))
         game-account-pubkey (pubkey/make-public-key game-id)
         game-account-state (some-> (<!?
                                     (conn/get-account-info conn game-account-pubkey "finalized"))
                                    :data
                                    (parse-state-data))

         _ (when-not (some? game-account-state)
             (log/errorf "game account not found: game[%s]" game-id))

         dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

         {:keys [players stack-account-pubkey mint-pubkey]} game-account-state

         player-ids (for [p players]
                      (when p
                        (str (:pubkey p))))

         ix-body (build-settle-ix-body player-ids chips-change-map player-status-map)

         ix-data (apply ib/make-instruction-data (cons c/instruction-header-settle ix-body))

         [pda] (<!? (pubkey/find-program-address #js [(buffer-from "stack")] dealer-program-id))

         ata-keys (<!? (find-player-ata-keys player-ids mint-pubkey player-status-map))

         ix-keys
         (into
          [{:pubkey     (keypair/public-key fee-payer),
            :isSigner   true,
            :isWritable false}
           {:pubkey     game-account-pubkey,
            :isSigner   false,
            :isWritable true}
           {:pubkey     stack-account-pubkey,
            :isSigner   false,
            :isWritable true}
           {:pubkey     pda,
            :isSigner   false,
            :isWritable false}
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

         ret (<!? (conn/confirm-transaction conn sig "finalized"))]
     (if (nil? (get-in ret [:value :err]))
       (log/infof "Transaction succeed")
       (log/errorf "Transaction failed")))))

;;; Implementations

(defrecord SolanaApi [input output pending confirming])

(defn make-solana-api
  []
  (log/debug "Use Solana chain api.")
  (let [input      (a/chan 30)
        output     (a/chan 30)
        pending    (atom [])
        confirming (atom [])]
    (->SolanaApi input output pending confirming)))

(extend-type SolanaApi
 p/IChainApi

 (p/-settle-finished-game
   [this game-id chips-change-map player-status-map]
   (settle game-id chips-change-map player-status-map))

 (p/-settle-failed-game
   [this game-id player-status-map]
   (settle game-id {} player-status-map))

 (p/-fetch-game-account
   [this game-id]
   (go-try
    (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some-> (<!?
                                      (conn/get-account-info conn game-account-pubkey "finalized"))
                                     :data
                                     (parse-state-data))]
      game-account-state)))

 (p/-fetch-mint-info
   [this mint-address]
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

 ep/IComponent
 (ep/-start [this opts]
   (let [{:keys [input output]} this
         {:keys [game-id]}      opts]
     (sync-loop/start-sync-loop this game-id output)
     (transact-loop/start-transact-loop this game-id input output))
   nil))
