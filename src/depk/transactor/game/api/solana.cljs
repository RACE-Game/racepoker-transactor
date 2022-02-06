(ns depk.transactor.game.api.solana
  (:require
   [depk.transactor.util :refer [go-try <!?]]
   [cljs.core.async :refer [<! timeout]]
   [depk.transactor.game.api.protocols :as p]
   [solana-clj.connection :as conn]
   [solana-clj.publickey :as pubkey]
   [solana-clj.keypair :as keypair]
   [solana-clj.extra.buffer-layout :as bl]
   [solana-clj.extra.instruction-builder :as ib]
   [solana-clj.transaction :as transaction]
   [depk.transactor.constant :as c]
   [depk.transactor.state.config :refer [config]]
   [taoensso.timbre :as log]
   ["fs" :as fs]
   ["bn.js" :as bn]))

(def settle-type-no-update 0)
(def settle-type-chip-add 1)
(def settle-type-chip-sub 2)

(def settle-status-normal 0)
(def settle-status-leave 1)

(defn load-private-key
  []
  (if-let [key (some-> (:transactor-solana-keypair @config)
                       (fs/readFileSync)
                       (js/JSON.parse)
                       (js/Uint8Array.from)
                       (keypair/from-secret-key))]
    key
    (throw (ex-info "Can't load fee payer keypair" {}))))

(defrecord Player [pubkey chips])

(def player-layout (bl/struct ->Player [:pubkey :u64]))

(defrecord GameState [is-initialized game-no players stack-account-pubkey mint-pubkey level
                      mint-decimals])

(def game-state-layout
  (bl/struct ->GameState
             [:bool                                  ; is_initialized
              :u32                                   ; game_no
              (bl/array 6 (bl/option player-layout)) ; players
              :pubkey                                ; stack_account_pubkey
              :pubkey                                ; mint_pubkey
              (bl/enum :NL100 :NL200 :NL500 :NL1000) ; game_level
              :u8                                    ; mint_decimals
             ]))

(bl/size game-state-layout)

(defn parse-state-data
  [data]
  (bl/unpack game-state-layout (bl/buffer-from data)))

(defrecord SolanaApi [])

(extend-type SolanaApi
 p/IChainApi
 (-settle-finished-game [this game-id chips-change-map player-status-map]
   (when-not (transduce (map val) + 0 chips-change-map)
     (throw (ex-info "Invalid chips change!" {:chips-change-map chips-change-map})))

   (log/infof "settle game result on Solana: game[%s]" game-id)
   (log/infof "chips-change-map: %s" (prn-str chips-change-map))
   (log/infof "player-status-map: %s" (prn-str player-status-map))
   (go-try
    (let [fee-payer (load-private-key)

          conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some-> (<!? (conn/get-account-info conn game-account-pubkey))
                                     :data
                                     (parse-state-data))

          _ (when-not (some? game-account-state)
              (log/errorf "game account not found: game[%s]" game-id))

          dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

          _ (log/infof "dealer program: %s" (get @config :dealer-program-address))

          {:keys [players]} game-account-state
          player-ids (->> players
                          (map (comp str :pubkey)))
          ix-body (->> (for [pid player-ids]
                         (let [chip-change (get chips-change-map pid 0)
                               status      (get player-status-map pid :normal)]
                           [[(case status
                               :normal settle-status-normal
                               :leave  settle-status-leave)
                             1]
                            [(cond
                               (zero? chip-change) settle-type-no-update
                               (pos? chip-change)  settle-type-chip-add
                               :else               settle-type-chip-sub)
                             1]
                            [(js/Math.abs chip-change) 8]]))
                       (mapcat identity))

          _ (log/infof "settle instruction body: %s" (prn-str ix-body))

          ix-data (apply ib/make-instruction-data (cons c/instruction-header-settle ix-body))

          ix
          (transaction/make-transaction-instruction
           {:keys      [{:pubkey     (keypair/public-key fee-payer),
                         :isSigner   true,
                         :isWritable false}
                        {:pubkey     game-account-pubkey,
                         :isSigner   false,
                         :isWritable true}],
            :programId (pubkey/make-public-key dealer-program-id),
            :data      ix-data})
          tx
          (doto (transaction/make-transaction)
           (transaction/add ix))]

      (log/infof "sending settle transaction: game[%s]" game-id)

      (let [sig (<! (conn/send-transaction conn tx [fee-payer]))]

        (log/infof "confirming settle transaction: game[%s]" game-id)

        (<! (conn/confirm-transaction conn sig))

        (log/infof "settle succeed: game[%s]" game-id)))))

 (-settle-failed-game [this game-id player-state-map])

 (-fetch-game-account [this game-id]
   (go-try
    ;; (log/infof "fetch game account state for game[%s]" game-id)
    (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some-> (<!?
                                      (conn/get-account-info conn game-account-pubkey "confirmed"))
                                     :data
                                     (parse-state-data))]
      ;; (log/infof "game state for [%s]: %s" game-id (prn-str game-account-state))
      game-account-state))))


(js/Uint8Array.of)

(comment
  (.log js/console
        (into-array
         (ib/make-instruction-data
          [1 8]
          [0 8]
          [1 8]
          [0 8]
          [1 8]
          [0 8]
          [1 1]
          [0 1]
          [1 1]
          [0 1]
          [1 1]
          [0 1])))

  (p/-fetch-game-account
   (->SolanaApi)
   "GJecU1PjuFJo5mjXEphmjPmE58UvBorhvy7jczVQS6Lr")

  (p/-settle-finished-game
   (->SolanaApi)
   "96jrkBBa3iKGeG5KYrXTAWwu7RkNRYVYnXVvU2URVbc7"
   {"ENR11vPNw2xPXkCJk1Woheui1Yrd68NGWDBmgwSdzcwP" 10,
    "DfgtACV9VzRUKUqRjuzxRHmeycyomcCzfRHgVyDPha9F" -10}
   nil))
