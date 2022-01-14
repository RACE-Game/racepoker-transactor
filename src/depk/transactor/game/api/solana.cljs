(ns depk.transactor.game.api.solana
  (:require
   [depk.transactor.util :refer [go-try <!?]]
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
   ["fs" :as fs]))

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

(def player-layout (bl/struct ->Player [:pubkey :u32]))

(defrecord GameState [is-initialized game-no players stack-account-pubkey mint-pubkey level])

(def game-state-layout
  (bl/struct ->GameState
             [:bool
              :u32
              (bl/array 6 (bl/option player-layout))
              :pubkey
              :pubkey
              (bl/enum :NL10 :NL20 :NL50 :NL100)]))

(defn parse-state-data
  [data]
  (bl/unpack game-state-layout (bl/buffer-from data)))

(defrecord SolanaApi [])

(extend-type SolanaApi
 p/IChainApi
 (-settle-finished-game [this game-id chips-change-map player-status-map]
   (.debug js/console "game-id:" game-id)
   (.debug js/console "chips-change-map:" chips-change-map)
   (.debug js/console "player-status-map:" player-status-map)
   (go-try
    (let [fee-payer (load-private-key)

          conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some-> (<!? (conn/get-account-info conn game-account-pubkey))
                                     :data
                                     (parse-state-data))

          dealer-program-id (pubkey/make-public-key (get @config :dealer-program-address))

          {:keys [players]} game-account-state
          player-ids (->> players
                          (map (comp str :pubkey)))
          ix-body (->> (for [pid player-ids]
                         (let [chip-change (get chips-change-map pid 0)]
                           [[settle-status-normal 1]
                            [(cond
                               (zero? chip-change) settle-type-no-update
                               (pos? chip-change)  settle-type-chip-add
                               :else               settle-type-chip-sub)
                             1]
                            [(js/Math.abs chip-change) 4]]))
                       (mapcat identity))

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

      (conn/send-transaction conn tx [fee-payer]))))

 (-settle-failed-game [this game-id player-state-map])

 (-fetch-game-account [this game-id]
   (go-try
    ;; (log/infof "fetch game account state for game[%s]" game-id)
    (let [conn (conn/make-connection (get @config :solana-rpc-endpoint))
          game-account-pubkey (pubkey/make-public-key game-id)
          game-account-state (some-> (<!? (conn/get-account-info conn game-account-pubkey))
                                     :data
                                     (parse-state-data))]
      game-account-state))))

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
