(ns depk.transactor.game.api.state
  (:require
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]))

(def init-game-ix-id 0)
(def buyin-ix-id 1)
(def claim-ix-id 2)
(def settle-ix-id 3)
(def init-player-profile-ix-id 4)
(def close-player-profile-ix-id 5)
(def close-game-ix-id 6)
(def open-game-ix-id 7)

(def ^:const max-players-num 9)

(defrecord Player [pubkey chips rebuy])

(def player-layout (bl/struct ->Player [:pubkey :u64 :u8]))

(defrecord GameState [is-initialized game-no players stack-account-pubkey mint-pubkey
                      level size game-type transactor-pubkey owner-pubkey rake status])

(def game-state-layout
  (bl/struct ->GameState
             [;; is_initialized
              :bool
              ;; game_no
              :u32
              ;; players
              (bl/array max-players-num (bl/option player-layout))
              ;; stack_account_pubkey
              :pubkey
              ;; mint_pubkey
              :pubkey
              ;; level
              (bl/enum :NL100 :NL200 :NL500 :NL1000)
              ;; size
              :u8
              ;; game_type
              (bl/enum :cash :sng :bonus :tournament)
              ;; transactor_pubkey
              :pubkey
              ;; owner_pubkey
              :pubkey
              ;; rake
              :u16
              ;; status
              (bl/enum :open :wait-claim :locked :in-progress :closed)
             ]))

(defn parse-state-data
  [data]
  (bl/unpack game-state-layout (bl/buffer-from data)))

(def game-state-data-len (bl/size game-state-layout))
