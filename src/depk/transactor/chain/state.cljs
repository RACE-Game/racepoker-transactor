(ns depk.transactor.chain.state
  (:require
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]
   [clojure.string :as str]))

(defmethod bl/unpack :str16
  [_ buf]
  (str/replace (.toString buf)
               (js/String.fromCharCode 0)
               ""))

(defmethod bl/size :str16
  [type]
  16)

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

(defrecord GameState [is-initialized game-no players stake-account-pubkey mint-pubkey
                      level size game-type transactor-pubkey owner-pubkey rake status
                      bonus-pubkey name])

(def game-state-layout
  (bl/struct ->GameState
             [;; is_initialized
              :bool
              ;; game_no
              :u32
              ;; players
              (bl/array max-players-num (bl/option player-layout))
              ;; stake_account_pubkey
              :pubkey
              ;; mint_pubkey
              :pubkey
              ;; level
              (bl/enum :nl100 :nl200 :nl500 :nl1000 :sng)
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
              ;; bonus_pubkey
              (bl/option :pubkey)
              ;; name
              :str16
             ]))

(defn parse-state-data
  [data]
  (bl/unpack game-state-layout (bl/buffer-from data)))

(def game-state-data-len (bl/size game-state-layout))
