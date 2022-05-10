(ns depk.transactor.chain.state
  (:require
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]
   [clojure.string :as str]))

(def pubrsa-len 162)
(def max-joined-num 3)

(defmethod bl/size :str64
  [_]
  64)

(defmethod bl/unpack :str64
  [_ ^js buf]
  (str/replace (.toString (.slice buf 0 64))
               (js/String.fromCharCode 0)
               ""))

(defmethod bl/unpack :str16
  [_ buf]
  (str/replace (.toString (.slice buf 0 16))
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
(def set-winner 8)
(def cancel-game-ix-id 9)

(def ^:const max-players-num 9)

(defrecord Player [pubkey chips buyin-serial rebuy])

(def player-layout (bl/struct ->Player [:pubkey :u64 :u32 :u8]))

(defrecord GameState [is-initialized buyin-serial settle-serial players stake-account-pubkey
                      mint-pubkey ante sb bb buyin size game-type transactor-pubkey owner-pubkey
                      transactor-rake owner-rake status bonus-pubkey name])

(def game-state-layout
  (bl/struct ->GameState
             [;; is_initialized
              :bool
              ;; buyin_serial
              :u32
              ;; settle_serial
              :u32
              ;; players
              (bl/array max-players-num (bl/option player-layout))
              ;; stake_account_pubkey
              :pubkey
              ;; mint_pubkey
              :pubkey
              ;; ante
              :u64
              ;; sb
              :u64
              ;; bb
              :u64
              ;; buyin
              :u64
              ;; size
              :u8
              ;; game_type
              (bl/enum :cash :sng :bonus :tournament)
              ;; transactor_pubkey
              :pubkey
              ;; owner_pubkey
              :pubkey
              ;; transactor-rake
              :u16
              ;; owner-rake
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

(defrecord PlayerProfileState [is-initialized avatar-pubkey nick])

(def player-profile-state-layout
  (bl/struct ->PlayerProfileState
             [:bool
              :pubkey
              :str64]))

(def player-profile-state-data-len
  (bl/size player-profile-state-layout))

(defn parse-profile-state-data
  [data]
  (bl/unpack player-profile-state-layout (bl/buffer-from data)))
