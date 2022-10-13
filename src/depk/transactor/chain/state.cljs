(ns depk.transactor.chain.state
  (:require
   [solana-clj.extra.buffer-layout :as    bl
                                   :refer [buffer-from]]
   [clojure.string :as str]))

(def pubrsa-len 162)

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

(defmethod bl/unpack :arweave-id
  [_ buf]
  (str/replace (.toString (.slice buf 0 43))
               (js/String.fromCharCode 0)
               ""))

(defmethod bl/size :arweave-id
  [type]
  43)

(defmethod bl/unpack :str140
  [_ buf]
  (str/replace (.toString (.slice buf 0 140))
               (js/String.fromCharCode 0)
               ""))

(defmethod bl/size :str140
  [type]
  140)

(def init-player-profile-ix-id 0)
(def close-player-profile-ix-id 1)

(def init-game-ix-id 0)
(def buyin-ix-id 1)
(def settle-ix-id 2)
(def set-winner-ix-id 3)
(def cancel-game-ix-id 4)
(def rebuy-ix-id 5)
(def attach-bonus-ix-id 6)
(def init-bonus-ix-id 7)
(def close-game-ix-id 8)
(def init-tournament-ix-id 9)
(def join-tournament-ix-id 10)
(def settle-tournament-ix-id 11)
(def start-tournament-ix-id 12)
(def deposit-bonus-ix-id 13)
(def deposit-prize-ix-id 14)
(def claim-prize-ix-id 15)
(def claim-bonus-ix-id 16)
(def grant-ix-id 17)
(def reg-tournament-ix-id 18)
(def init-reg-accounts-ix-id 19)
(def reg-game-ix-id 20)
(def unreg-game-ix-id 21)

;; Game Account

(def max-players-num 9)
(def max-winners 20)
(def max-bonuses 20)

(defrecord Player [pubkey chips buyin-serial rebuy])

(def player-layout (bl/struct ->Player [:pubkey :u64 :u32 :u8]))

(defrecord GameState [is-initialized buyin-serial settle-serial scene-pubkey players
                      stake-account-pubkey mint-pubkey ante sb bb buyin size game-type
                      transactor-pubkey owner-pubkey transactor-rake owner-rake status bonus-pubkey
                      name])

(def game-state-layout
  (bl/struct ->GameState
             [;; is_initialized
              :bool
              ;; buyin_serial
              :u32
              ;; settle_serial
              :u32
              ;; scene_pubkey
              :pubkey
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

;; Player Profile

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

;; Bonus Account

(defrecord BonusState [is-initialized owner-pubkey mint-pubkey stake-pubkey quota ex-rate])

(def bonus-state-layout
  (bl/struct ->BonusState
             [:bool
              :pubkey
              :pubkey
              :pubkey
              (bl/array max-players-num :u64)
              :u64]))

(def bonus-state-data-len
  (bl/size bonus-state-layout))

(defn parse-bonus-state-data
  [data]
  (bl/unpack bonus-state-layout (bl/buffer-from data)))

;; Tournament ranks

(defrecord Registration [pubkey rebuy buyin-serial ranking is-settled])

(def registration-layout
  (bl/struct ->Registration
             [:pubkey ; pubkey
              :u8     ; rebuy
              :u32    ; buyin-serial
              :u16    ; ranking
              :bool   ; is_settled
             ]))

(def registration-data-len (bl/size registration-layout))

(defn parse-registration-data
  [data]
  (bl/unpack registration-layout (bl/buffer-from data)))

;; Tournament Account
;; Since the number of participants is uncertain, we need another account to save the ranks
;; The size of the ranks will be stored in this account.

(defrecord TournamentState [is-initialized settle-serial buyin-serial scene-pubkey size
                            transactor-pubkey
                            owner-pubkey ticket-pubkey ticket-price max-players
                            num-players buyin-limit start-time status name
                            transactor-rake owner-rake blinds-mode total-prize
                            registration-pubkey prize-pubkey start-chips
                            bonuses prize-quota cover desc])

(defrecord BonusItem [stake-pubkey ranking])

(def bonus-item-layout
  (bl/struct ->BonusItem
             [:pubkey
              :u16]))

(def tournament-state-layout
  (bl/struct ->TournamentState
             [:bool   ; is-initialized
              :u32    ; settle-serial
              :u32    ; buyin-serial
              :pubkey ; scene-pubkey
              :u8     ; size
              :pubkey ; transactor-pubkey
              :pubkey ; owner-pubkey
              :pubkey ; ticket-pubkey
              :u64    ; ticket-price
              :u16    ; max-players
              :u16    ; num-players
              :u8     ; buyin-limit
              :u32    ; start-time
              (bl/enum :registering :playing :completed) ; status
              :str16 ; name
              :u16   ; transactor-rake
              :u16   ; owner-rake
              (bl/enum :normal :turbo :hyper)
              :u64   ; total-prize
              :pubkey ; registration-pubkey
              :pubkey ; prize-pubkey
              :u64 ; start-chips
              (bl/array max-bonuses (bl/option bonus-item-layout))
              (bl/array max-winners :u16)
              :arweave-id ;cover
              :str140     ; desc
             ]))

(def tournament-state-data-len (bl/size tournament-state-layout))

(defn parse-tournament-state-data
  [data]
  (bl/unpack tournament-state-layout (bl/buffer-from data)))

;; Registration

(defrecord RegCenter [is-initialized is-private owner tournament-reg game-reg])

(def reg-center-layout
  (bl/struct ->RegCenter
             [:bool
              :bool
              :pubkey
              :pubkey
              :pubkey]))

(def reg-center-data-len (bl/size reg-center-layout))

(defn parse-reg-center-data
  [data]
  (bl/unpack reg-center-layout (bl/buffer-from data)))

(defrecord TournamentReg [pubkey mint reg-time start-time is-hidden])

(def tournament-reg-layout
  (bl/struct ->TournamentReg
             [:pubkey
              :pubkey
              :u32
              :u32
              :bool]))

(def tournament-reg-data-len (bl/size tournament-reg-layout))

(defn parse-tournament-reg-data
  [data]
  (bl/unpack tournament-reg-layout (bl/buffer-from data)))

(defrecord GameReg [pubkey mint is-private])

(def game-reg-layout
  (bl/struct ->GameReg
             [:pubkey
              :pubkey
              :bool]))

(def game-reg-data-len (bl/size game-reg-layout))

(defn parse-game-reg-data
  [data]
  (bl/unpack game-reg-layout (bl/buffer-from data)))
