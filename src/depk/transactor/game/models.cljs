(ns depk.transactor.game.models
  "Models for game state machine."
  (:require
   ["uuid" :as uuid]
   [depk.transactor.constant :as c]
   [depk.transactor.log :as log]))

(defn missing-state-id!
  []
  (throw (ex-info "Missing state id" {})))

(defrecord Event
  [type data player-id timestamp])

(defn make-event
  ([type]
   (make-event type {}))
  ([type data]
   (make-event type data nil))
  ([type data player-id]
   (into {} (->Event type data player-id nil)))
  ([type data player-id timestamp]
   (into {} (->Event type data player-id timestamp))))

(defrecord PlayerState
  [
   ;; general info
   player-id
   ;; chips, sync with blockchain data
   chips
   ;; position: int, from 0 to (dec size)
   position
   ;; player status: wait in-action acted fold allin
   status
   ;; online status: normal dropout leave sit-out
   online-status
   ;; count the number of continuous drop
   drop-count
   ;; time bank remainning in seconds
   time-bank])

(defn make-player-state
  ([player-id chips position]
   (make-player-state player-id chips position :player-status/wait :normal))
  ([player-id chips position status online-status]
   (into {}
         (map->PlayerState {:player-id     player-id,
                            :chips         chips,
                            :position      position,
                            :status        status,
                            :online-status online-status,
                            :drop-count    0}))))

(defrecord Pot
  [owner-ids amount winner-ids])

(defn make-pot
  ([owner-ids amount]
   (make-pot owner-ids amount nil))
  ([owner-ids amount winner-ids]
   (into {}
         (map->Pot {:owner-ids  owner-ids,
                    :amount     amount,
                    :winner-ids winner-ids}))))

(defrecord GameState
  [
   ;; sychronized onchain game state
   game-account-state
   joined-players
   mint-info

   ;; ----------------------------------------------
   ;; general info
   ;; game type: cash-game, sng
   game-type
   game-id
   sb
   bb
   ante
   buyin
   btn
   rake
   size
   ;; game status: init prepare shuffle encrypt key-share play showdown settle
   status
   ;; Halt the game, waiting for the tournament reply
   halt?
   ;; street: preflop flop turn river
   street
   player-map

   ;; ----------------------------------------------
   ;; encryption
   rsa-pub-map
   ed-pub-map
   sig-map
   secret-nonce-map

   ;; ----------------------------------------------
   ;; SNG
   winner-id
   ranking

   start-time
   base-sb
   base-bb

   ;; ----------------------------------------------
   ;; encryption data and keys
   card-ciphers
   ;; player-id to shared keys
   ;; a map, key-ident -> key(plain or encrypted)
   ;; key-ident is [player-id ident-type ident-args]
   share-key-map
   ;; all requested keys
   require-key-idents
   ;; handler after key share, can be: init-street settle
   after-key-share

   ;; released keys, keys backup from fold players
   ;; a nested map, player-id -> released-keys
   ;; released-keys is a list of encrypt-key
   released-keys-map


   ;; ----------------------------------------------
   ;; prepare states

   ;; prepare-cards represent the progress of decentralized cards shuffling
   ;; which is a list of map which has keys:
   ;; * data, card data, usually encrypted
   ;; * op, can be :init, :shuffle or :encrypt
   ;; * player-id, the operator id
   op-player-ids
   prepare-cards
   shuffle-player-id
   encrypt-player-id

   ;; ----------------------------------------------
   ;; running states
   community-cards
   pots
   min-raise
   street-bet
   bet-map
   collect-bet-map
   action-player-id
   action-timeout-ts

   ;; ----------------------------------------------
   ;; player id to showdown info
   showdown-map
   ;; winner to prize
   prize-map
   ;; player id to chips change
   chips-change-map
   ;; the accumulator of current rake
   rake-map

   ;; dispatch event [ms, event]
   ;; an events dispatched with a delay
   dispatch-event
   ;; preserve current timeout dispatch
   reserve-timeout

   ;; uuid, updated whenever state changes
   state-id

   ;; api requests
   api-requests

   ;; winning type: last-player showdown runner
   winning-type

   ;; player ids of the leaving or kicked players
   leaving-player-ids

   ;; Display instructions
   ;; Decide how client will display
   display

   ;; Affect the :this-event key in state broadcast.
   ;; The default value of :this-event is the type of current event.
   ;; Set this value to overwrite.
   overwrite-this-event

   ;; Logs
   logs

   ;; shutdown event loop
   shutdown?
  ])

;;; Initial state builder

(defn get-blind-bet
  "Get blind bet amount as [sb, bb]."
  [game-account-state mint-info]
  (let [{:keys [level game-type]} game-account-state]
    (case game-type
      :cash
      (let [{:keys [decimals]} mint-info
            {:keys [sb bb]} (get c/level-info-map level)
            base (js/BigInt (js/Math.pow 10 decimals))]
        [(* base sb)
         (* base bb)])

      (:sng :bonus :tournament)
      [(js/BigInt 100)
       (js/BigInt 200)])))

(defn players->player-map
  [players]
  (->> players
       (keep-indexed
        (fn [idx p]
          (when p
            (let [player-id (str (:pubkey p))]
              (make-player-state player-id
                                 (:chips p)
                                 idx
                                 :player-status/fold
                                 :dropout)))))
       (map (juxt :player-id identity))
       (into {})))

(defn parse-raw-game-account-state
  "Parse game account state, remove useless fields."
  [game-account-state]
  (letfn [(parse-players [players]
                         (vec (for [p players]
                                (when p
                                  (into {} (update p :pubkey str))))))]
    (-> game-account-state
        (update :players parse-players)
        (select-keys [:name :game-type :players :buyin-serial :settle-serial]))))

(defn parse-raw-mint-info
  [mint-info]
  (-> mint-info
      (select-keys [:decimals])))

(defn make-game-state
  ([game-account-state init-state]
   (let [state-id (uuid/v4)]
     (into {}
           (map->GameState
            (merge init-state
                   {:state-id           state-id,
                    :status             :game-status/init,
                    :game-account-state game-account-state})))))
  ([game-account-state mint-info init-state]
   (let [state-id   (uuid/v4)
         player-map (players->player-map (:players game-account-state))]
     (into {}
           (map->GameState
            (merge
             init-state
             {:sb                 (:sb game-account-state),
              :bb                 (:bb game-account-state),
              :ante               (:ante game-account-state),
              :base-sb            (:sb game-account-state),
              :base-bb            (:bb game-account-state),
              :base-ante          (:ante game-account-state),
              :rake               (+ (js/BigInt (:transactor-rake game-account-state))
                                     (js/BigInt (:owner-rake game-account-state))),
              :game-account-state (parse-raw-game-account-state game-account-state),
              ;; If game already started, set a start-time
              ;; Mark this game is in progress
              ;; We can't kick players from a started SNG game
              :start-time         (cond
                                    (= :in-progress (:status game-account-state))
                                    (.getTime (js/Date.))

                                    (:start-time game-account-state)
                                    (:start-time game-account-state)),
              :state-id           state-id,
              :status             :game-status/init,
              :game-type          (:game-type game-account-state),
              :size               (:size game-account-state),
              :mint-info          (parse-raw-mint-info mint-info),
              :player-map         player-map}))))))

(defn game-state->resp
  "Parse game state to response.

  This help reducing the response's body size."
  [state]
  (dissoc state :game-account-state :mint-info))
