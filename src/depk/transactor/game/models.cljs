(ns depk.transactor.game.models
  "Models for game state machine."
  (:require
   ["uuid" :as uuid]))

(defn missing-state-id!
  []
  (throw (ex-info "Missing state id" {})))

(defrecord Event
  [type state-id data player-id])

(defn make-event
  ([type current-state]
   (make-event type current-state {}))
  ([type current-state data]
   (make-event type current-state data nil))
  ([type current-state data player-id]
   (if-let [state-id (:state-id current-state)]
     (into {} (->Event type state-id data player-id))
     (missing-state-id!))))

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

   ;; online status: normal dropout leave
   online-status])

(defn make-player-state
  [player-id chips position]
  (into {} (map->PlayerState {:player-id player-id,
                              :chips     chips,
                              :position  position,
                              :status    :player-status/wait})))

(defrecord Pot
  [owner-ids amount winner-ids])

(defn make-pot
  ([owner-ids amount]
   (make-pot owner-ids amount nil))
  ([owner-ids amount winner-ids]
   (into {} (map->Pot {:owner-ids  owner-ids,
                       :amount     amount,
                       :winner-ids winner-ids}))))

(defrecord GameState
  [
   ;; sychronized onchain game state
   game-account-state

   ;; ----------------------------------------------
   ;; general info
   ;; game type: cash-game, sng
   game-type
   game-id
   sb
   bb
   btn
   size
   ;; game status: init shuffle encrypt key-share play showdown settle
   status
   ;; street: preflop flop turn river
   street
   player-map
   player-events

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

   ;; ----------------------------------------------
   ;; prepare states

   ;; prepare-cards represent the progress of decentralized cards shuffling
   ;; which is a list of map which has keys:
   ;; * data, card data, usually encrypted
   ;; * op, can be :init, :shuffle or :encrypt
   ;; * player-id, the operator id
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
   action-player-id

   ;; ----------------------------------------------
   ;; player id to showdown info
   showdown-map
   ;; winner to prize
   prize-map
   ;; player id to chips change
   chips-change-map

   ;; events, delayed event trigger
   dispatch-events

   ;; uuid, updated whenever state changes
   state-id

   ;; api requests
   api-requests
   ])

(defn make-game-state
  [game-account-state init-state]
  (let [state-id (uuid/v4)]
    (into {} (map->GameState
              (merge init-state
                     {:state-id           state-id,
                      :status             :game-status/init,
                      :game-account-state game-account-state})))))
