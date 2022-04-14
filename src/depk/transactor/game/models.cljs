(ns depk.transactor.game.models
  "Models for game state machine."
  (:require
   ["uuid" :as uuid]
   [depk.transactor.constant :as c]))

(defn missing-state-id!
  []
  (throw (ex-info "Missing state id" {})))

(defrecord Event
  [type dispatch-id data player-id])

(defn make-event
  ([type state]
   (make-event type state {}))
  ([type state data]
   (make-event type state data nil))
  ([type state data player-id]
   (let [dispatch-id (:state-id state)]
     (->Event type dispatch-id data player-id))))

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
  ([player-id chips position]
   (make-player-state player-id chips position :player-status/wait :normal))
  ([player-id chips position status online-status]
   (into {}
         (map->PlayerState {:player-id     player-id,
                            :chips         chips,
                            :position      position,
                            :status        status,
                            :online-status online-status}))))

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
   ;; a serial number, read from blockchain
   game-no
   sb
   bb
   btn
   size
   ;; game status: init prepare shuffle encrypt key-share play showdown settle
   status
   ;; street: preflop flop turn river
   street
   player-map

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

   ;; dispatch event [ms, event]
   ;; an events dispatched with a delay
   dispatch-event
   ;; an ID for dispatched event, the event will be handled only when the IDs are same.
   ;; this property is managed by event loop
   ;; after each event, the value will be updated, unless reserve-dispatch-id is true
   dispatch-id
   ;; preserve current dispatch, means "don't update dispatch-id"
   reserve-dispatch-id

   ;; uuid, updated whenever state changes
   state-id

   ;; a list of player actions in this game
   player-actions

   ;; api requests
   api-requests

   ;; winning type: last-player showdown runner
   winning-type
  ])

;;; Initial state builder

(defn get-blind-bet
  "Get blind bet amount as [sb, bb]."
  [game-account-state mint-info]
  (let [{:keys [level]} game-account-state
        {:keys [decimals]} mint-info
        {:keys [sb bb]} (get c/level-info-map level)
        base (js/BigInt (js/Math.pow 10 decimals))]
    [(* base sb)
     (* base bb)]))

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
                                 :player-status/wait
                                 :dropout)))))
       (map (juxt :player-id identity))
       (into {})))

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
   (let [state-id (uuid/v4)
         [sb bb]  (get-blind-bet game-account-state mint-info)]
     (into {}
           (map->GameState
            (merge
             init-state
             {:sb                 sb,
              :bb                 bb,
              :game-account-state game-account-state,
              :state-id           state-id,
              :status             :game-status/init,
              :game-no            (:game-no game-account-state)
              :mint-info          mint-info,
              :player-map         (players->player-map (:players game-account-state))}))))))

(defn game-state->resp
  "Parse game state to response.

  This help reducing the response's body size."
  [state]
  (select-keys state
               [:game-no :status :street :player-map :pots :min-raise
                :street-bet :bet-map :action-player-id
                :showdown-map :prize-map :state-id :prepare-cards
                :shuffle-player-id :encrypt-player-id
                :btn :sb :bb :require-key-idents :share-key-map
                :card-ciphers :player-actions :winning-type :dispatch-id
                :game-id :this-event]))
