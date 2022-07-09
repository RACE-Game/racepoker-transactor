(ns depk.transactor.game.handle
  "Game handle is used to control a set of components of a game.

  A running game includes following components:
      - Event bus
      - Chain API
      - Store API
      - Broadcaster
      - Event loop
  "
  (:require
   [cljs.core.async :as a]
   [depk.transactor.game.models :as m]
   [depk.transactor.event :as event]
   [depk.transactor.chain :as chain]
   [depk.transactor.store :as store]
   [depk.transactor.broadcast :as broadcast]
   [depk.transactor.game.event-loop :as eloop]
   [depk.transactor.log :as log]))

(defrecord GameHandle
  [event-bus
   chain-api
   store-api
   broadcaster
   event-loop])

(defn make-game-handle
  [game-id post-msg-fn]
  (log/infof "🏁Create game handle for game: %s" game-id)
  (a/go
   (let [chain-api          (chain/make-solana-api)
         game-account-state (a/<! (chain/fetch-game-account chain-api
                                                            game-id
                                                            {:commitment "finalized"}))
         mint-info          (a/<! (chain/fetch-mint-info chain-api
                                                         (str (:mint-pubkey
                                                               game-account-state))))
         init-state         (m/make-game-state game-account-state mint-info {:game-id game-id})
         opts               {:game-id game-id, :init-state init-state}
         synchronizer       (chain/make-synchronizer chain-api)
         submitter          (chain/make-submitter chain-api)
         event-bus          (event/make-mem-event-bus)
         store-api          (store/make-fake-store-api)
         broadcaster        (broadcast/make-game-broadcaster post-msg-fn)
         event-loop         (eloop/make-event-loop)]
     ;; Attach components to event bus
     (event/attach event-loop event-bus)
     (event/attach synchronizer event-bus)
     (event/attach submitter event-bus)
     (event/attach store-api event-bus)
     (event/attach broadcaster event-bus)
     ;; Start components
     (event/start-component event-bus opts)
     (event/start-component synchronizer opts)
     (event/start-component submitter opts)
     (event/start-component event-loop opts)
     (event/start-component store-api opts)
     (event/start-component broadcaster opts)

     (log/infof "🏁Game handle started")
     (->GameHandle event-bus chain-api store-api broadcaster event-loop))))

(defn game-handle?
  [x]
  (instance? GameHandle x))

(defn send-event
  [game-handle event]
  (event/send (:event-bus game-handle) event))
