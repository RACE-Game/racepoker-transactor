(ns depk.transactor.game.handle
  "Game handle is used to control a set of components of a game.

  A running game includes following components:
    * Event bus
    * Chain API
    * Store API
    * Broadcaster
    * Event loop
  "
  (:require
   [cljs.core.async           :as a]
   [depk.transactor.event     :as event]
   [depk.transactor.chain     :as chain]
   [depk.transactor.store     :as store]
   [depk.transactor.broadcast :as broadcast]
   [depk.transactor.game.event-loop :as eloop]
   [depk.transactor.log       :as log]))

(defrecord GameHandle
  [event-bus
   chain-api
   store-api
   broadcaster
   event-loop])

(defn make-game-handle
  [game-id init-state ws-conn]
  (log/infof "🏁Create game handle for game: %s" game-id)
  (let [opts        {:game-id game-id, :init-state init-state}
        chain-api   (chain/make-solana-api)
        event-bus   (event/make-mem-event-bus)
        store-api   (store/make-fake-store-api)
        broadcaster (broadcast/make-broadcaster ws-conn)
        event-loop  (eloop/make-event-loop)]
    ;; Attach components to event bus
    (event/attach event-loop event-bus)
    (event/attach chain-api event-bus)
    (event/attach store-api event-bus)
    (event/attach broadcaster event-bus)
    ;; Start components
    (event/start-component event-bus opts)
    (event/start-component chain-api opts)
    (event/start-component event-loop opts)
    (event/start-component store-api opts)
    (event/start-component broadcaster opts)

    (->GameHandle event-bus chain-api store-api broadcaster event-loop)))

(defn game-handle?
  [x]
  (instance? GameHandle x))

(defn send-event
  [game-handle event]
  (event/send (:event-bus game-handle) event))

(defn get-snapshot
  [game-handle]
  (broadcast/get-snapshot (:broadcaster game-handle)))

(defn shutdown-game-handle
  [game-handle]
  (event/shutdown (:event-bus game-handle)))
