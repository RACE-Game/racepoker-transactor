(ns depk.transactor.game.manager
  "Game manager for game status in memory."
  (:require
   [cljs.core.async :refer [go-loop <! >! timeout close!]]
   [depk.transactor.util :refer [go-try <!?]]
   [depk.transactor.game.api :as api]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.game.sync-loop :as sync-loop]
   [depk.transactor.game.event-loop :as event-loop]
   [depk.transactor.game.api-transport :as api-transport]
   [taoensso.timbre :as log]))

(defn game-not-exist!
  [game-id]
  (throw (ex-info "Game not exist" {:game-id game-id})))

(defrecord GameManager [game-handle-map chain-api])

(defn load-game
  "Load a game into a manager, return game handle.

  Raise exception when there's no game on blockchain.

  TODO fix concurrent issue"
  [manager game-id]
  (go-try
   (let [chain-api (:chain-api manager)]
     (if-let [game-account-state (<!? (api/fetch-game-account chain-api game-id))]
       (let [init-state  (m/make-game-state game-account-state {})
             game-handle (handle/make-game-handle game-id init-state)]
         (event-loop/start-event-loop game-handle)
         (sync-loop/start-game-state-sync game-handle chain-api)
         (api-transport/start-api-transport game-handle chain-api)
         (swap! (:game-handle-map manager) assoc game-id game-handle)
         game-handle)
       (game-not-exist! game-id)))))

(defn find-game
  "Find a running game handle by game-id.

  If the game-handle is stopped, return nil."
  [manager game-id]
  (let [game-handle (get @(:game-handle-map manager) game-id)]
    (when (and game-handle
               (= :running (:event-loop (handle/get-status game-handle))))
      game-handle)))

(defn find-game-unchecked
  "Find a running game handle by game-id."
  [manager game-id]
  (get @(:game-handle-map manager) game-id))

(defn make-game-manager
  [chain-api]
  (log/info "Initialize game manager")
  (let [game-handle-map (atom {})]
    (map->GameManager {:game-handle-map game-handle-map,
                       :chain-api       chain-api})))
