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
   [depk.transactor.log :as log]))

(defn game-not-exist!
  [game-id]
  (throw (ex-info "Game not exist" {:game-id game-id})))

(defrecord GameManager [game-handle-map chain-api])

(defn assign-new-game-handle
  [game-handle-map game-id game-account-state chain-api store-api]
  (if (get game-handle-map game-id)
    game-handle-map
    (let [init-state  (m/make-game-state game-account-state {:game-id game-id})
          game-handle (handle/make-game-handle game-id init-state)]
      (event-loop/start-event-loop game-handle)
      (sync-loop/start-game-state-sync game-handle chain-api)
      (api-transport/start-api-transport game-handle chain-api store-api)
      (assoc game-handle-map game-id game-handle))))

(defn load-game
  "Load a game into a manager, return game handle.

  Raise exception when there's no game on blockchain."
  [manager game-id]
  (go-try
   (let [{:keys [chain-api store-api]} manager
         game-account-state (<!? (api/fetch-game-account chain-api game-id))
         _ (when-not game-account-state (game-not-exist! game-id))]
     (swap! (:game-handle-map manager)
       assign-new-game-handle
       game-id
       game-account-state
       chain-api
       store-api)
     (get @(:game-handle-map manager) game-id))))

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
  [chain-api store-api]
  (log/info "Initialize game manager")
  (let [game-handle-map (atom {})]
    (map->GameManager {:game-handle-map game-handle-map,
                       :chain-api       chain-api,
                       :store-api       store-api})))

(defn fetch-game-histories
  [manager game-id]
  (let [{:keys [store-api]} manager]
    (api/fetch-game-histories store-api game-id)))
