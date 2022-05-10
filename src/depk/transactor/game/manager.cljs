(ns depk.transactor.game.manager
  "Game manager for game status in memory."
  (:require
   [depk.transactor.util        :refer [go-try <!?]]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.log         :as log]
   [depk.transactor.chain       :as chain]
   [depk.transactor.game.models :as m]
   [depk.transactor.state.global-chain-api :refer [global-chain-api]]))

(defn game-not-exist!
  [game-id]
  (throw (ex-info "Game not exist" {:game-id game-id})))

(defrecord GameManager [game-handle-map])

(defn assign-new-game-handle
  [game-handle-map game-id init-state ws-conn]
  (if (get game-handle-map game-id)
    game-handle-map
    (let [game-handle (handle/make-game-handle game-id init-state ws-conn)]
      (assoc game-handle-map game-id game-handle))))

(defn find-game-unchecked
  "Find a running game handle by game-id."
  [manager game-id]
  (get @(:game-handle-map manager) game-id))

(defn find-game
  "Find a running game handle by game-id, will throw raise an exception when not found."
  [manager game-id]
  (if-let [handle (find-game-unchecked manager game-id)]
    handle
    (throw (ex-info "Game not found!" {:game-id game-id}))))

(defn try-start-game
  "Try to start game handle when non-exist."
  [manager game-id]
  (go-try
   (when-not (find-game-unchecked manager game-id)
     (let [game-account-state (<!? (chain/fetch-game-account @global-chain-api
                                                             game-id
                                                             {:commitment "finalized"}))

           ;; Use a fixed one for SNG/Bonus game.
           mint-info          (<!? (chain/fetch-mint-info @global-chain-api
                                                          (str (:mint-pubkey
                                                                game-account-state))))
           init-state         (m/make-game-state game-account-state mint-info {:game-id game-id})
           {:keys [game-handle-map]} manager]
       (swap! game-handle-map assign-new-game-handle game-id init-state (:ws-conn manager))))))

(defn make-game-manager
  [ws-conn]
  (log/info "🏁Initialize game manager")
  (let [game-handle-map (atom {})]
    (map->GameManager {:game-handle-map game-handle-map,
                       :ws-conn         ws-conn})))

(defn fetch-game-histories
  [manager game-id]
  ;; (let [{:keys [store-api]} manager]
  ;;   (api/fetch-game-histories store-api game-id))
)

(defn list-game-ids-by-player-id
  [manager player-id]
  (when-let [handle-map @(:game-handle-map manager)]
    (->> handle-map
         (keep (fn [[game-id h]]
                 (let [player-ids (some-> (handle/get-snapshot h)
                                          :player-map
                                          keys)]
                   (when (seq (filter #(= % player-id) player-ids))
                     game-id)))))))
