(ns depk.transactor.game.manager
  "Game manager for game status in memory."
  (:require
   [depk.transactor.util        :refer [go-try <!?]]
   [depk.transactor.game.worker :as worker]
   [depk.transactor.log         :as log]
   [depk.transactor.chain       :as chain]))

(defn game-not-exist!
  [game-id]
  (throw (ex-info "Game not exist" {:game-id game-id})))

(defrecord GameManager [game-worker-map])

(defn assign-new-game-worker
  [game-worker-map game-id ws-conn]
  (if (get game-worker-map game-id)
    game-worker-map
    (let [game-worker (worker/make-worker game-id ws-conn)]
      (assoc game-worker-map game-id game-worker))))

(defn find-game-unchecked
  "Find a running game handle by game-id."
  [manager game-id]
  (get @(:game-worker-map manager) game-id))

(defn find-game
  "Find a running game worker by game-id, will throw raise an exception when not found."
  [manager game-id]
  (if-let [worker (find-game-unchecked manager game-id)]
    worker
    (game-not-exist! game-id)))

(defn try-start-game
  "Try to start game worker when non-exist."
  [manager game-id]
  (go-try
   (when-not (find-game-unchecked manager game-id)
     (let [{:keys [game-worker-map ws-conn]} manager]
       (swap! game-worker-map assign-new-game-worker game-id ws-conn)))))

(defn make-game-manager
  [ws-conn]
  (log/info "🏁Initialize game manager")
  (let [game-worker-map (atom {})]
    (map->GameManager {:game-worker-map game-worker-map,
                       :ws-conn         ws-conn})))

(defn fetch-game-histories
  [manager game-id])

(defn list-game-ids-by-player-id
  [manager player-id]
  (when-let [worker-map @(:game-worker-map manager)]
    (->> worker-map
         (keep (fn [[game-id w]]
                 (let [player-ids (some-> (worker/get-snapshot w)
                                          :player-map
                                          keys)]
                   (when (seq (filter #(= % player-id) player-ids))
                     game-id)))))))

(defn list-running-games
  [manager]
  (when-let [worker-map @(:game-worker-map manager)]
    (->> worker-map
         (keep (fn [[game-id w]]
                 (let [{:keys [player-map start-time]} (worker/get-snapshot w)]
                   [game-id
                    {:start-time start-time,
                     :player-ids (keys player-map)}])))
         (into {}))))

(defn list-players
  [manager]
  (when-let [worker-map @(:game-worker-map manager)]
    (->> worker-map
         (mapcat (fn [[_ w]]
                   (let [{:keys [player-map]} (worker/get-snapshot w)]
                     (keys player-map))))
         (distinct))))
