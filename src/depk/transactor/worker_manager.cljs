(ns depk.transactor.worker-manager
  "Worker manager for game/tournament states in memory."
  (:require
   [depk.transactor.util :refer [go-try]]
   [depk.transactor.log  :as log]))

(defn worker-not-exist!
  [id]
  (throw (ex-info "Worker not found" {:id id})))

(defrecord WorkerManager [worker-opts worker-map])

(defn assign-new-worker
  [worker-map id builder start-opts]
  (if (get worker-map id)
    worker-map
    (let [worker (builder id start-opts)]
      (assoc worker-map id worker))))

(defn find-worker-unchecked
  "Find a running worker handle by id."
  [manager id]
  (get @(:worker-map manager) id))

(defn find-worker
  "Find a running worker by id, will throw raise an exception when not found."
  [manager id]
  (if-let [worker (find-worker-unchecked manager id)]
    worker
    (worker-not-exist! id)))

(defn try-start
  "Try to start worker when non-exist."
  ([manager id builder]
   (try-start manager id builder nil))
  ([manager id builder start-opts]
   (go-try
    (when-not (find-worker-unchecked manager id)
      (let [{:keys [worker-map worker-opts]} manager
            clean-fn (fn [] (swap! worker-map dissoc id))
            opts     (merge worker-opts start-opts {:clean-fn clean-fn})]
        (swap! worker-map assign-new-worker id builder opts))))))

(defn make-worker-manager
  "Create worker manager."
  [worker-opts]
  (log/log "🎉" nil "Initialize worker manager")
  (let [worker-map (atom {})]
    (map->WorkerManager {:worker-map  worker-map,
                         :worker-opts worker-opts})))
