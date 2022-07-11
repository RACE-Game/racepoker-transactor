(ns depk.transactor.game.worker
  (:require
   ["path" :as path]
   ["worker_threads" :refer [Worker]]
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.log :as log]
   [depk.transactor.util :as u]))

(defn make-worker-message-handler
  [snapshot opts]
  (let [{:keys [chsk-send! connected-uids]} (:websocket opts)]
    (fn [data]
      (let [{:keys [game-id event serialized-state player-ids start-time]} (u/transit-read data)]
        (when serialized-state
          (reset! snapshot
            {:serialized-state serialized-state,
             :player-ids       player-ids,
             :start-time       start-time}))

        (when event
          (doseq [uid   (:any @connected-uids)
                  :when (= game-id (first uid))]
            (chsk-send! uid event)))))))

(defn on-worker-error
  [x]
  (log/errorf "💀Game worker error. %s" x))

(defn on-worker-exit
  [x]
  (log/infof "💀Game worker exit. %s" x))

(defn make-worker
  [game-id opts]
  (log/infof "📯Spawn game worker. %s" game-id)
  (let [{:keys [tournament-id players]} opts
        snapshot   (atom {})
        on-message (make-worker-message-handler snapshot opts)
        worker     (doto
                    (Worker. js/__filename
                             #js
                              {:workerData
                               #js
                                {:worker "game",
                                 :params (u/transit-write
                                          {:game-id       game-id,
                                           :env           @env,
                                           :tournament-id tournament-id,
                                           :players       players})}})
                    (.on "message" on-message)
                    (.on "error" on-worker-error)
                    (.on "exit" on-worker-exit)
                    (.ref))]
    {:worker   worker,
     :snapshot snapshot}))

(defn send-event
  [worker event]
  (let [event (u/transit-write event)]
    (.postMessage ^js (:worker worker) event)))

(defn shutdown
  [worker]
  (.postMessage ^js (:worker worker)
                #js {:type "shutdown"}))

(defn get-snapshot
  [worker]
  @(:snapshot worker))
