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
  [game-id x]
  (log/log "💀" game-id "Game worker error. %s" x))

(defn on-worker-exit
  [game-id clean-fn x]
  (log/log "💀" game-id "Game worker exit. %s" x)
  (clean-fn))

(defn make-worker
  [game-id opts]
  (log/log "📯" game-id "Spawn game worker")
  (let [{:keys [players clean-fn]} opts
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
                                           :players       players})}})
                    (.on "message" on-message)
                    (.on "error" (partial on-worker-error game-id))
                    (.on "exit" (partial on-worker-exit game-id clean-fn)))]
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
