(ns depk.transactor.game.worker
  (:require
   ["path" :as path]
   ["worker_threads" :refer [Worker]]
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.constant :as c]
   [depk.transactor.log :as log]
   [depk.transactor.util :as u]))

(defn make-worker-message-handler
  [snapshot ws-conn]
  (let [{:keys [chsk-send! connected-uids]} ws-conn]
    (fn [data]
      (let [{:keys [game-id message]} (u/transit-read data)]
        (when (= :game/state (first message))
          (reset! snapshot (last message)))
        (doseq [uid   (:any @connected-uids)
                :when (= game-id (first uid))]
          (chsk-send! uid message))))))

(defn on-worker-error
  [x]
  (log/errorf "💀Worker error. %s" x))

(defn on-worker-exit
  [x]
  (log/infof "💀Worker exit. %s" x))

(defn make-worker
  [game-id ws-conn]
  (let [snapshot   (atom {})
        on-message (make-worker-message-handler snapshot ws-conn)
        worker     (doto (Worker. js/__filename
                                  #js
                                   {:workerData #js
                                                 {"game-id" game-id,
                                                  "env"     (name @env)}})
                    (.on "message" on-message)
                    (.on "error" on-worker-error)
                    (.on "exit" on-worker-exit)
                    (.ref))]
    (log/infof "📯Spawn worker. %s" game-id)
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
