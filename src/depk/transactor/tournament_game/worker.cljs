(ns depk.transactor.tournament-game.worker
  (:require
   ["path" :as path]
   ["worker_threads" :refer [Worker]]
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.log :as log]
   [depk.transactor.util :as u]))

(defn make-worker-message-handler
  [snapshot opts]
  (let [{:keys [report-fn]} opts
        {:keys [chsk-send! connected-uids]} (:websocket opts)]
    (fn [data]
      (let [data (u/transit-read data)]
        (case (:broadcast data)
          :broadcast/tournament-game-settle
          (let [{:keys [game-id settle-map tournament-id]}
                data

                event {:type          :system/tournament-game-settle,
                       :tournament-id tournament-id,
                       :game-id       game-id,
                       :data          {:settle-map settle-map}}]

            (report-fn event))

          :broadcast/game-event
          (let [{:keys [game-id event serialized-state player-ids start-time]}
                data]
            (reset! snapshot
              {:serialized-state serialized-state,
               :player-ids       player-ids,
               :start-time       start-time})
            (when event
              (doseq [uid   (:any @connected-uids)
                      :when (= game-id (first uid))]
                (chsk-send! uid event)))))))))

(defn on-worker-error
  [game-id x]
  (log/log "💀" game-id "Tournament Game worker error. %s" x))

(defn on-worker-exit
  [game-id x]
  (log/log "💀" game-id "Tournament Game worker exit. %s" x))

(defn make-worker
  [game-id opts]
  (let [{:keys [tournament-id players size start-time]} opts
        snapshot   (atom {})
        on-message (make-worker-message-handler snapshot opts)
        worker     (doto
                    (Worker. js/__filename
                             #js
                              {:workerData
                               #js
                                {:worker "tournament-game",
                                 :params (u/transit-write
                                          {:game-id       game-id,
                                           :env           @env,
                                           :tournament-id tournament-id,
                                           :size          size,
                                           :players       players,
                                           :start-time    start-time})}})
                    (.on "message" on-message)
                    (.on "error" (partial on-worker-error game-id))
                    (.on "exit" (partial on-worker-exit game-id))
                    (.ref))]
    (log/log "📯" game-id "Spawn tournament game worker")
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
