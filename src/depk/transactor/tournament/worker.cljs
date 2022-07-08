(ns depk.transactor.tournament.worker
  "Dedicated worker for tournament process."
  (:require
   ["worker_threads"             :refer [Worker]]
   [depk.transactor.tournament-game :as tournament-game]
   [depk.transactor.state.worker-manager :refer [worker-manager]]
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.log          :as log]
   [depk.transactor.util         :as u]))

(defn send-event
  [worker event]
  (let [event (u/transit-write event)]
    (.postMessage ^js (:worker worker) event)))

(defn make-worker-message-handler
  [snapshot worker opts]
  (fn [data]
    (let [{:keys [tournament-id event serialized-state]} (u/transit-read data)]
      (when serialized-state
        (reset! snapshot {:serialized-state serialized-state}))

      (when event
        (let [{:keys [type data]} event]
          (case type
            ;; Start new tournament, create tables
            :system/start-tournament
            (let [{:keys [games]} data]
              (doseq [{:keys [game-id players size start-time]} games]
                (tournament-game/start-tournament-game
                 @worker-manager
                 {:tournament-id tournament-id,
                  :game-id       game-id,
                  :players       players,
                  :size          size,
                  :start-time    start-time,
                  :report-fn     (fn [event]
                                   (send-event {:worker worker} event))})))

            ;; Update the pseudo game-account-state
            (:system/next-game :system/resit-table)
            (let [{:keys [game-id]} data]
              (tournament-game/send-tournament-event
               @worker-manager
               game-id
               event))))))))

(defn on-worker-error
  [x]
  (log/errorf "💀Tournament worker error. %s" x))

(defn on-worker-exit
  [x]
  (log/infof "💀Tournament worker exit. %s" x))

(defn make-worker
  [tournament-id opts]
  (log/infof "📯Spawn tournament worker. %s" tournament-id)
  (let [snapshot   (atom {})
        worker     (Worker. js/__filename
                            #js
                             {:workerData
                              #js
                               {:worker "tournament",
                                :params (u/transit-write
                                         {:tournament-id tournament-id,
                                          :env           (name @env)})}})
        on-message (make-worker-message-handler snapshot worker opts)]
    (doto worker
     (.on "message" on-message)
     (.on "error" on-worker-error)
     (.on "exit" on-worker-exit)
     (.unref))
    {:worker   worker,
     :snapshot snapshot}))

(defn get-snapshot
  [worker]
  @(:snapshot worker))
