(ns depk.transactor.tournament.worker
  "Dedicated worker for tournament process."
  (:require
   ["worker_threads" :refer [Worker]]
   [cljs.core.async :as a]
   [depk.transactor.tournament-game :as tournament-game]
   [depk.transactor.state.worker-manager :refer [worker-manager]]
   [depk.transactor.state.config :refer [env]]
   [depk.transactor.log :as log]
   [depk.transactor.util :as u]))

(defn send-event
  [worker event]
  (let [event (u/transit-write event)]
    (.postMessage ^js (:worker worker) event)))

(defn make-worker-message-handler
  [snapshot worker {:keys [tournament-id]}]
  (let [input (a/chan 24)]
    (a/go-loop [event (a/<! input)]
      (when event
        (let [{:keys [type data]} event]
          (case type
            ;; Start new tournament, create tables
            :system/start-tournament
            (let [{:keys [games start-time blinds-mode]} data]
              (doseq [{:keys [game-id players size]} games]
                ;; Wait till the game is started
                (a/<! (tournament-game/start-tournament-game
                       @worker-manager
                       {:tournament-id tournament-id,
                        :game-id       game-id,
                        :players       players,
                        :size          size,
                        :blinds-mode   blinds-mode,
                        :report-fn     (fn [event]
                                         (send-event {:worker worker} event))}))
                (tournament-game/send-tournament-event
                 @worker-manager
                 game-id
                 {:type :system/start-tournament-game,
                  :data {:start-time start-time}})))

            ;; Update the pseudo game-account-state
            (:system/next-game :system/resit-table :system/sync-state)
            (let [{:keys [game-id]} data]
              (tournament-game/send-tournament-event
               @worker-manager
               game-id
               event))))
        (recur (a/<! input))))

    (fn [data]
      (let [{:keys [event serialized-state]} (u/transit-read data)]
        (when serialized-state
          (reset! snapshot {:serialized-state serialized-state}))
        (when event
          (a/put! input event))))))

(defn on-worker-error
  [tournament-id x]
  (log/log "💀" tournament-id "Tournament worker error. %s" x))

(defn on-worker-exit
  [tournament-id clean-fn x]
  (log/log "💀" tournament-id "Tournament worker exit. %s" x)
  (a/go
    ;; Wait 10 mins before remove it
    ;; Otherwise it will keep starting
   (a/<! (a/timeout 600000))
   (clean-fn)))

(defn make-worker
  [tournament-id opts]
  (log/log "📯" tournament-id "Spawn tournament worker")
  (let [snapshot   (atom {})
        {:keys [clean-fn]} opts
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
     (.on "error" (partial on-worker-error tournament-id))
     (.on "exit" (partial on-worker-exit tournament-id clean-fn))
     (.unref))
    {:worker   worker,
     :snapshot snapshot}))

(defn get-snapshot
  [worker]
  @(:snapshot worker))
