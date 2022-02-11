(ns depk.transactor.game.event-loop
  (:require
   [cljs.core.async :as    a
                    :refer [go go-loop <! timeout close! put!]]
   [depk.transactor.game.event-handler :as event-handler]
   ["uuid" :as uuid]
   [taoensso.timbre :as log]
   [depk.transactor.util :refer [go-try <!? log-group-collapsed log-group-end info error]]))

(defn expired-event!
  [state event]
  (ex-info "drop expired event" {:event event, :state-id (:state-id state)}))

(defn handle-event
  "Apply event to state, return a channel that contains the result map.

  The result map contains:
  * result, :ok | :err
  * state, the new state if succeed, the original state otherwise
  * (optional)error, an Exception

  An event with an invalid state-id is considered to be expired."
  [state event]
  (go
   (if (not= (:state-id state) (:state-id event))
     (do
       (info "drop expired event[%s]" (name (:type event)))
       {:result :err, :state state, :error (expired-event! state event)})
     (try
       (info "event[%s] %s" (name (:type event)) (prn-str event))
       ;; (js/console.log "state: " state)
       (let [new-state-id (uuid/v4)
             new-state    (event-handler/handle-event (assoc state :state-id new-state-id) event)]
         (if (map? new-state)
           (do
             (info "status:" (name (:status new-state)))
             {:result :ok, :state new-state})
           (let [v (<! new-state)]
             (if (map? v)
               (do
                 (info "status:" (name (:status v)))
                 {:result :ok, :state v})
               (do
                 (error "error from event handler: %s" (ex-message v))
                 {:result :err, :state state, :error v})))))
       (catch ExceptionInfo e
         (error "error in event loop: %s" (ex-message e))
         {:result :err, :state state, :error e})))))

(defn dispatch-delay-event
  "Dispatch dispatch-events to input channel.

  dispatch-events is a map from timeout millis to event."
  [dispatch-events input]
  (doseq [[ms event] dispatch-events]
    (when event
      (log/debugf "dispatch event[%s] after %sms"
                  (str (:type event))
                  (str ms))
      (go-try
       (<!? (timeout ms))
       (put! input event)))))

(defn dispatch-api-request
  "Dispatch api-requests to output channel."
  [api-requests output]
  (doseq [req api-requests]
    (when req
      (log/debugf "dispatch api request[%s]" (prn-str req))
      (put! output req))))

(defn collect-and-dispatch-game-history
  "Collect and (maybe) dispatch game history to output channel,
  return the new records."
  [old-state new-state event records output]

  (if (= (:game-no old-state) (:game-no new-state))
    (conj records [event new-state])
    (do
      (when (:game-no old-state)
        (put! output
              {:api-request/type :save-game-history,
               :game-id          (:game-id old-state),
               :game-no          (:game-no old-state),
               :records          records}))
      [])))

(defn start-event-loop
  [game-handle]
  (let [{:keys [snapshot input output game-id]} game-handle]
    (log/infof "start event loop for game[%s]" game-id)
    (go-loop [state   @snapshot
              records []]
      (let [event (<! input)]
        (if event
          (let [old-state state
                {:keys [result state]} (<! (handle-event state event))
                {:keys [dispatch-events api-requests]} state
                ;; flush dispatch-events & api-requests
                new-state (dissoc state :dispatch-events :api-requests)]
            (if (= result :ok)
              (let [records
                    (collect-and-dispatch-game-history old-state new-state event records output)]
                (dispatch-delay-event dispatch-events input)
                (dispatch-api-request api-requests output)
                (reset! snapshot new-state)
                (recur state records))
              (recur state records)))
          (do (log/infof "stop event loop for game[%s]" game-id)
              (swap! (:status game-handle) assoc :event-loop :stopped)
              (close! output)))))))
