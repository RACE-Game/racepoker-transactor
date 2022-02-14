(ns depk.transactor.game.event-loop
  (:require
   [cljs.core.async :as    a
                    :refer [go go-loop <! timeout close! put!]]
   [depk.transactor.game.event-handler :as event-handler]
   ["uuid" :as uuid]
   [depk.transactor.log :as log]
   [depk.transactor.util :refer [go-try <!?]]))

(def non-expire-event-types
  "A list of event types, those should never be considered as expired."
  #{:client/leave :client/release})

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
       (log/debugf "Drop expired event[%s]" (name (:type event)))
       {:result :err, :state state, :error (expired-event! state event)})
     (try
       (let [new-state-id (if (get non-expire-event-types (:type event))
                            (:state-id event)
                            (uuid/v4))
             new-state    (event-handler/handle-event (assoc state :state-id new-state-id) event)]
         (if (map? new-state)
           (do
             (log/debugf "Handle event [%s], status: %s -> %s"
                         (str (:type event))
                         (str (:status state))
                         (str (:status new-state)))
             {:result :ok, :state new-state})
           (let [v (<! new-state)]
             (if (map? v)
               (do
                 (log/debugf "Handle event [%s], status: %s -> %s"
                             (str (:type event))
                             (str (:status state))
                             (str (:status v)))
                 {:result :ok, :state v})
               (do
                 (log/warnf "Error(async) from event handler: %s" (ex-message v))
                 ;; (.warn js/console "Error data: " (ex-data v))
                 {:result :err, :state state, :error v})))))
       (catch ExceptionInfo e
         (log/warnf "Error(sync) in event loop: %s" (ex-message e))
         ;; (.warn js/console "Error data: " (ex-data e))
         {:result :err, :state state, :error e})))))

(defn dispatch-delay-event
  "Dispatch dispatch-events to input channel.

  dispatch-events is a map from timeout millis to event."
  [event dispatch-event input]
  (when dispatch-event
    (let [[ms evt] dispatch-event]
      (log/debugf "Event[%s] dispatch event[%s] after %sms"
                  (str (:type event))
                  (str (:type evt))
                  (str ms))
      (go-try
       (<!? (timeout ms))
       (put! input evt)))))

(defn dispatch-api-request
  "Dispatch api-requests to output channel."
  [event api-requests output]
  (doseq [req api-requests]
    (when req
      (log/debugf "Event[%s] dispatch api request[%s]" (str (:type event)) (prn-str req))
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
    (log/infof "Start event loop for game[%s]" game-id)
    (go-loop [state   @snapshot
              records []]
      (let [event (<! input)]
        (if event
          (let [old-state state
                {:keys [result state]} (<! (handle-event state event))
                {:keys [dispatch-event api-requests]} state
                ;; flush dispatch-events & api-requests
                new-state (dissoc state :dispatch-event :api-requests)]
            (if (= result :ok)
              (let [records
                    (collect-and-dispatch-game-history old-state new-state event records output)]
                (dispatch-delay-event event dispatch-event input)
                (dispatch-api-request event api-requests output)
                (reset! snapshot new-state)
                ;; (js/console.log "new-state: " new-state)
                (recur new-state records))
              (recur old-state records)))
          (do (log/infof "Stop event loop for game[%s]" game-id)
              (swap! (:status game-handle) assoc :event-loop :stopped)
              (close! output)))))))
