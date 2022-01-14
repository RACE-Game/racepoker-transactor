(ns depk.transactor.game.event-loop
  (:require
   [cljs.core.async :as    a
                    :refer [go go-loop <! timeout close! put!]]
   [depk.transactor.game.event-handler :as event-handler]
   ["uuid" :as uuid]
   [taoensso.timbre :as log]
   [depk.transactor.util :refer [go-try <!?
                                 info warn log-group-collapsed log-group-end]]))

(defn expired-event!
  [state event]
  (ex-info "drop expired event" {:event event, :state-id (:state-id state)}))

(defn handle-event
  "Apply event to state, return async channel contains result map.

  Expired event will be dropped."
  [state event]
  (go
   (log-group-collapsed "handle event[%s]" (str (:type event)))
   (if (not= (:state-id state) (:state-id event))
     (do
       (info js/console "drop expired event[%s]" (:type event))
       {:result :err, :state state, :error (expired-event! state event)})
     (try
       (info "event:" event)
       (info "state:" state)
       (let [new-state-id (uuid/v4)
             new-state    (event-handler/handle-event (assoc state :state-id new-state-id) event)]
         (if (map? new-state)
           (do
             (info "sync result:" new-state)
             {:result :ok, :state new-state})
           (let [v (<! new-state)]
             (if (map? v)
               (do
                 (info "async result:" v)
                 {:result :ok, :state v})
               (do
                 (warn "error:" (ex-message v))
                 {:result :err, :state state, :error v})))))
       (catch ExceptionInfo e
         (warn e)
         {:result :err, :state state, :error e})
       (finally (log-group-end))))))

(defn dispatch-delay-event
  "dispatch delayed event"
  [dispatch-events input]
  (doseq [[ts event] dispatch-events]
    (when event
      (log/debugf "dispatch event[%s] at time[%s]" (str (:type event)) (str (js/Date. ts)))
      (go-try
       (<!? (timeout ts))
       (put! input event)))))

(defn dispatch-api-request
  "dispatch api requests"
  [api-requests output]
  (doseq [req api-requests]
    (when req
      (log/debugf "dispatch api request[%s]" (prn-str req))
      (a/put! output req))))

(defn start-event-loop
  [game-handle]
  (let [{:keys [snapshot input output game-id]} game-handle]
    (log/infof "start event loop for game[%s]" game-id)
    (go-loop [state @snapshot]
      (let [event (<! input)]
        (if event
          (let [{:keys [result state]} (<! (handle-event state event))
                {:keys [dispatch-events api-requests]} state
                ;; flush dispatch-events & api-requests
                new-state (dissoc state :dispatch-events :api-requests)]
            (when (= result :ok)
              (dispatch-delay-event dispatch-events input)
              (dispatch-api-request api-requests output)
              (reset! snapshot new-state))
            (recur new-state))
          (do (log/infof "stop event loop for game[%s]" game-id)
              (swap! (:status game-handle) assoc :event-loop :stopped)
              (close! output)))))))
