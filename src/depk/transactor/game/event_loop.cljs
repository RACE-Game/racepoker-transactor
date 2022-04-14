(ns depk.transactor.game.event-loop
  (:require
   [cljs.core.async :as    a
                    :refer [go go-loop <! timeout close! put!]]
   [depk.transactor.game.event-handler :as event-handler]
   ["uuid" :as uuid]
   [depk.transactor.log :as log]
   [depk.transactor.game.models :refer [game-state->resp]]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.util :refer [go-try <!?]]))

(def event-list
  #{:system/sync-state
    :system/reset
    :system/start-game
    :client/shuffle-cards
    :client/encrypt-cards
    :client/share-keys
    :system/key-share-timeout
    :system/shuffle-timeout
    :system/encrypt-timeout
    :system/player-action-timeout
    :client/alive
    :client/leave
    :client/release
    :player/fold
    :player/call
    :player/check
    :player/raise
    :player/bet})

(defn expired-event!
  [state event]
  (throw (ex-info "Expired event"
                  {:event    event,
                   :state-id (:state-id state)})))

(defn expired-event?
  [state event]
  (not (or
        (nil? (:dispatch-id event))
        (= (:state-id state) (:dispatch-id event))
        (= (:dispatch-id state) (:dispatch-id event)))))

(defn handle-result
  "Handle the result of event handler.

  Flush :dispatch-event, :api-requests and :reserve-dispatch-id
  Set :dispatch-id and :this-event."
  [event {:keys [state], :as res}]
  (let [{:keys [dispatch-event reserve-dispatch-id api-requests state-id]} state
        state (cond-> (dissoc state :dispatch-event :api-requests :reserve-dispatch-id)
                (not reserve-dispatch-id)
                (assoc :dispatch-id state-id)

                true
                (assoc :this-event (:type event)))]
    (assoc res
           :state          state
           :dispatch-event dispatch-event
           :api-requests   api-requests)))

(defn handle-event
  "Apply event to state, return a channel that contains the result map.

  The result map contains:
    * result, :ok | :err
    * state, the new state if succeed, the original state otherwise
    * (optional) api-requests, requests to send to chain api and store api.
    * (optional) dispatch-event, [ms event], a event to dispatch with ms as delay
    * (optional) error, an Exception

  An event with an invalid state-id is considered to be expired."
  [state event]
  (go (try
        ;; Check event expired?
        (when (expired-event? state event)
          (expired-event! state event))

        ;; Process event
        (let [new-state-id (uuid/v4)
              new-state    (event-handler/handle-event (assoc state :state-id new-state-id) event)]
          (if (map? new-state)
            (do
              (log/debugf "Handle event success: %s" (:type event))
              ;; (js/console.debug "state: " new-state)
              (handle-result event {:result :ok, :state new-state}))
            (let [v (<! new-state)]
              (if (map? v)
                (do
                  (log/debugf "Handle event success %s" (:type event))
                  ;; (js/console.debug "state: " v)
                  (handle-result event {:result :ok, :state v}))
                (do
                  (log/debugf "Error in event handler: %s, event: %s"
                              (ex-message v)
                              (prn-str event))
                  {:result :err, :state state, :error v})))))

        (catch ExceptionInfo e
          (log/debugf "Error in event handler: %s, event: %s"
                      (ex-message e)
                      (prn-str event))
          {:result :err, :state state, :error e})
        (catch js/Error e
          (log/errorf "Error in event handler: %s, event: %s"
                      (ex-message e)
                      (prn-str event))
          {:result :err, :state state, :error e}))))

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
              {:type :system/save-game-history,
               :data {:game-id (:game-id old-state),
                      :game-no (:game-no old-state),
                      :records records}}))
      [])))

(defn dispatch-broadcast-state
  [game-id state output]
  (put! output
        {:type :system/broadcast-state,
         :data {:game-id game-id,
                :state   (game-state->resp state)}}))

(defn run-event-loop
  [game-id init-state input output]
  (log/infof "Start event loop for game[%s]" game-id)
  (go-loop [state   init-state
            records []]
    (let [event (<! input)]
      ;; (log/debugf "Event loop receive: %s" (:type event))
      (if (event-list (:type event))
        (let [old-state state

              {:keys [result state api-requests dispatch-event]}
              (<! (handle-event state event))]
          ;; (log/debugf "Handle event: %s, result: %s" (:type event) result)
          (if (= result :ok)
            (let [records
                  (collect-and-dispatch-game-history old-state state event records output)]

              ;; (.info js/console "event:" event)
              ;; (.info js/console "before:" old-state)
              ;; (.info js/console "after:" state)

              (dispatch-delay-event event dispatch-event input)
              (dispatch-api-request event api-requests output)
              (dispatch-broadcast-state game-id state output)
              (recur state records))
            (recur old-state records)))
        (recur state records)))))

(defrecord EventLoop [input output])

(extend-type EventLoop
 ep/IAttachable

 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   (:output this))

 ep/IComponent
 (ep/-start [this opts]
   (run-event-loop (:game-id opts)
                   (:init-state opts)
                   (:input this)
                   (:output this))))

(defn make-event-loop
  []
  (let [input  (a/chan 10)
        output (a/chan 10)]
    (->EventLoop input output)))
