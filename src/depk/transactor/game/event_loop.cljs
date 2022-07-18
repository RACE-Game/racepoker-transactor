(ns depk.transactor.game.event-loop
  (:require
   [cljs.core.async :as    a
                    :refer [go go-loop <! timeout close! put!]]
   [depk.transactor.game.event-handler :as event-handler]
   [depk.transactor.log :as log]
   [depk.transactor.game.models :refer [game-state->resp make-event]]
   [depk.transactor.event.protocol :as ep]))

(def event-list
  #{:system/sync-state
    :system/reset
    :system/start-game
    :system/start-tournament-game
    :client/shuffle-cards
    :client/encrypt-cards
    :client/share-keys
    :system/key-share-timeout
    :system/shuffle-timeout
    :system/encrypt-timeout
    :system/player-action-timeout
    :client/ready
    :client/fix-keys
    :system/alive
    :system/dropout
    :client/leave
    :player/fold
    :player/call
    :player/check
    :player/raise
    :player/bet
    ;; Only for tournament games
    :system/next-game
    :system/resit-table})

(defn handle-result
  "Handle the result of event handler.

  Flush :dispatch-event, :api-requests
  Also set :this-event."
  [event {:keys [state], :as res}]
  (let [{:keys [dispatch-event reserve-timeout api-requests overwrite-this-event]}
        state
        state (cond-> (dissoc state
                              :dispatch-event
                              :api-requests
                              :reserve-timeout
                              :overwrite-this-event)

                (not reserve-timeout)
                (assoc :timeout-event nil)

                dispatch-event
                (assoc :timeout-event
                       [(+ (.getTime (js/Date.)) (first dispatch-event))
                        (second dispatch-event)])

                true
                (assoc :this-event
                       (or overwrite-this-event
                           (:type event))))]
    (assoc res
           :state        state
           :api-requests api-requests)))

(defn pre-process-state
  [state]
  (-> state
      (assoc :state-id (str (random-uuid)))
      (dissoc :display)))

(defn handle-event
  "Apply event to state, return a channel that contains the result map.

  The result map contains:
    * result, :ok | :err
    * state, the new state if succeed, the original state otherwise
    * (optional) api-requests, requests to send to chain api and store api.
    * (optional) error, an Exception

  An event with an invalid state-id is considered to be expired."
  [state event]
  (go
   (try
     (let [new-state (-> state
                         (pre-process-state)
                         (event-handler/handle-event event))]
       (if (map? new-state)
         (handle-result event {:result :ok, :state new-state})
         (let [v (<! new-state)]
           (if (map? v)
             (handle-result event {:result :ok, :state v})
             {:result :err, :state state, :error v}))))
     (catch ExceptionInfo e
       {:result :err, :state state, :error e})
     (catch js/Error e
       (log/log "‼️" (:game-id state) "Error when handling Event[%s]" (:type event))
       (js/console.error e)
       {:result :err, :state state, :error e}))))

(defn dispatch-api-request
  "Dispatch api-requests to output channel."
  [game-id event api-requests output]
  (doseq [req api-requests]
    (when req
      (log/log "✨"
               game-id
               "Event[%s] dispatch api request: %s"
               (:type event)
               req)
      (put! output req))))

(defn collect-and-dispatch-game-history
  "Collect and (maybe) dispatch game history to output channel,
  return the new records."
  [old-state new-state event records output]
  ;; (if (= (:game-no old-state) (:game-no new-state))
  ;;   (conj records [event new-state])
  ;;   (do
  ;;     (when (:game-no old-state)
  ;;       (put! output
  ;;             {:type :system/save-game-history,
  ;;              :data {:game-id (:game-id old-state),
  ;;                     :game-no (:game-no old-state),
  ;;                     :records records}}))
  ;;     []))
)

(defn dispatch-broadcast-state
  [game-id event state output]
  (put! output
        {:type :system/broadcast-state,
         :data {:game-id game-id,
                :state   (game-state->resp state),
                :event   (assoc event :state-id (:state-id state))}}))

(defn take-event
  [input state]
  (go
   (let [{:keys [timeout-event]} state
         [to to-evt] timeout-event
         ms (- (or to 0) (.getTime (js/Date.)))]

     (if (pos? ms)
       (let [to-ch      (a/timeout ms)
             _ (log/log "⌛" (:game-id state) "Timeout %sms event %s" ms (name (:type to-evt)))
             [val port] (a/alts! [to-ch input])]

         (if (= port to-ch)
           ;; Fix the dispatch-id of timeout event
           (assoc to-evt :dispatch-id (:state-id state))
           val))
       (a/<! input)))))

(defn run-event-loop
  [game-id init-state input output]

  ;; Put initial event
  (go (a/>! input (make-event :system/reset init-state)))
  (go-loop [state   init-state
            records []]
    (if-let [event (<! (take-event input state))]
      (if (event-list (:type event))
        (let [;; Set current time as timestamp
              event     (assoc event :timestamp (.getTime (js/Date.)))
              old-state state
              {:keys [result state api-requests error]}
              (<! (handle-event state event))]

          ;; Log the event
          (log/log (case result
                     :ok      "🟩"
                     :err     "🟥"
                     :expired "🟨")
                   game-id
                   "Event[%s] from %s. %s"
                   (:type event)
                   (if-let [pid (:player-id event)]
                     (str "Player[" pid "]")
                     "system")
                   (if-not error
                     (str "Status:" (name (:status old-state)) "->" (name (:status state)))
                     (str "Err: " (ex-message error))))

          (if (= result :ok)
            (let [records
                  (collect-and-dispatch-game-history old-state state event records output)]
              (dispatch-api-request game-id event api-requests output)
              (dispatch-broadcast-state game-id event state output)
              (recur state records))
            (recur old-state records)))
        (recur state records))
      ;; EXIT
      (log/log "💤" game-id "Event loop quit"))))

(defrecord EventLoop [input output])

(extend-type EventLoop
 ep/IAttachable

 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   (:output this))

 (ep/-interest-event-types
   [_this]
   (vec event-list))

 ep/IComponent
 (ep/-start [this opts]
   (log/log "🎉" (:game-id opts) "Start event loop")
   (run-event-loop (:game-id opts)
                   (:init-state opts)
                   (:input this)
                   (:output this))))

(defn make-event-loop
  []
  (let [input  (a/chan 10)
        output (a/chan 10)]
    (->EventLoop input output)))
