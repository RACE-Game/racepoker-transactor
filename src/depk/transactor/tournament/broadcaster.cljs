(ns depk.transactor.tournament.broadcaster
  "Broadcast used to emit message to the websocket or other workers."
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

(defn start-tournament-broadcast-loop
  [broadcaster opts]
  (log/infof "🏁Start tournament broadcaster: %s" (:tournament-id opts))
  (let [{:keys [post-msg-fn input]} broadcaster]
    (a/go-loop [{:keys [type data], :as event} (a/<! input)]
      (if-not event
        ;; EXIT
        (log/infof "💤Tournament broadcast quit: %s" (:tournament-id opts))
        (do
          (log/infof "🔈Tournament broadcaster, event: %s" (:type event))
          (condp = type
            :system/tournament-broadcast
            (let [{:keys [state event]} data]
              ;; The state will be sent in Transit serialized
              ;; So the main thread doesn't have to unpack/pack it.
              (post-msg-fn (cond-> {:broadcast     :broadcast/tournament-state,
                                    :tournament-id (:tournament-id opts)}

                             state
                             (assoc :serialized-state (u/transit-write state))

                             event
                             (assoc :event event))))
            :noop)
          (recur (a/<! input)))))))

(defrecord TournamentBroadcaster [post-msg-fn snapshot input])

(extend-type TournamentBroadcaster
 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [_this]
   nil)

 (ep/-interest-event-types [_this]
   [:system/tournament-broadcast])

 ep/IComponent
 (ep/-start [this opts]
   (start-tournament-broadcast-loop this opts)))

(defn make-tournament-broadcaster
  [post-msg-fn]
  (let [snapshot (atom nil)
        input    (a/chan 10)]
    (->TournamentBroadcaster post-msg-fn snapshot input)))
