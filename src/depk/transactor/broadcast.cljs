(ns depk.transactor.broadcast
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.broadcast.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

(defn start-broadcast-loop
  [broadcaster opts]
  (log/infof "🏁Start broadcaster for game[%s]" (:game-id opts))
  (let [{:keys [post-msg-fn input snapshot]} broadcaster]
    (a/go-loop [{:keys [type data], :as event} (a/<! input)]
      (if-not event
        ;; EXIT
        (log/infof "💤Broadcast quit for game[%s]" (:game-id opts))
        (do
          (condp = type
            :system/broadcast-state
            (let [{:keys [state game-id event]} data]
              ;; Do not dispatch reset event.
              (log/infof "🔈Broadcaster event: %s status: %s" (:this-event state) (:status state))
              (reset! snapshot state)
              ;; The state will be sent in Transit serialized
              ;; So the main thread doesn't have to unpack/pack it.
              (post-msg-fn {:game-id          game-id,
                            :serialized-state (u/transit-write state),
                            :player-ids       (keys (:player-map state)),
                            :start-time       (:start-time state),
                            :message          [:game/event event]}))
            :noop)
          (recur (a/<! input)))))))

(defrecord Broadcaster [post-msg-fn snapshot game-account-snapshot input])

(extend-type Broadcaster
 p/IBroadcaster

 (p/-get-snapshot [this]
   @(:snapshot this))

 (p/-get-game-account-snapshot [this]
   @(:game-account-snapshot this))

 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   nil)

 (ep/-interest-event-types [this]
   [:system/broadcast-state
    :system/recover-state])

 ep/IComponent
 (ep/-start [this opts]
   (start-broadcast-loop this opts)))

(defn make-broadcaster
  [post-msg-fn]
  (let [snapshot (atom nil)
        game-account-snapshot (atom nil)
        input    (a/chan 10)]
    (->Broadcaster post-msg-fn snapshot game-account-snapshot input)))

(defn get-snapshot
  [broadcaster]
  (p/-get-snapshot broadcaster))

(defn get-game-account-snapshot
  [broadcaster]
  (p/-get-game-account-snapshot broadcaster))
