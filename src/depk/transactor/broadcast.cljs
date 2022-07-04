(ns depk.transactor.broadcast
  "Broadcast used to emit message to the websocket or other workers.

  Available broadcasters:
      - GameBroadcaster, used for normal games
      - TournamentBroadcaster, used for tournament
      - TournamentGameBroadcaster, used for the games in a tournament
  "
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.broadcast.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

;; Game broadcaster

(defn start-game-broadcast-loop
  [broadcaster opts]
  (log/infof "🏁Start broadcaster for game[%s]" (:game-id opts))
  (let [{:keys [post-msg-fn input]} broadcaster]
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
              ;; The state will be sent in Transit serialized
              ;; So the main thread doesn't have to unpack/pack it.
              (post-msg-fn {:broadcast        :broadcast/game-event,
                            :game-id          game-id,
                            :serialized-state (u/transit-write state),
                            :player-ids       (keys (:player-map state)),
                            :start-time       (:start-time state),
                            :message          [:game/event event]}))
            :noop)
          (recur (a/<! input)))))))

(defrecord GameBroadcaster [post-msg-fn snapshot input])

(extend-type GameBroadcaster
 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   nil)

 (ep/-interest-event-types [this]
   [:system/broadcast-state])

 ep/IComponent
 (ep/-start [this opts]
   (start-game-broadcast-loop this opts)))

(defn make-game-broadcaster
  [post-msg-fn]
  (let [snapshot (atom nil)
        input    (a/chan 10)]
    (->GameBroadcaster post-msg-fn snapshot input)))

;; Tournament broadcaster

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
