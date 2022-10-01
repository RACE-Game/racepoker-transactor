(ns depk.transactor.game.broadcaster
  "Broadcast used to emit message to the websocket or other workers.

  Available broadcasters:
      - GameBroadcaster, used for normal games
      - TournamentBroadcaster, used for tournament
      - TournamentGameBroadcaster, used for the games in a tournament
  "
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

;; Game broadcaster

(defn start-game-broadcast-loop
  [broadcaster opts]
  (let [{:keys [post-msg-fn input]}  broadcaster
        {:keys [init-state game-id]} opts]
    (log/log "🎉" game-id "Start game broadcaster")
    ;; Initialize the snapshot
    (post-msg-fn {:broadcast        :broadcast/game-event,
                  :game-id          (:game-id init-state),
                  :serialized-state (u/transit-write init-state),
                  :player-ids       (keys (:player-map init-state)),
                  :start-time       (:start-time init-state)})

    (a/go-loop [{:keys [type data], :as event} (a/<! input)
                last-state-id (:state-id init-state)]
      (if-not event
        ;; EXIT
        (log/log "💤" game-id "Broadcast quit")
        (condp = type
          :system/broadcast-state
          (let [{:keys [state game-id event]} data]
            (log/log "🔈" game-id "Broadcaster receives Event[%s]" (:this-event state))
            (log/log "🔈"
                     game-id
                     "Broadcaster last id: %s, this id: %s"
                     last-state-id
                     (:state-id state))
            ;; The state will be sent in Transit serialized
            ;; So the main thread doesn't have to unpack/pack it.
            (post-msg-fn {:broadcast        :broadcast/game-event,
                          :game-id          game-id,
                          :serialized-state (u/transit-write state),
                          :player-ids       (keys (:player-map state)),
                          :start-time       (:start-time state),
                          :event            [:game/event
                                             (assoc event
                                                    :last-state-id
                                                    last-state-id)]})
            (recur (a/<! input) (:state-id state)))

          ;; default
          (recur (a/<! input) last-state-id))))))

(defrecord GameBroadcaster [post-msg-fn snapshot input])

(extend-type GameBroadcaster
 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [_this]
   nil)

 (ep/-interest-event-types [_this]
   [:system/broadcast-state])

 ep/IComponent
 (ep/-start [this opts]
   (start-game-broadcast-loop this opts)))

(defn make-game-broadcaster
  [post-msg-fn]
  (let [snapshot (atom nil)
        input    (a/chan 10)]
    (->GameBroadcaster post-msg-fn snapshot input)))
