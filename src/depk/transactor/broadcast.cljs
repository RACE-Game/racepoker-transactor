(ns depk.transactor.broadcast
  (:require
   [cljs.core.async :as a]
   [depk.transactor.broadcast.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]))

(defn start-broadcast-loop
  [broadcaster opts]
  (log/debugf "Start broadcaster for game[%s]" (:game-id opts))
  (let [{:keys [ws-conn input snapshot]}    broadcaster
        {:keys [chsk-send! connected-uids]} ws-conn]
    (a/go-loop [{:keys [type data]} (a/<! input)]
      ;; (log/debugf "Broadcaster receive: %s" type)
      (condp = type
        :system/broadcast-state
        (let [{:keys [state game-id]} data]
          (log/debugf "Broadcast state, game-id: %s, state-id: %s, this-event: %s"
                      game-id
                      (:state-id state)
                      (:this-event state))
          ;; (js/console.debug "state: " state)
          (doseq [uid   (:any @connected-uids)
                  :when (= game-id (first uid))]
            (reset! snapshot state)
            ;; (log/debugf "Send state to uid: %s" uid)
            (chsk-send! uid [:game/state state])))
        :noop)
      (recur (a/<! input)))))

(defrecord Broadcaster [ws-conn snapshot input])

(extend-type Broadcaster
 p/IBroadcaster

 (p/-get-snapshot [this]
   @(:snapshot this))

 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   nil)

 ep/IComponent
 (ep/-start [this opts]
   (start-broadcast-loop this opts)))

(defn make-broadcaster
  [ws-conn]
  (let [snapshot (atom nil)
        input    (a/chan 10)]
    (->Broadcaster ws-conn snapshot input)))

(defn get-snapshot
  [broadcaster]
  (p/-get-snapshot broadcaster))
