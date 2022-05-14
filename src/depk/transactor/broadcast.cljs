(ns depk.transactor.broadcast
  (:require
   [cljs.core.async :as a]
   [depk.transactor.broadcast.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]))

(defn shrink-state
  [state]
  (let [status (:status state)]
    (case status
      ;; :game-status/play (dissoc state :prepare-cards)

      state)))

(defn start-broadcast-loop
  [broadcaster opts]
  (log/infof "🏁Start broadcaster for game[%s]" (:game-id opts))
  (let [game-id (:game-id opts)
        {:keys [ws-conn input snapshot game-account-snapshot]} broadcaster
        {:keys [chsk-send! connected-uids]} ws-conn]
    (a/go-loop [{:keys [type data]} (a/<! input)]

      (condp = type
        :system/broadcast-state
        (let [{:keys [state game-id game-account-state]} data]
          ;; Do not dispatch reset event.
          ;; (log/infof "🔈Broadcaster event: %s status: %s" (:this-event state) (:status state))
          (doseq [uid   (:any @connected-uids)
                  :when (= game-id (first uid))]
            (reset! snapshot state)
            (reset! game-account-snapshot game-account-state)
            ;; (log/infof "Send state to uid: %s" uid)
            (chsk-send! uid [:game/state (shrink-state state)])))

        :system/recover-state
        (do
          (log/infof "🔈Broadcaster state reset")
          (doseq [uid   (:any @connected-uids)
                  :when (= game-id (first uid))]
            (chsk-send! uid [:game/state-reset])))

        :noop)
      (recur (a/<! input)))))

(defrecord Broadcaster [ws-conn snapshot game-account-snapshot input])

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
  [ws-conn]
  (let [snapshot (atom nil)
        game-account-snapshot (atom nil)
        input    (a/chan 10)]
    (->Broadcaster ws-conn snapshot game-account-snapshot input)))

(defn get-snapshot
  [broadcaster]
  (p/-get-snapshot broadcaster))

(defn get-game-account-snapshot
  [broadcaster]
  (p/-get-game-account-snapshot broadcaster))
