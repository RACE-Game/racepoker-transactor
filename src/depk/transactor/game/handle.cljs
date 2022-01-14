(ns depk.transactor.game.handle
  "Storage for game state."
  (:require
   [clojure.core.async :as    a
                       :refer [go >! chan close!]]))

(defrecord GameHandle [game-id input output snapshot status])

(defn make-game-handle
  [game-id init-game-state]
  (let [input    (chan)
        output   (chan)
        snapshot (atom init-game-state)
        status   (atom {:event-loop :running,
                        :sync-loop  :running})]
    (->GameHandle game-id input output snapshot status)))

(defn shutdown-game-handle
  [game-handle]
  (let [{:keys [input]} game-handle]
    (close! input)))

(defn game-handle?
  [x]
  (instance? GameHandle x))

(defn send-event
  [game-handle event]
  (go
   (let [input (:input game-handle)]
     (>! input event))))

(defn get-snapshot
  [game-handle]
  @(:snapshot game-handle))

(defn get-status
  [game-handle]
  @(:status game-handle))
