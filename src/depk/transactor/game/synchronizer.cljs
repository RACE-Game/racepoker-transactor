(ns depk.transactor.game.synchronizer
  (:require
   [cljs.core.async :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]
   [depk.transactor.game.models :as m]))

(def sync-times 8)
(def sync-buf-lens 2)

(defn empty-game?
  [state]
  (every? nil? (:players state)))

(defn start
  [chain-api game-id input output init-state]
  (a/go-loop [buyin-serial (:buyin-serial init-state)
              quit-count   0]
    (log/log "👀️" game-id "Synchronizer progress: %s/%s" quit-count sync-times)
    (let [state (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))]
      (cond
        (nil? state)
        ;; Invalid response, retry
        (do
          (a/<! (a/timeout 3000))
          (recur (max buyin-serial (:buyin-serial state)) quit-count))

        ;; Pause
        (>= quit-count sync-times)
        (do (log/log "💤" game-id "Synchronizer paused")
            (when (a/<! input)
              (recur buyin-serial 0)))

        ;; New buyin
        (< buyin-serial (:buyin-serial state))
        (do
          (log/log "👀️"
                   game-id
                   "Synchronizer get new game state, %s -> %s"
                   buyin-serial
                   (:buyin-serial state))
          (a/>! output
                {:type    :system/sync-state,
                 :game-id game-id,
                 :data
                 {:game-account-state (m/parse-raw-game-account-state state)}})
          (a/<! (a/timeout 500))
          (recur (max buyin-serial (:buyin-serial state)) (inc quit-count)))

        ;; Refetch
        :else
        (do
          (a/<! (a/timeout 3000))
          (recur (max buyin-serial (:buyin-serial state)) (inc quit-count)))))))

(defrecord Synchronizer [chain-api input output])

(extend-type Synchronizer
 ep/IAttachable
 (-input [this]
   (:input this))
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   [:system/start-synchronizer])

 ep/IComponent
 (-start [this opts]
   (let [{:keys [chain-api input output]} this
         {:keys [game-id init-state]} opts]
     (log/log "🎉" game-id "Start synchronizer")
     (start chain-api game-id input output init-state))))

(defn make-synchronizer
  [chain-api]
  (let [input  (a/chan (a/dropping-buffer sync-buf-lens))
        output (a/chan)]
    (->Synchronizer chain-api input output)))
