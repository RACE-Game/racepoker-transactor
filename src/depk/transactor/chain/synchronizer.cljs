(ns depk.transactor.chain.synchronizer
  (:require
   [cljs.core.async :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]
   [depk.transactor.game.models :as m]))

(defn start
  [chain-api game-id output init-state]
  (a/go-loop [buyin-serial (:buyin-serial init-state)]
    (let [state (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))]
      (when (and state (< buyin-serial (:buyin-serial state)))
        (log/infof "👀️Read game[%s] state, %s -> %s"
                   game-id
                   buyin-serial
                   (:buyin-serial state))
        (a/>! output
              {:type    :system/sync-state,
               :game-id game-id,
               :data
               {:game-account-state (m/parse-raw-game-account-state state)}}))

      (a/<! (a/timeout 3000))
      (recur (max buyin-serial (:buyin-serial state))))))

(defrecord Synchronizer [chain-api input output])

(extend-type Synchronizer
 ep/IAttachable
 (-input [_this]
   nil)
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   nil)

 ep/IComponent
 (-start [this opts]
   (let [{:keys [chain-api output]}   this
         {:keys [game-id init-state]} opts]
     (log/infof "🏁Start synchronizer for game[%s]" game-id)
     (start chain-api game-id output init-state))))

(defn make-synchronizer
  [chain-api]
  (let [input  (a/chan)
        output (a/chan)]
    (->Synchronizer chain-api input output)))
