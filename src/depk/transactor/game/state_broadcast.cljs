(ns depk.transactor.game.state-broadcast
  (:require
   [cljs.core.async :refer [go-loop <! put! chan]]))

(defonce broadcast-ch (chan 64))

(defn watch-broadcast
  [k r o n]
  (let [{:keys [game-id]} n
        state (select-keys
               n
               [:game-id :game-no :status :street :player-map
                :pots :min-raise :street-bet :bet-map :action-player-id
                :showdown-map :prize-map :state-id :prepare-cards
                :shuffle-player-id :encrypt-player-id
                :btn :sb :bb :require-key-idents :share-key-map
                :card-ciphers :player-actions :winning-type])]
    (put! broadcast-ch [game-id state])))

(defn start-broadcast
  [game-handle]
  (add-watch (:snapshot game-handle) :broadcast watch-broadcast))

(defn attach-broadcast-handler
  [connected-uids chsk-send!]
  (go-loop [[game-id state :as evt] (<! broadcast-ch)]
    (doseq [[game-id :as uid] (:any @connected-uids)]
      (chsk-send! uid [:game/state state]))
    (recur (<! broadcast-ch))))
