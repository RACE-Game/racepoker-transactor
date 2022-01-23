(ns depk.transactor.game.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async          :refer [go-loop <! >! timeout close!]]
   [depk.transactor.game.api :as api]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as game-handle]
   [taoensso.timbre          :as log]))

(defn start-game-state-sync
  "Fetch game state through chain API, send :system/sync-state event to game handle.

  Exit and close game handle's input chan when there's no player in game account."
  [game-handle chain-api]
  (let [{:keys [game-id input]} game-handle]
    (log/infof "start state sync for game[%s]" game-id)
    (go-loop [prev nil]
      (let [state      (<! (api/fetch-game-account chain-api game-id))
            {:keys [players]} state
            player-cnt (count (filter some? players))]
        (if (pos? player-cnt)
          (do (when (not= prev state)
                (let [event (m/make-event :system/sync-state
                                          (game-handle/get-snapshot game-handle)
                                          {:players            players,
                                           :game-account-state state})]
                  (>! input event)))
              (<! (timeout 1000))
              (recur state))
          (do (log/infof "stop state sync for game[%s]" game-id)
              (swap! (:status game-handle) assoc :sync-loop :stopped)
              (close! input)))))))
