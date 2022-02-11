(ns depk.transactor.game.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async          :refer [go-loop <! >! timeout close!]]
   [depk.transactor.game.api :as api]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as game-handle]
   [depk.transactor.log          :as log]))

(defn start-game-state-sync
  "Fetch game state through chain API, send :system/sync-state event to game handle."
  [game-handle chain-api]
  (let [{:keys [game-id input snapshot]} game-handle]

    (log/infof "start state sync for game[%s]" game-id)

    (go-loop []
      (<! (timeout 1000))
      (let [{:keys [status game-no]} @snapshot]
        (when (= :game-status/init status)
          (log/infof "fetch game state for game[%s]" game-id)
          (let [{:keys [players], :as state} (<! (api/fetch-game-account chain-api game-id))]
            (when (not= game-no (:game-no state))
              (let [event (m/make-event :system/sync-state
                                        (game-handle/get-snapshot game-handle)
                                        {:players            players,
                                         :game-account-state state})]
                (>! input event)))))
        (recur)))))
