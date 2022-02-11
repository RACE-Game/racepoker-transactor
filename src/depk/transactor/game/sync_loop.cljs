(ns depk.transactor.game.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async          :refer [go-loop <! >! timeout close!]]
   [depk.transactor.game.api :as api]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as game-handle]
   [taoensso.timbre          :as log]))

(defn start-game-state-sync
  "Fetch game state through chain API, send :system/sync-state event to game handle."
  [game-handle chain-api]
  (let [{:keys [game-id input snapshot]} game-handle]

    (log/infof "start state sync for game[%s]" game-id)

    (go-loop [prev nil]
      ;; only fetch state for init status
      (if (= :game-status/init (:status @snapshot))

        (let [state (<! (api/fetch-game-account chain-api game-id))
              {:keys [players]} state]
          (log/infof "fetch game state for game[%s]" game-id)
          (<! (timeout 1000))
          (when (not= prev state)
            (let [event (m/make-event :system/sync-state
                                      (game-handle/get-snapshot game-handle)
                                      {:players            players,
                                       :game-account-state state})]
              (>! input event)))
          (recur state))

        (do
          (<! (timeout 1000))
          (recur prev))))))
