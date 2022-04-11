(ns depk.transactor.chain.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]))

(defn start-sync-loop
  "Fetch game state through chain API, emit :system/sync-state event to game handle."
  [chain-api game-id output]
  (log/infof "Start state sync loop for game[%s]" game-id)
  (a/go-loop [last-state nil]
    (a/<! (a/timeout 2000))
    (log/debugf "Fetch game account state, game[%s]" game-id)
    (let [game-account-state (try
                               (a/<! (p/-fetch-game-account chain-api game-id))
                               (catch js/Error _))]
      (when (and game-account-state (not= game-account-state last-state))
        (log/debugf "New game account state with game-no: %s"
                    (:game-no game-account-state))
        (a/>! output
              {:type :system/sync-state,
               :data {:game-account-state game-account-state}}))
      (recur game-account-state))))
