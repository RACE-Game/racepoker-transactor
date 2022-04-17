(ns depk.transactor.chain.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]
   [clojure.set         :as set]
   [depk.transactor.constant :as c]))


(defn start-sync-loop
  "Fetch game state through chain API, emit :system/sync-state event to game handle."
  [chain-api game-id output init-game-account-state]
  (log/infof "🏁Start state sync loop for game[%s]" game-id)
  (a/go-loop []
    (a/<! (a/timeout 5000))
    ;; (log/infof "Fetch game account state, game[%s]" game-id)
    (let [game-account-state (try
                               (a/<! (p/-fetch-game-account chain-api game-id))
                               (catch js/Error _))]
      (let [game-no (:game-no game-account-state)]
        (a/>! output
              {:type    :system/sync-state,
               :game-id game-id,
               :data    {:game-account-state game-account-state}}))
      (recur))))
