(ns depk.transactor.chain.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]
   [clojure.set         :as set]
   [depk.transactor.constant :as c]))

(def empty-players (vec (repeat c/max-player-num nil)))

(defn start-sync-loop
  "Fetch game state through chain API, emit :system/sync-state event to game handle."
  [chain-api game-id output init-game-account-state]
  (log/infof "🏁Start state sync loop for game[%s]" game-id)
  (a/go-loop [last-state init-game-account-state]
    (a/<! (a/timeout 1000))
    ;; (log/infof "Fetch game account state, game[%s]" game-id)
    (let [game-account-state (try
                               (a/<! (p/-fetch-game-account chain-api game-id))
                               (catch js/Error _))]
      (when (and game-account-state (not= game-account-state last-state))
        (let [game-no        (:game-no game-account-state)
              old-players    (or (:players last-state) empty-players)
              new-players    (:players game-account-state)
              joined-players (map (fn [idx]
                                    (let [o (nth old-players idx)
                                          n (nth new-players idx)]
                                      (cond
                                        o        nil
                                        (nil? n) nil
                                        :else    n)))
                                  (range 9))]
          (log/infof "✨New game account state with game-no: %s, current game-no: %s"
                     game-no
                     (:game-no last-state))
          (log/infof "😀Joined players: %s" (map :pubkey joined-players))
          (a/>! output
                {:type :system/sync-state,
                 :data {:game-account-state game-account-state,
                        :joined-players     joined-players}})))
      (recur (or game-account-state last-state)))))
