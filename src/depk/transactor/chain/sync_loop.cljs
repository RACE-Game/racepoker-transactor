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
  (a/go-loop [last-game-no (:game-no init-game-account-state)]
    (a/<! (a/timeout 5000))
    (let [game-account-state (try
                               (a/<! (p/-fetch-game-account chain-api game-id))
                               (catch js/Error _))]
      (when-let [game-no (:game-no game-account-state)]
        (log/infof "Sync game state, game-no: %s" game-no)
        ;; Detect rollback - current game-no is less than previous game-no
        (cond
          ;; Rollback
          (or (nil? last-game-no) (< game-no last-game-no))
          (a/>! output
                {:type    :system/recover-state,
                 :game-id game-id,
                 :data    {:game-account-state game-account-state}})

          ;; Normal
          (> game-no last-game-no)
          (a/>! output
                {:type    :system/sync-state,
                 :game-id game-id,
                 :data    {:game-account-state game-account-state}})

          :else
          :noop))

      (recur (or (:game-no game-account-state) last-game-no)))))
