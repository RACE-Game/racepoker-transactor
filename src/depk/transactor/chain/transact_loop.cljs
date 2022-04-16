(ns depk.transactor.chain.transact-loop
  "Sending transaction to blockchain."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]))

(defn start-transact-loop
  "Send transact through chain API, emit :system/transaction-succeed or :system/transaction-failed
  when complete."
  [chain-api game-id input output]
  (log/infof "🏁Start transact loop for game[%s]" game-id)
  (a/go-loop [{:keys [type data]} (a/<! input)]
    ;;; FIXME: Introduce event merging
    (condp = type
      :system/settle-finished
      (a/<! (p/-settle-finished-game chain-api
                                     game-id
                                     (:chips-change-map data)
                                     (:player-status-map data)
                                     (:expected-player-map data)))

      :system/settle-failed
      (a/<! (p/-settle-failed-game chain-api
                                   game-id
                                   (:player-status-map data)
                                   (:expected-player-map data)))

      :system/set-winner
      (a/<! (p/-set-winner chain-api
                           game-id
                           (:winner-id data)))

      :noop)
    (recur (a/<! input))))
