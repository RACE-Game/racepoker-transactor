(ns depk.transactor.chain.transact-loop
  "Sending transaction to blockchain."
  (:require
   [cljs.core.async :as a]
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.chain.misc :as misc]))

(defn start-transact-loop
  "Send transact through chain API, emit :system/transaction-succeed or :system/transaction-failed
  when complete."
  [chain-api game-id input output]
  (log/infof "🏁Start transact loop for game[%s]" game-id)
  (a/go-loop [{:keys [type data]} (a/<! input)]
    ;; (log/debugf "Transact loop receive: %s" type)
    (let [{:keys [confirming pending]} chain-api]
      (condp = type
        :system/settle-finished
        (p/-settle-finished-game chain-api
                                 game-id
                                 (:chips-change-map data)
                                 (:player-status-map data))

        :system/settle-failed
        (p/-settle-failed-game chain-api game-id (:player-status-map data))

        :noop))
    (recur (a/<! input))))
