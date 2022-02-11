(ns depk.transactor.game.api-transport
  (:require
   [cljs.core.async          :refer [<! go-loop]]
   [depk.transactor.game.api :refer [settle-finished-game
                                     settle-failed-game
                                     save-game-history]]
   [depk.transactor.log          :as log]))

(defn start-api-transport
  [game-handle chain-api store-api]
  (let [{:keys [output game-id]} game-handle]
    (log/infof "start api transport for game[%s]" game-id)
    (log/infof "chain api: %s" (prn-str chain-api))
    (log/infof "store api: %s" (prn-str store-api))
    (go-loop [req (<! output)]
      (log/infof "API request[%s]" (name (:api-request/type req)))
      (case (:api-request/type req)
        :settle-finished-game
        (let [{:keys [chips-change-map player-status-map]} req]
          (settle-finished-game chain-api game-id chips-change-map player-status-map)
          (recur (<! output)))

        :settle-failed-game
        (let [{:keys [player-status-map]} req]
          (settle-failed-game chain-api game-id player-status-map)
          (recur (<! output)))

        :save-game-history
        (let [{:keys [game-id game-no records]} req]
          (save-game-history store-api game-id game-no records)
          (recur (<! output)))

        (log/infof "stop api transport for game[%s]" game-id)))))
