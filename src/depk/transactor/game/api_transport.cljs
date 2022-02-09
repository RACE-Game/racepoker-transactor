(ns depk.transactor.game.api-transport
  (:require
   [cljs.core.async          :refer [<! go-loop]]
   [depk.transactor.game.api :refer [settle-finished-game
                                     settle-failed-game]]
   [taoensso.timbre          :as log]))

(defn start-api-transport
  [game-handle chain-api]
  (let [{:keys [output game-id]} game-handle]
    (log/infof "start api transport for game[%s]" game-id)
    (go-loop [req (<! output)]
      (log/infof "API request[%s] %s" (name (:api-request/type req)) (prn-str req))
      (case (:api-request/type req)
        :settle-finished-game
        (let [{:keys [chips-change-map player-status-map]} req]
          (settle-finished-game chain-api game-id chips-change-map player-status-map)
          (recur (<! output)))

        :settle-failed-game
        (let [{:keys [player-status-map]} req]
          (settle-failed-game chain-api game-id player-status-map)
          (recur (<! output)))

        (log/infof "stop api transport for game[%s]" game-id)))))
