(ns depk.transactor.tournament-game
  "Commit game events to Arweave and Solana."
  (:require
   [depk.transactor.util        :refer [go-try]]
   [depk.transactor.game.models :as m]
   [depk.transactor.tournament-game.worker :as worker]
   [depk.transactor.worker-manager :as manager]
   [depk.transactor.log         :as log]))

(defn start-tournament-game
  "Start a tournament game.

  start-opts:
      - game-id
      - tournament-id
      - players
      - size
      - report-fn
  "
  [worker-manager start-opts]
  (manager/try-start worker-manager (:game-id start-opts) worker/make-worker start-opts))


(defn send-tournament-event
  "Update game account state.

  This will trigger the next game."
  [worker-manager game-id event]
  (let [worker (manager/find-worker worker-manager game-id)]
    (worker/send-event worker event)))
