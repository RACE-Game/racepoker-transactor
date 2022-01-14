(ns depk.transactor.state.game-manager
  (:require [mount.core :as mount]
            [depk.transactor.game.manager :refer [make-game-manager]]
            [depk.transactor.state.api :as api]))

(mount/defstate game-manager
  :start
  (make-game-manager @api/chain-api)
  :stop
  nil)
