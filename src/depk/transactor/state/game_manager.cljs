(ns depk.transactor.state.game-manager
  (:require [mount.core :as mount]
            [depk.transactor.game.manager :refer [make-game-manager]]
            [depk.transactor.state.api :as api]))

(mount/defstate game-manager
  :start
  (make-game-manager @api/chain-api @api/store-api)
  :stop
  nil)

(comment
  (keys (get @(:store (:store-api @game-manager)) nil))
  @(:snapshot (val (first @(:game-handle-map @game-manager)))))
