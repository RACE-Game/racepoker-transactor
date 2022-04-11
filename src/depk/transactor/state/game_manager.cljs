(ns depk.transactor.state.game-manager
  (:require
   [mount.core :as mount]
   [depk.transactor.state.websocket :refer [websocket]]
   [depk.transactor.game.manager :refer [make-game-manager]]))

(mount/defstate game-manager
  :start
  (make-game-manager @websocket)
  :stop
  nil)
