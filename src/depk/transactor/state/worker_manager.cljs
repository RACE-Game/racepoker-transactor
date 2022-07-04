(ns depk.transactor.state.worker-manager
  (:require
   [mount.core :as mount]
   [depk.transactor.state.websocket :refer [websocket]]
   [depk.transactor.game.worker :as worker]
   [depk.transactor.worker-manager :refer [make-worker-manager]]))

(mount/defstate worker-manager
  :start
  (make-worker-manager {:websocket @websocket})
  :stop
  nil)
