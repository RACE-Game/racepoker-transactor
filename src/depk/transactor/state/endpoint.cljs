(ns depk.transactor.state.endpoint
  (:require
   [depk.transactor.routes :refer [make-endpoint]]
   [depk.transactor.log :as log]
   [mount.core :as mount]))

(mount/defstate endpoint
  :start
  (do (log/infof "Start endpoint")
      (make-endpoint)))
