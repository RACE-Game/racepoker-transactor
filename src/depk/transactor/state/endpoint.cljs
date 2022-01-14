(ns depk.transactor.state.endpoint
  (:require
   [depk.transactor.routes :refer [make-endpoint]]
   [mount.core :as mount]))

(mount/defstate endpoint
  :start
  (make-endpoint)
  :stop
  nil)
