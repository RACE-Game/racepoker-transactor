(ns depk.transactor.main-server
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.express]
   [depk.transactor.state.worker-manager]
   [depk.transactor.state.global-chain-api]
   [depk.transactor.state.reg-center]
   [taoensso.timbre :as log]
   [mount.core      :as mount]))

(defn main-server
  [env]
  ;; Global error handler
  (.on js/process
       "uncaughtException"
       (fn [err]
         (log/error err)))

  (when env (use-env env))
  (mount/start #'depk.transactor.state.config/config
               #'depk.transactor.state.worker-manager/worker-manager
               #'depk.transactor.state.express/server
               #'depk.transactor.state.reg-center/reg-center))
