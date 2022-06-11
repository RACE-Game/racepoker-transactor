(ns depk.transactor.main-server
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.express]
   [depk.transactor.state.game-manager]
   [depk.transactor.state.global-chain-api]
   [mount.core :as mount]))

(defn main-server
  [env]
  ;; Global error handler
  (.on js/process
       "uncaughtException"
       (fn [err]
         (println err)
         (js/console.error "There was an uncaught error, " err)))

  (when env (use-env env))
  (mount/start #'depk.transactor.state.config/config
               #'depk.transactor.state.game-manager/game-manager
               #'depk.transactor.state.express/server))
