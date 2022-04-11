(ns depk.transactor.main
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.express]
   [depk.transactor.state.game-manager]
   [depk.transactor.state.global-chain-api]
   ;; [depk.transactor.helpers]
   [mount.core :as mount]
   [cljs.core.async :as a]
   [cljs.core.async.interop :refer [<p!]]))

;; Global error handler
(.on js/process "uncaughtException"
     (fn [err]
       (println err)
       (js/console.error "There was an uncaught error" err)))

(defn main
  [& args]
  (when (seq args) (use-env (keyword (first args))))
  (mount/start #'depk.transactor.state.config/config
               #'depk.transactor.state.game-manager/game-manager
               #'depk.transactor.state.express/server))

(comment
  (main))
