(ns depk.transactor.main
  (:require
   [depk.transactor.state.config :refer [use-env]]
   ;; [depk.transactor.state.websocket]
   ;; [depk.transactor.state.server]
   [depk.transactor.state.express]
   [depk.transactor.state.api]
   [depk.transactor.state.game-manager]
   [depk.transactor.helpers]
   [mount.core :as mount]
   [cljs.core.async :as a]
   [cljs.core.async.interop :refer [<p!]]))

;; Global error handler

;; process.on('uncaughtException', err => {
;;   console.error('There was an uncaught error', err)
;;   process.exit(1) //mandatory (as per the Node.js docs)
;; })

(.on js/process "uncaughtException"
     (fn [err]
       (js/console.error "There was an uncaught error" err)))

(defn main
  [& args]
  (when (seq args) (use-env (keyword (first args))))
  (mount/start #'depk.transactor.state.config/config
               #'depk.transactor.state.api/chain-api
               #'depk.transactor.state.api/store-api
               #'depk.transactor.state.game-manager/game-manager
               ;; #'depk.transactor.state.endpoint/endpoint
               ;; #'depk.transactor.state.websocket/websocket
               ;; #'depk.transactor.state.server/server
               #'depk.transactor.state.express/server
               ))

(comment
  (main))
