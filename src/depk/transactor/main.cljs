(ns depk.transactor.main
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.server]
   [depk.transactor.state.api]
   [depk.transactor.state.game-manager]
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
  (mount/start))

(defn reset []
  (mount/stop)
  (mount/start))

(aset js/global "reset" reset)

(comment
  (main))
