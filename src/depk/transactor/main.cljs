(ns depk.transactor.main
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.server]
   [depk.transactor.state.api]
   [depk.transactor.state.game-manager]
   [mount.core :as mount]
   [cljs.core.async :as a]
   [cljs.core.async.interop :refer [<p!]]))

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
