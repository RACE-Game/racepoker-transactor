(ns depk.transactor.main
  (:require
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.state.server]
   [depk.transactor.state.api]
   [depk.transactor.state.game-manager]
   [mount.core :as mount]
   [cljs.core.async :as a]
   [cljs.core.async.interop :refer [<p!]]
   [devtools.core :as devtools]))

(try
  (let [{:keys [cljs-land-style]} (devtools/get-prefs)]
    (devtools/set-pref! :cljs-land-style (str "filter:invert(1);" cljs-land-style)))
  (devtools/install!)
  (catch js/Error e))

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
