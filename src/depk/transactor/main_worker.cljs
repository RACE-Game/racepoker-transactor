(ns depk.transactor.main-worker
  (:require
   ["worker_threads"     :refer [parentPort workerData]]
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.util :as u]
   [cljs.core.async      :as a]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.log  :as log]
   [goog.string          :refer [format]]
   [mount.core           :as mount]))

(defn main-worker
  []
  (a/go
   (let [game-id     (aget workerData "game-id")
         env         (aget workerData "env")
         _ (use-env env)
         _ (mount/start #'depk.transactor.state.config/config)
         _ (log/infof "👷Starting worker thread: %s" game-id)
         ;; Function passed to broadcaster, to collect SSE
         post-msg-fn (fn [data]
                       (.postMessage ^js parentPort (u/transit-write data)))
         handle      (a/<! (handle/make-game-handle game-id post-msg-fn))]

     ;; Catch and log all exceptions
     (.on js/process
          "uncaughtException"
          (fn [err]
            (js/console.error
             (format "[Worker %s]There was an uncaught error, " game-id)
             err)))

     ;; Receive events
     (.on parentPort
          "message"
          (fn [data]
            (handle/send-event handle (u/transit-read data)))))))
