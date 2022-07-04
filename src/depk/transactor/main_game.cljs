(ns depk.transactor.main-game
  (:require
   ["worker_threads"     :refer [parentPort workerData]]
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.util :as u]
   [cljs.core.async      :as a]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.log  :as log]
   [mount.core           :as mount]))

(defn start-game-worker
  []
  (a/go
   (let [params      (u/transit-read (aget workerData "params"))
         {:keys [game-id env]} params
         _ (u/register-global-error-handler! (str "Game " game-id))
         _ (use-env env)
         _ (mount/start #'depk.transactor.state.config/config)
         _ (log/infof "👷Starting worker thread: %s" game-id)
         ;; Function passed to broadcaster, to collect SSE
         post-msg-fn (fn [data]
                       (let [s (u/transit-write data)]
                         (.postMessage ^js parentPort s)))
         handle      (a/<! (handle/make-game-handle game-id post-msg-fn))]

     ;; Receive events
     (.on parentPort
          "message"
          (fn [data]
            (handle/send-event handle (u/transit-read data)))))))
