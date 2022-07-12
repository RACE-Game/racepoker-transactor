(ns depk.transactor.main-tournament
  (:require
   ["worker_threads"     :refer [parentPort workerData]]
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.tournament.handle :as handle]
   [depk.transactor.util :as u]
   [cljs.core.async      :as a]
   [depk.transactor.log  :as log]
   [mount.core           :as mount]))

(defn start-tournament-worker
  []
  (a/go
   (let [params      (u/transit-read (aget workerData "params"))
         {:keys [tournament-id env]} params
         _ (u/register-global-error-handler! (str "Tournament " tournament-id))
         _ (use-env env)
         _ (mount/start #'depk.transactor.state.config/config)
         _ (log/log "👷" tournament-id "Starting worker thread")
         ;; Function passed to broadcaster, to collect SSE
         post-msg-fn (fn [data]
                       (let [s (u/transit-write data)]
                         (.postMessage ^js parentPort s)))
         handle      (a/<! (handle/make-tournament-handle tournament-id post-msg-fn))]

     ;; Receive events
     (.on parentPort
          "message"
          (fn [data]
            (handle/send-event handle (u/transit-read data)))))))
