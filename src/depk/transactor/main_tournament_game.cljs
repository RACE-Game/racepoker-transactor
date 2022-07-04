(ns depk.transactor.main-tournament-game
  (:require
   ["worker_threads"     :refer [parentPort workerData]]
   [depk.transactor.state.config :refer [use-env]]
   [depk.transactor.util :as u]
   [cljs.core.async      :as a]
   [depk.transactor.tournament-game.handle :as handle]
   [depk.transactor.log  :as log]
   [mount.core           :as mount]))

(defn start-tournament-game-worker
  []
  (a/go
   (let [params      (u/transit-read (aget workerData "params"))
         {:keys [game-id tournament-id players size env]} params
         _ (u/register-global-error-handler! (str "Tournament game " tournament-id "#" game-id))
         _ (log/infof "👷Starting tournament game worker thread: %s # %s" tournament-id game-id)
         _ (log/infof "👷Worker params: %s" params)
         _ (use-env env)
         _ (mount/start #'depk.transactor.state.config/config)
         ;; Function passed to broadcaster, to collect SSE
         post-msg-fn (fn [data]
                       (let [s (u/transit-write data)]
                         (.postMessage ^js parentPort s)))
         handle      (a/<! (handle/make-game-handle game-id players size post-msg-fn))]

     ;; Receive events
     (.on parentPort
          "message"
          (fn [data]
            (handle/send-event handle (u/transit-read data)))))))
