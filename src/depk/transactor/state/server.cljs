(ns depk.transactor.state.server
  (:require
   [depk.transactor.state.config :refer [config]]
   [depk.transactor.state.endpoint :refer [endpoint]]
   [macchiato.server :as http]
   [taoensso.timbre  :refer [info]]
   [mount.core       :as mount]))

(defn start-server
  [endpoint]
  (let [{:keys [host port]} (:server @config)
        opts {:handler    endpoint,
              :host       (or host "0.0.0.0")
              :port       (or port 3000)
              :on-success #(info "Server started on " host ":" port)}]
    (http/start opts)))

(mount/defstate server
  :start
  (start-server @endpoint)
  :stop
  (.close @server))

(comment
  (mount/start server)
  (info @server))
