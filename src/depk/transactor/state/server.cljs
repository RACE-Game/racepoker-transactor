(ns depk.transactor.state.server
  (:require
   [depk.transactor.state.config :refer [config]]
   [depk.transactor.state.endpoint :refer [endpoint]]
   [macchiato.server :as http]
   [depk.transactor.log :as log]
   [mount.core       :as mount]))

(defn start-server
  [endpoint]
  (let [{:keys [host port]} (:transactor @config)
        host (or host "0.0.0.0")
        port (or port 3000)
        opts {:handler    endpoint,
              :host       host,
              :port       port,
              :on-success #(log/infof "Server started on %s:%s" host port)}]
    (http/start opts)))

(mount/defstate server
  :start
  (start-server @endpoint)
  :stop
  (.close @server))

(comment
  (mount/start server)
  (info @server))
