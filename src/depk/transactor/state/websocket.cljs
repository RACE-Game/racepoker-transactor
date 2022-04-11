(ns depk.transactor.state.websocket
  (:require
   [mount.core :as mount]
   [taoensso.sente.server-adapters.express :as sente-express]
   [taoensso.sente]
   [depk.transactor.util :as u]
   [depk.transactor.auth :as auth]))

(defn start-websocket!
  []
  (let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
                connected-uids]}
        (sente-express/make-express-channel-socket-server! {:csrf-token-fn nil,
                                                            :packer        u/sente-packer,
                                                            :user-id-fn    auth/user-id-fn})]
    {:ajax-post                ajax-post-fn,
     :ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn,
     :ch-chsk                  ch-recv,
     :chsk-send!               send-fn,
     :connected-uids           connected-uids}))

(mount/defstate websocket
  :start
  (start-websocket!))
