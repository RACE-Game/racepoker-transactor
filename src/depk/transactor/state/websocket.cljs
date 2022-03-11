(ns depk.transactor.state.websocket
  (:require
   [mount.core :as mount]
   [taoensso.sente.server-adapters.express :as sente-express]
   [taoensso.sente]
   [depk.transactor.handlers :as h]
   [depk.transactor.game.state-broadcast :as b]
   [depk.transactor.log :as log]
   [cljs.core.async :as a]
   [solana-clj.publickey :as pubkey]
   ["tweetnacl" :as nacl]
   ["buffer" :as buffer]))

(defn start-websocket!
  []
  (let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
                connected-uids]}
        (sente-express/make-express-channel-socket-server! {:csrf-token-fn nil,
                                                            :user-id-fn    h/user-id-fn})]
    (h/attach-event-handler ch-recv)
    (b/attach-broadcast-handler connected-uids send-fn)
    {:ajax-post                ajax-post-fn,
     :ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn,
     :ch-chsk                  ch-recv,
     :chsk-send!               send-fn,
     :connected-uids           connected-uids}))

(mount/defstate websocket
  :start
  (start-websocket!))
