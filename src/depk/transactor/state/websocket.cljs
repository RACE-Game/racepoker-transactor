(ns depk.transactor.state.websocket
  (:require
   [mount.core :as mount]
   [taoensso.sente.server-adapters.macchiato :refer [make-macchiato-channel-socket-server!]]
   [taoensso.sente]
   [depk.transactor.handlers :as h]
   [depk.transactor.log :as log]
   [cljs.core.async :as a]
   [solana-clj.publickey :as pubkey]
   ["tweetnacl" :as nacl]
   ["buffer" :as buffer]))

(def lock_ (atom nil))

(defn make-websocket
  []
  (when-not @lock_
    (reset! lock_ true)
    (let [{:keys [ch-recv send-fn connected-uids
                  ajax-post-fn ajax-get-or-ws-handshake-fn]}
          (make-macchiato-channel-socket-server!
           {:csrf-token-fn nil, ; skip CSRF check, no page hosted
            :user-id-fn    h/user-id-fn})]
      (log/info "Start websocket...")
      (h/attach-event-handler ch-recv)
      {:ring-ajax-post ajax-post-fn,
       :ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn,
       :ch-chsk        ch-recv,
       :chsk-send!     send-fn,
       :connected-uids connected-uids})))

(mount/defstate websocket
  :start
  (make-websocket))

(comment
  (:connected-uids @websocket)
  ((:chsk-send! @websocket) "F6JoJgWrVZEUaRVpA2uQyRQDNdZXyhyiD8KqdfXcjXQN" [:htnshnt/htns 1]))
