(ns depk.transactor.state.websocket
  (:require
   [mount.core          :as mount]
   [taoensso.sente.server-adapters.macchiato :refer [make-macchiato-channel-socket-server!]]
   [taoensso.sente]
   [depk.transactor.handlers :as h]
   [depk.transactor.log :as log]
   [cljs.core.async     :as a]))

(defn user-id-fn
  [{:keys [params]}]
  (:pubkey params))

(defn make-websocket
  []
  (let [{:keys [ch-recv send-fn connected-uids
                ajax-post-fn ajax-get-or-ws-handshake-fn]}
        (make-macchiato-channel-socket-server! {:csrf-token-fn nil,
                                                :user-id-fn    user-id-fn})]
    (log/info "Start websocket...")
    (a/go-loop [evt (a/<! ch-recv)]
      (try
        (h/event-msg-handler evt)
        (catch js/Error e (log/errorf e)))
      (recur (a/<! ch-recv)))
    {:ring-ajax-post ajax-post-fn,
     :ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn,
     :ch-chsk        ch-recv,
     :chsk-send!     send-fn,
     :connected-uids connected-uids}))

(mount/defstate websocket
  :start
  (make-websocket))

(comment
  (:connected-uids @websocket)
  ((:chsk-send! @websocket) "F6JoJgWrVZEUaRVpA2uQyRQDNdZXyhyiD8KqdfXcjXQN" [:htnshnt/htns 1]))
