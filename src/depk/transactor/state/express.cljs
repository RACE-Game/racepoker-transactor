(ns depk.transactor.state.express
  (:require
   [depk.transactor.handlers :as h]
   [depk.transactor.log :as log]
   [taoensso.sente]
   [taoensso.sente.server-adapters.express :as sente-express]
   [mount.core      :as mount]
   ["http"          :as http]
   ["express"       :as express]
   ["express-ws"    :as express-ws]
   ["ws"            :as ws]
   ["cookie-parser" :as cookie-parser]
   ["body-parser"   :as body-parser]
   ["express-session" :as session]
   ["csurf"         :as csurf]))

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
              connected-uids]}
      (sente-express/make-express-channel-socket-server! {:csrf-token-fn nil,
                                                          :user-id-fn    h/user-id-fn})]
  (def ajax-post ajax-post-fn)
  (def ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def connected-uids connected-uids))

(defonce _attach-event-handler
  (h/attach-event-handler ch-chsk))

(defn routes
  [^js express-app]
  (doto express-app
   (.ws "/api"
        (fn [ws req next]
          (ajax-get-or-ws-handshake req
                                    nil
                                    nil
                                    {:websocket? true,
                                     :websocket  ws})))
   (.get "/api" ajax-get-or-ws-handshake)
   (.post "/api" ajax-post)
   (.use (fn [req res next]
           (log/warnf "Unhandled request: %s" (.-originalUrl ^js req))
           (next)))))

(defn wrap-defaults
  [^js express-app routes]
  (let [cookie-secret "the shiz"]
    (doto express-app
     (.use (fn [req res next]
             (log/debugf "Request: %s" (.-originalUrl ^js req))
             (next)))
     (.use (session
            #js
             {:secret            cookie-secret,
              :resave            true,
              :cookie            {},
              :store             (.MemoryStore session),
              :saveUninitialized true}))
     (.use (.urlencoded body-parser
                        #js {:extended false}))
     (.use (cookie-parser cookie-secret))
     (.use (csurf
            #js {:cookie false}))
     (routes))))

(defn main-ring-handler
  [express-app]
  ;; Can we even call this a ring handler?
  (wrap-defaults express-app routes))

(defn start-selected-web-server!
  [ring-handler port]
  (log/infof "Starting express...")
  (let [express-app       (express)
        express-ws-server (express-ws express-app)]

    (ring-handler express-app)

    (let [http-server (.listen express-app port)]
      {:express-app express-app,
       :ws-server   express-ws-server,
       :http-server http-server,
       :stop-fn     #(.close http-server),
       :port        port})))

(mount/defstate server
  :start
  (start-selected-web-server! routes 3000)
  :stop
  (do (.close (:http-server @server))))
