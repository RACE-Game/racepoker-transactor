(ns depk.transactor.state.express
  (:require
   [depk.transactor.handlers :as h]
   [depk.transactor.log :as log]
   [depk.transactor.state.websocket :refer [websocket]]
   [depk.transactor.state.config :refer [config]]
   [taoensso.sente]
   [mount.core          :as mount]
   ["express"           :as express]
   ["express-ws"        :as express-ws]
   ["cookie-parser"     :as cookie-parser]
   ["body-parser"       :as body-parser]
   ["express-session"   :as session]
   ["csurf"             :as csurf]
   ["cors"              :as cors]))

(def cors-opts
  #js {:origin #".*"})

(defn setup-routes
  [^js express-app]
  (h/attach-event-handler @websocket)
  (doto express-app
   (.use (cors))
   (.use (.urlencoded body-parser
                      #js {:extended false}))
   (.ws "/api"
        (fn [ws req next]
          ((:ajax-get-or-ws-handshake @websocket)
           req
           nil
           nil
           {:websocket? true,
            :websocket  ws})))
   (.get "/api" (:ajax-get-or-ws-handshake @websocket))
   (.post "/api" (:ajax-post @websocket))
   (.get "/tournament/:tournamentId"
         (fn [req res]
           (h/get-tournament req res)))
   (.put "/tournament/:tournamentId"
         (fn [req res]
           (h/load-tournament req res)))
   (.get "/stats"
         (fn [req res]
           (h/stats req res)))
   (.use (fn [req _res next]
           (log/log "🚫" nil "Unhandled request: %s" (.-originalUrl ^js req))
           (next)))))

(defn wrap-defaults
  [^js express-app]
  (let [cookie-secret "the shiz"]
    (doto express-app
     (.use (fn [req res next]
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
     (setup-routes))))

(defn start-selected-web-server!
  [port]
  (log/log "🎉" nil "Start web server at port: %s" port)
  (let [express-app       (express)
        express-ws-server (express-ws express-app)]

    (setup-routes express-app)

    (let [http-server (.listen express-app port)]
      {:express-app express-app,
       :ws-server   express-ws-server,
       :http-server http-server,
       :stop-fn     #(.close http-server),
       :port        port})))

(mount/defstate server
  :start
  (start-selected-web-server! (get-in @config [:transactor :port] 3000))
  :stop
  (.close (:http-server @server)))
