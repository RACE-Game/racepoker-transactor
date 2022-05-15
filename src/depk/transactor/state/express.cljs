(ns depk.transactor.state.express
  (:require
   [depk.transactor.handlers :as h]
   [depk.transactor.log :as log]
   [depk.transactor.state.websocket :refer [websocket]]
   [taoensso.sente]
   [mount.core          :as mount]
   [cljs.core.async     :as a]
   ["http"              :as http]
   ["express"           :as express]
   ["express-ws"        :as express-ws]
   ["ws"                :as ws]
   ["cookie-parser"     :as cookie-parser]
   ["body-parser"       :as body-parser]
   ["express-session"   :as session]
   ["csurf"             :as csurf]
   ["cors"              :as cors]
   [clojure.string      :as str]
   [goog.string         :as gstr]))

(def cors-opts
  #js {:origin #".*"})

(defn setup-routes
  [^js express-app]
  (h/attach-event-handler @websocket)
  (doto express-app
   (.use (cors))
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
   (.get "/stats" (fn [req res] (h/stats req res)))
   (.use (fn [req _res next]
           (log/warnf "Unhandled request: %s" (.-originalUrl ^js req))
           (next)))))

(defn wrap-defaults
  [^js express-app]
  (let [cookie-secret "the shiz"]
    (doto express-app
     (.use (fn [req res next]
             (log/infof "Request: %s" (.-originalUrl ^js req))
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
  (log/infof "🏁Starting express...")
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
  (start-selected-web-server! 3000)
  :stop
  (.close (:http-server @server)))
