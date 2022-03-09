(ns depk.transactor.routes
  (:require
   [macchiato.middleware.params :refer [wrap-params]]
   [macchiato.middleware.keyword-params :refer [wrap-keyword-params]]
   [macchiato.middleware.restful-format :refer [wrap-restful-format]]
   [macchiato.middleware.defaults :refer [wrap-defaults api-defaults]]
   [reitit.ring :as ring]
   ;; [ring.middleware.cors :refer [wrap-cors]]
   [depk.transactor.cors :refer [wrap-cors]]
   [depk.transactor.handlers :as h]
   [depk.transactor.state.config :as config]
   [depk.transactor.state.websocket :refer [websocket]]
   [depk.transactor.util :as u]
   [cognitect.transit :as transit]))

(defn make-game-routes
  []
  ["/game"
   ["/attach" {:post h/attach-game}]
   ["/state" {:post h/state}]])

(defn make-websocket-routes
  []
  ["/chsk" {:get (:ring-ajax-get-or-ws-handshake @websocket)
            :post (:ring-ajax-post @websocket)
            :ws (:ring-ajax-get-or-ws-handshake @websocket)}])

(defn make-info-routes
  []
  ["/info"
   ["/histories" {:get h/histories}]])

(defn make-faucet-routes
  []
  ["/faucet"
   ["/request" {:post h/request-test-token}]])

(defn make-action-routes
  []
  ["/action"
   ["/release" {:post h/release}]
   ["/leave" {:post h/leave}]
   ["/alive" {:post h/alive}]
   ["/shuffle" {:post h/shuffle-cards}]
   ["/encrypt" {:post h/encrypt-cards}]
   ["/share" {:post h/share-keys}]
   ["/call" {:post h/call}]
   ["/fold" {:post h/fold}]
   ["/bet" {:post h/bet}]
   ["/raise" {:post h/raise}]
   ["/check" {:post h/check}]
   ["/musk" {:post h/musk}]])

(defn make-routes
  []
  [""
   ["/" {:get h/alive-handler}]
   ["/api/v1"
    (make-websocket-routes)
    (make-game-routes)
    (make-info-routes)
    (make-faucet-routes)
    (make-action-routes)]])

(defn make-endpoint
  []
  (ring/ring-handler
   (ring/router
    [(make-routes)]
    ;; {:data {:middleware [[wrap-cors
    ;;                       :access-control-allow-origin #".*"
    ;;                       :access-control-allow-methods [:get :post :put :delete :ws]]
    ;;                      [wrap-restful-format
    ;;                       {:keywordize? true}]
    ;;                      wrap-params
    ;;                      wrap-keyword-params]}}
    {:data {:middleware [[wrap-defaults api-defaults]]}}
    )
   (ring/create-default-handler)))
