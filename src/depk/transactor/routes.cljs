(ns depk.transactor.routes
  (:require
   [macchiato.middleware.params :refer [wrap-params]]
   [macchiato.middleware.keyword-params :refer [wrap-keyword-params]]
   [macchiato.middleware.restful-format :refer [wrap-restful-format]]
   [reitit.ring :as ring]
   ;; [ring.middleware.cors :refer [wrap-cors]]
   [depk.transactor.cors :refer [wrap-cors]]
   [depk.transactor.handlers :as h]
   [depk.transactor.state.config :as config]
   [depk.transactor.util :as u]))

(defn make-game-routes
  []
  ["/game"
   ["/attach" {:post h/attach-game}]
   ["/dettach" {:post h/dettach-game}]
   ["/state" {:post h/state}]])

(defn make-action-routes
  []
  ["/action"
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
    (make-game-routes)
    (make-action-routes)]])

(defn make-endpoint
  []
  (ring/ring-handler
   (ring/router
    [(make-routes)]
    {:data {:middleware [[wrap-cors
                          :access-control-allow-origin #".*"
                          :access-control-allow-methods [:get :post :put :delete]]
                         [wrap-restful-format
                          {:keywordize? true}]
                         wrap-params
                         wrap-keyword-params]}})
   (ring/create-default-handler)))
