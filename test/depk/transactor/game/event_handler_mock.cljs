(ns depk.transactor.game.event-handler-mock
  (:require [depk.transactor.game.event-handler :as sut]
            [cljs.core.async :refer [go]]
            ;; [cljs.test :as t :include-macros true]
            ))

;; some mock event handler

(defmethod sut/handle-event :mock/success
  [state _event]
  (assoc state :success true))

(defmethod sut/handle-event :mock/failure
  [_state _event]
  (throw (ex-info "Mock failure" {})))

(defmethod sut/handle-event :mock/async
  [state _event]
  (go (assoc state :success true)))
