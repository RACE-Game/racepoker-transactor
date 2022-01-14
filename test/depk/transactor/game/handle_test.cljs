(ns depk.transactor.game.handle-test
  (:require
   [depk.transactor.game.handle :as sut]
   [depk.transactor.game.event-handler-mock]
   [depk.transactor.game.models :as m]
   [clojure.core.async :refer [go <! alts! timeout]]
   [cljs.test :as t
              :include-macros true]))


(t/deftest shutdown-game-handle
  (t/testing "success"
    (t/async done
      (go
       (let [init-state  (m/make-game-state {} {})
             game-handle (sut/make-game-handle "FAKEGAMEID" init-state)]
         (sut/shutdown-game-handle game-handle)
         (t/is (nil? (<! (:output game-handle))))
         (done))))))

(t/deftest get-snapshot
  (t/testing "success"
    (let [init-state  (m/make-game-state {} {})
          game-handle (sut/make-game-handle "FAKEGAMEID" init-state)]
      (t/is (= init-state (sut/get-snapshot game-handle))))))

(t/deftest send-event
  (t/testing "success"
    (t/async done
      (go
       (let [init-state  (m/make-game-state {} {})
             game-handle (sut/make-game-handle "FAKEGAMEID" init-state)
             event       (m/make-event :mock/success init-state)
             _ (sut/send-event game-handle event)
             output      (:output game-handle)
             [val port]  (alts! [output (timeout 100)])]
         (t/is (= port output))
         (t/is (true? (:success val)))
         (done))))))
