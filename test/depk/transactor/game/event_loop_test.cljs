(ns depk.transactor.game.event-loop-test
  (:require
   [depk.transactor.game.event-loop :as sut]
   [depk.transactor.game.event-handler-mock]
   [depk.transactor.game.models :as m]
   [cljs.core.async :refer [go <!]]
   [cljs.test :as t
              :include-macros true]))

(t/deftest handle-event
  (t/async done
    (go
     (t/testing "with mock success"
       (let [state (m/make-game-state {} {:sb 100, :bb 200})
             ret   (<! (sut/handle-event state (m/make-event :mock/success state)))]
         (t/is (= :ok (:result ret)))
         (t/is (-> ret
                   :state
                   :success))))

     (t/testing "with invalid state-id"
       (let [state (m/make-game-state {} {:sb 100, :bb 200})
             ret   (<! (sut/handle-event state
                                         (m/make-event :mock/success
                                                       {:state-id "INVALID STATE ID"})))]
         (t/is (= :err (:result ret)))
         (t/is (= state state))))

     (t/testing "with mock failure"
       (let [state (m/make-game-state {} {:sb 100, :bb 200})
             ret   (<! (sut/handle-event state (m/make-event :mock/failure state)))]
         (t/is (= :err (:result ret)))
         (t/is (= state state))
         (t/is (= "Mock failure" (ex-message (:error ret))))))

     (t/testing "with mock async"
       (let [state (m/make-game-state {} {:sb 100, :bb 200})
             ret   (<! (sut/handle-event state (m/make-event :mock/async state)))]
         (t/is (= :ok (:result ret)))
         (t/is (-> ret
                   :state
                   :success))))
     (done))))
