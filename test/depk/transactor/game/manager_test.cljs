(ns depk.transactor.game.manager-test
  (:require
   [cljs.core.async :refer [go]]
   [depk.transactor.util :refer [<!?]]
   [depk.transactor.game.manager :as sut]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.game.api-mock :as api-mock]
   [cljs.test :as t
              :include-macros true]))

(def chain-api (api-mock/->MockChainApi))

(t/deftest load-game
  (t/testing "success"
    (t/async done
      (go
       (let [manager (sut/make-game-manager chain-api)
             game-id "GAME_ID"]
         (<!? (sut/load-game manager game-id))
         (t/is (handle/game-handle? (sut/find-game manager game-id))))
       (done))))


  (t/testing "fail"
    (t/async done
      (go
       (let [manager (sut/make-game-manager chain-api)
             game-id "NONEXIST GAME ID"]
         (t/is (thrown-with-msg? ExceptionInfo #"Account not found"
                 (<!? (sut/load-game manager game-id)))))
       (done)))))

(t/deftest find-game
  (t/testing "success"
    (t/async done
      (go
       (let [manager (sut/make-game-manager chain-api)
             game-id "GAME_ID"]
         (t/is (nil? (sut/find-game manager game-id))))
       (done)))))
