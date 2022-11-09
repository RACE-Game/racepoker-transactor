(ns depk.transactor.game.event-handler.misc-test
  (:require
   [depk.transactor.util :refer [bigint]]
   [cljs.core.async :refer [go <!]]
   [cljs.core.async.interop :refer [<p!]]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.event-handler.misc :as sut]
   [cljs.test :as t
              :include-macros true]))

(t/deftest test-remove-players
  (t/testing "ok"
    (let [state-before {:player-map  {"player-a" (m/make-player-state
                                                  "player-a"
                                                  (bigint 100)
                                                  0),
                                      "player-b" (m/make-player-state
                                                  "player-b"
                                                  (bigint 100)
                                                  0)},
                        :rsa-pub-map {"player-a" "rsa-pub-a",
                                      "player-b" "rsa-pub-b"},
                        :ed-pub-map  {"player-a" "ed-pub-a",
                                      "player-b" "ed-pub-b"},
                        :sig-map     {"player-a" "sig-a",
                                      "player-b" "sig-b"}}
          state-after  {:player-map  {"player-b" (m/make-player-state
                                                  "player-b"
                                                  (bigint 100)
                                                  0)},
                        :rsa-pub-map {"player-b" "rsa-pub-b"},
                        :ed-pub-map  {"player-b" "ed-pub-b"},
                        :sig-map     {"player-b" "sig-b"}}]
      (t/is (= state-after (sut/remove-players state-before ["player-a"]))))))

(t/deftest test-remove-eliminated-players
  (t/testing "ok - cash"
    (let [state-before {:game-id     "game-id",
                        :player-map  {"player-a" (m/make-player-state
                                                  "player-a"
                                                  (bigint 0)
                                                  0),
                                      "player-b" (m/make-player-state
                                                  "player-b"
                                                  (bigint 0)
                                                  1),
                                      "player-c" (m/make-player-state
                                                  "player-c"
                                                  (bigint 100)
                                                  2)},
                        :rsa-pub-map {"player-a" "rsa-pub-a",
                                      "player-b" "rsa-pub-b",
                                      "player-c" "rsa-pub-c"},
                        :ed-pub-map  {"player-a" "ed-pub-a",
                                      "player-b" "ed-pub-b",
                                      "player-c" "ed-pub-c"},
                        :sig-map     {"player-a" "sig-a",
                                      "player-b" "sig-b",
                                      "player-c" "sig-c"}}
          state-after  {:game-id     "game-id",
                        :player-map  {"player-c" (m/make-player-state
                                                  "player-c"
                                                  (bigint 100)
                                                  2)},
                        :rsa-pub-map {"player-c" "rsa-pub-c"},
                        :ed-pub-map  {"player-c" "ed-pub-c"},
                        :sig-map     {"player-c" "sig-c"},
                        :ranking     nil}]
      (t/is (= state-after (sut/remove-eliminated-players state-before)))))

  (t/testing "ok - sng"
    (let [state-before {:game-id     "game-id",
                        :game-type   :sng,
                        :player-map  {"player-a" (m/make-player-state
                                                  "player-a"
                                                  (bigint 0)
                                                  0),
                                      "player-b" (m/make-player-state
                                                  "player-b"
                                                  (bigint 0)
                                                  1),
                                      "player-c" (m/make-player-state
                                                  "player-c"
                                                  (bigint 200)
                                                  2)},
                        :ranking     '("player-d"),
                        :rsa-pub-map {"player-a" "rsa-pub-a",
                                      "player-b" "rsa-pub-b",
                                      "player-c" "rsa-pub-c"},
                        :ed-pub-map  {"player-a" "ed-pub-a",
                                      "player-b" "ed-pub-b",
                                      "player-c" "ed-pub-c"},
                        :sig-map     {"player-a" "sig-a",
                                      "player-b" "sig-b",
                                      "player-c" "sig-c"}}
          state-after  {:game-id     "game-id",
                        :game-type   :sng,
                        :player-map  {"player-c" (m/make-player-state
                                                  "player-c"
                                                  (bigint 200)
                                                  2)},
                        :rsa-pub-map {"player-c" "rsa-pub-c"},
                        :ed-pub-map  {"player-c" "ed-pub-c"},
                        :sig-map     {"player-c" "sig-c"},
                        :ranking     '("player-a" "player-b" "player-d")}]
      (t/is (= state-after (sut/remove-eliminated-players state-before))))))

(t/deftest test-reset-sng-state
  (t/testing "ok"
    (let [state-before {:base-sb   (bigint 10),
                        :base-bb   (bigint 20),
                        :winner-id "player-a"}
          state-after  {:player-map {},
                        ;; Why we keep winner-id here?
                        :winner-id  "player-a",
                        :start-time nil,
                        :ranking    nil,
                        :sb         (bigint 10),
                        :bb         (bigint 20),
                        :base-sb    (bigint 10),
                        :base-bb    (bigint 20)}]
      (t/is (= state-after (sut/reset-sng-state state-before))))))

(t/deftest test-add-joined-player
  (t/testing "ok"
    (let [state-before {:player-map
                        {"player-a" (m/make-player-state
                                     "player-a"
                                     (bigint 200)
                                     0)},
                        :joined-players
                        [nil {:pubkey "player-b", :chips (bigint 300)} nil nil nil nil nil nil
                         nil],
                        :game-id        "game-id"}
          state-after  {:player-map
                        {"player-a" (m/make-player-state
                                     "player-a"
                                     (bigint 200)
                                     0),
                         "player-b" (m/make-player-state
                                     "player-b"
                                     (bigint 300)
                                     1
                                     :player-status/fold
                                     :dropout)},
                        :joined-players
                        [nil {:pubkey "player-b", :chips (bigint 300)} nil nil nil nil nil nil
                         nil],
                        :game-id        "game-id"}]
      (t/is (= state-after (sut/add-joined-player state-before))))))
