(ns depk.transactor.game.event-handler-test
  "Tests for event handler.

  The tests are based on event types:
  - For each event type, we must make sure only expected events are accepted.
  - For each event, we must make sure invalid events are rejected.
  - For each event, we must make sure all branches are covered."
  (:require
   [depk.transactor.util :refer [bigint]]
   [cljs.core.async :refer [go <!]]
   [depk.transactor.constant :as c]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.event-handler :as sut]
   [cljs.test :as t
              :include-macros true]))

;; Event type: system/sync-state
(t/deftest test-system-sync-state
  (t/testing "Err:"
    (t/testing "Invalid buyin serial"
      (let [state-before {:game-account-state {:buyin-serial 1,
                                               :players      [{:pubkey "player-a"}
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil]}}
            event        (m/make-event :system/sync-state
                                       {:game-account-state
                                        {:buyin-serial 1,
                                         :players      [{:pubkey "player-a"}
                                                        nil
                                                        nil
                                                        nil
                                                        nil
                                                        nil
                                                        nil
                                                        nil
                                                        nil]}})]
        (t/is
         (thrown-with-msg? ExceptionInfo #"State already merged"
           (sut/handle-event state-before event))))))

  (t/testing "Ok:"
    (t/testing "Reschedule start game"
      (let [state-before {:status             :game-status/init,
                          :game-account-state {:buyin-serial 2,
                                               :players      [{:pubkey "player-a", :buyin-serial 1}
                                                              {:pubkey "player-b", :buyin-serial 2}
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil]}}
            event        (m/make-event :system/sync-state
                                       {:game-account-state
                                        {:buyin-serial 3,
                                         :players      [nil
                                                        {:pubkey "player-b", :buyin-serial 2}
                                                        nil
                                                        nil
                                                        {:pubkey "player-c", :buyin-serial 3}
                                                        nil
                                                        nil
                                                        nil
                                                        nil]}})
            state-after  {:status             :game-status/init,
                          :game-account-state {:buyin-serial 3,
                                               :players      [nil
                                                              {:pubkey "player-b", :buyin-serial 2}
                                                              nil
                                                              nil
                                                              {:pubkey "player-c", :buyin-serial 3}
                                                              nil
                                                              nil
                                                              nil
                                                              nil]},
                          :joined-players     [nil
                                               nil
                                               nil
                                               nil
                                               {:pubkey "player-c", :buyin-serial 3}
                                               nil
                                               nil
                                               nil
                                               nil],
                          :dispatch-event     [c/new-player-start-delay
                                               (m/make-event :system/start-game)]}]
        (t/is (= state-after (sut/handle-event state-before event)))))

    (t/testing "Reverse current dispatch"
      (let [state-before {:game-account-state {:buyin-serial 2,
                                               :players      [{:pubkey "player-a", :buyin-serial 1}
                                                              {:pubkey "player-b", :buyin-serial 2}
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil]}}
            event        (m/make-event :system/sync-state
                                       {:game-account-state
                                        {:buyin-serial 3,
                                         :players      [nil
                                                        {:pubkey "player-b", :buyin-serial 2}
                                                        nil
                                                        nil
                                                        {:pubkey "player-c", :buyin-serial 3}
                                                        nil
                                                        nil
                                                        nil
                                                        nil]}})
            state-after  {:game-account-state {:buyin-serial 3,
                                               :players      [nil
                                                              {:pubkey "player-b", :buyin-serial 2}
                                                              nil
                                                              nil
                                                              {:pubkey "player-c", :buyin-serial 3}
                                                              nil
                                                              nil
                                                              nil
                                                              nil]},
                          :joined-players     [nil
                                               nil
                                               nil
                                               nil
                                               {:pubkey "player-c", :buyin-serial 3}
                                               nil
                                               nil
                                               nil
                                               nil],
                          :reserve-timeout    true}]
        (t/is (= state-after (sut/handle-event state-before event)))))))

;; Event type: system/reset

(t/deftest test-system-reset
  (t/testing "Ok"
    (let [state-before
          {:player-map     {"player-a"
                            (m/make-player-state
                             "player-a"
                             (bigint 100)
                             0
                             :player-status/acted
                             :normal),

                            "player-b"
                            (m/make-player-state
                             "player-b"
                             (bigint 0)
                             1
                             :player-status/acted
                             :normal),

                            "player-c"
                            (m/make-player-state
                             "player-c"
                             (bigint 100)
                             2
                             :player-status/acted
                             :dropout),

                            "player-d"
                            (m/make-player-state
                             "player-d"
                             (bigint 100)
                             3
                             :player-status/acted
                             :leave)},
           :joined-players
           [nil nil nil nil {:pubkey "player-e", :chips (bigint 100)} nil nil nil nil nil]}

          state-after
          {:player-map         {"player-a"
                                (m/make-player-state
                                 "player-a"
                                 (bigint 100)
                                 0
                                 :player-status/wait
                                 :dropout),

                                "player-e"
                                (m/make-player-state
                                 "player-e"
                                 (bigint 100)
                                 4
                                 :player-status/wait
                                 :dropout)},
           :dispatch-event
           [c/default-start-game-delay
            {:type      :system/start-game,
             :data      {},
             :player-id nil,
             :timestamp nil}],

           :status             :game-status/init,
           :joined-players     nil,
           :card-ciphers       [],
           :released-keys-map  nil,
           :chips-change-map   nil,
           :after-key-share    nil,
           :showdown-map       nil,
           :share-key-map      nil,
           :community-cards    nil,
           :ed-pub-map         nil,
           :logs               [],
           :rake-map           nil,
           :prize-map          nil,
           :winner-id          nil,
           :street             nil,
           :bet-map            nil,
           :sig-map            nil,
           :secret-nonce-map   nil,
           :require-key-idents nil,
           :winning-type       nil,
           :pots               [],
           :ranking            nil,
           :min-raise          nil,
           :after-keyshare     nil,
           :street-bet         nil,
           :rsa-pub-map        nil}
          event        (m/make-event :system/reset {} nil 10000000)]
      ;; (t/is (= state-after (sut/handle-event state-before event)))
    )))

;; Event type: system/start-game

(t/deftest test-system-start-game
  (let [event (m/make-event :system/start-game)]

    (t/testing "Err:"
      (t/testing "Invalid game status"
        (t/testing "Shutdown due to game closed"
          (let [state-before {:game-type  :tournament,
                              :player-map {}}]
            (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
                    (sut/handle-event state-before event)))))))

    (t/testing "Ok:"
      (t/testing "Can't start game:"

        (t/testing "Tournament:"
          (t/testing "Shutdown due to game closed"
            (let [state-before {:game-type  :tournament,
                                :status     :game-status/init,
                                :player-map {}}
                  state-after  {:game-type  :tournament,
                                :status     :game-status/init,
                                :player-map {},
                                :shutdown?  true}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "Reset due to game halted"
            (let [state-before {:game-type  :tournament,
                                :status     :game-status/init,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a" (bigint 100) 0),
                                             "player-b"
                                             (m/make-player-state "player-b" (bigint 100) 1)},
                                :halt?      true}
                  state-after  {:game-type      :tournament,
                                :status         :game-status/init,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a" (bigint 100) 0),
                                                 "player-b"
                                                 (m/make-player-state "player-b" (bigint 100) 1)},
                                :halt?          true,
                                :dispatch-event [c/reset-timeout-delay
                                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          ;; In this case, we need to send a settlement request which
          ;; can trigger the reseat in tournament reconciler.
          (t/testing "Reset due to there's only one player waiting for a seat in other tables"
            (let [state-before {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0)}}
                  state-after  {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0)},
                                :api-requests
                                [{:type :system/settle,
                                  :data {:settle-map
                                         {"player-a" {:settle-type   :no-update,
                                                      :settle-status :no-update,
                                                      :amount        (bigint 0),
                                                      :rake          (bigint 0)}},
                                         :settle-serial 1}}],
                                :rake-map           nil,
                                ;; Why we set halt? to true? Is this necessary?
                                :halt?              true,
                                :dispatch-event
                                [c/reset-timeout-delay (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "Reset due to no player ready"
            (let [state-before {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0
                                                                          :player-status/acted
                                                                          :dropout),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)}}
                  state-after  {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0
                                                                          :player-status/acted
                                                                          :dropout),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)},
                                :api-requests
                                [{:type :system/settle,
                                  :data {:settle-map
                                         {"player-a" {:settle-type   :no-update,
                                                      :settle-status :leave,
                                                      :amount        (bigint 0),
                                                      :rake          (bigint 0)},
                                          "player-b" {:settle-type   :no-update,
                                                      :settle-status :leave,
                                                      :amount        (bigint 0),
                                                      :rake          (bigint 0)}},
                                         :settle-serial 1}}],
                                :rake-map           nil,
                                ;; Why we set halt? to true? Is this necessary?
                                :halt?              true,
                                :dispatch-event
                                [c/reset-timeout-delay (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          ;; How the blinds out event being cancelled?
          (t/testing "Blinds out due to there's only one player ready"
            (let [state-before {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :btn                0,
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)}}
                  state-after  {:game-type          :tournament,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :btn                0,
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)},
                                :dispatch-event     [c/blinds-out-delay
                                                     (m/make-event :system/blinds-out
                                                                   {:winner-id "player-a"})]}]
              (t/is (= state-after (sut/handle-event state-before event))))))

        (t/testing "SNG:"
          (t/testing "First start failed due to not all players are ready"
            (let [state-before {:game-type  :sng,
                                :status     :game-status/init,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a" (bigint 100) 0),
                                             "player-b"
                                             (m/make-player-state "player-b"
                                                                  (bigint 100)
                                                                  0
                                                                  :player-status/wait
                                                                  :dropout)}}
                  state-after  {:game-type      :sng,
                                :status         :game-status/init,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a" (bigint 100) 0),
                                                 "player-b"
                                                 (m/make-player-state "player-b"
                                                                      (bigint 100)
                                                                      0
                                                                      :player-status/wait
                                                                      :dropout)},
                                :dispatch-event
                                [c/reset-timeout-delay
                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "First start failed due to table is not full"
            (let [state-before {:game-type  :sng,
                                :status     :game-status/init,
                                :size       3,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a" (bigint 100) 0),
                                             "player-b"
                                             (m/make-player-state "player-b" (bigint 100) 0)}}
                  state-after  {:game-type      :sng,
                                :status         :game-status/init,
                                :size           3,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a" (bigint 100) 0),
                                                 "player-b"
                                                 (m/make-player-state "player-b" (bigint 100) 0)},
                                :dispatch-event
                                [c/reset-timeout-delay
                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "Reset due to no player ready"
            (let [state-before {:game-type  :sng,
                                :status     :game-status/init,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a" (bigint 100) 0),
                                             "player-b"
                                             (m/make-player-state "player-b"
                                                                  (bigint 100)
                                                                  0
                                                                  :player-status/wait
                                                                  :dropout)}}
                  state-after  {:game-type      :sng,
                                :status         :game-status/init,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a" (bigint 100) 0),
                                                 "player-b"
                                                 (m/make-player-state "player-b"
                                                                      (bigint 100)
                                                                      0
                                                                      :player-status/wait
                                                                      :dropout)},
                                :dispatch-event
                                [c/reset-timeout-delay
                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "Blinds out due to there's only one player ready"
            (let [state-before {:game-type          :sng,
                                :start-time         1000000,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :btn                0,
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)}}
                  state-after  {:game-type          :sng,
                                :start-time         1000000,
                                :status             :game-status/init,
                                :game-account-state {:settle-serial 1},
                                :btn                0,
                                :player-map         {"player-a"
                                                     (m/make-player-state "player-a"
                                                                          (bigint 100)
                                                                          0),
                                                     "player-b"
                                                     (m/make-player-state "player-b"
                                                                          (bigint 100)
                                                                          1
                                                                          :player-status/acted
                                                                          :dropout)},
                                :dispatch-event     [c/blinds-out-delay
                                                     (m/make-event :system/blinds-out
                                                                   {:winner-id "player-a"})]}]
              (t/is (= state-after (sut/handle-event state-before event))))))

        (t/testing "Cash:"
          (t/testing "Reset due to no enough players"
            (let [state-before {:game-type  :cash,
                                :status     :game-status/init,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a" (bigint 100) 0)}}
                  state-after  {:game-type      :cash,
                                :status         :game-status/init,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a" (bigint 100) 0)},
                                :dispatch-event
                                [c/reset-timeout-delay
                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))

          (t/testing "Reset due to someone is not ready"
            (let [state-before {:game-type  :cash,
                                :status     :game-status/init,
                                :player-map {"player-a"
                                             (m/make-player-state "player-a"
                                                                  (bigint 100)
                                                                  0),
                                             "player-b"
                                             (m/make-player-state "player-b"
                                                                  (bigint 100)
                                                                  1
                                                                  :player-status/acted
                                                                  :dropout)}}
                  state-after  {:game-type      :cash,
                                :status         :game-status/init,
                                :player-map     {"player-a"
                                                 (m/make-player-state "player-a"
                                                                      (bigint 100)
                                                                      0),
                                                 "player-b"
                                                 (m/make-player-state "player-b"
                                                                      (bigint 100)
                                                                      1
                                                                      :player-status/acted
                                                                      :dropout)},
                                :dispatch-event
                                [c/reset-timeout-delay
                                 (m/make-event :system/reset)]}]
              (t/is (= state-after (sut/handle-event state-before event)))))))

      (let
        [prepare-cards
         [{:player-id nil,
           :op        :init,
           :data
           "af11-af54-af57-af0b-af58-af59-af0a-af52-af55-af14-af53-af56-af01-bf11-bf54-bf57-bf0b-bf58-bf59-bf0a-bf52-bf55-bf14-bf53-bf56-bf01-b411-b454-b457-b40b-b458-b459-b40a-b452-b455-b414-b453-b456-b401-b811-b854-b857-b80b-b858-b859-b80a-b852-b855-b814-b853-b856-b801"}]]
        (t/testing "Start cash game"
          (let
            [state-before {:status     :game-status/init,
                           :game-type  :cash,
                           :player-map {"player-a"
                                        (m/make-player-state "player-a"
                                                             (bigint 100)
                                                             0),
                                        "player-b"
                                        (m/make-player-state "player-b"
                                                             (bigint 100)
                                                             1)},
                           :btn        0}
             state-after
             {:status            :game-status/shuffle,
              :start-time        nil,
              :prepare-cards     prepare-cards,
              :game-type         :cash,
              :player-map        {"player-a"
                                  (m/make-player-state "player-a"
                                                       (bigint 100)
                                                       0),
                                  "player-b"
                                  (m/make-player-state "player-b"
                                                       (bigint 100)
                                                       1)},
              :btn               1,
              :op-player-ids     ["player-a" "player-b"],
              :encrypt-player-id nil,
              :shuffle-player-id "player-a",
              :dispatch-event    [c/shuffle-timeout-delay
                                  (m/make-event :system/shuffle-timeout)]}]
            (t/async done
              (go
               (t/is (= state-after (<! (sut/handle-event state-before event))))
               (done)))))

        (t/testing "Start sng game"
          (let
            [state-before {:status     :game-status/init,
                           :game-type  :sng,
                           :size       3,
                           :start-time 1000000,
                           :player-map {"player-a"
                                        (m/make-player-state "player-a"
                                                             (bigint 100)
                                                             0),
                                        "player-b"
                                        (m/make-player-state "player-b"
                                                             (bigint 100)
                                                             1),
                                        "player-c"
                                        (m/make-player-state "player-)"
                                                             (bigint 100)
                                                             2
                                                             :player-status/wait
                                                             :dropout)},
                           :btn        0}
             state-after
             {:status            :game-status/shuffle,
              :start-time        1000000,
              :prepare-cards     prepare-cards,
              :game-type         :sng,
              :size              3,
              :player-map        {"player-a"
                                  (m/make-player-state "player-a"
                                                       (bigint 100)
                                                       0),
                                  "player-b"
                                  (m/make-player-state "player-b"
                                                       (bigint 100)
                                                       1),
                                  "player-c"
                                  ;; Is it necessary to have drop-count for SNG game?
                                  (assoc (m/make-player-state "player-)"
                                                              (bigint 100)
                                                              2
                                                              :player-status/wait
                                                              :dropout)
                                         :drop-count
                                         1)},
              :btn               1,
              :op-player-ids     ["player-a" "player-b"],
              :encrypt-player-id nil,
              :shuffle-player-id "player-a",
              :dispatch-event    [c/shuffle-timeout-delay
                                  (m/make-event :system/shuffle-timeout)]}]
            (t/async done
              (go
               (t/is (= state-after (<! (sut/handle-event state-before event))))
               (done)))))

        (t/testing "Start tournament game"
          (let
            [state-before {:status     :game-status/init,
                           :game-type  :tournament,
                           :size       3,
                           :start-time 1000000,
                           :player-map {"player-a"
                                        (m/make-player-state "player-a"
                                                             (bigint 100)
                                                             0),
                                        "player-b"
                                        (m/make-player-state "player-b"
                                                             (bigint 100)
                                                             1),
                                        "player-c"
                                        (m/make-player-state "player-)"
                                                             (bigint 100)
                                                             2
                                                             :player-status/wait
                                                             :dropout)},
                           :btn        0}
             state-after
             {:status            :game-status/shuffle,
              :start-time        1000000,
              :prepare-cards     prepare-cards,
              :game-type         :tournament,
              :size              3,
              :player-map        {"player-a"
                                  (m/make-player-state "player-a"
                                                       (bigint 100)
                                                       0),
                                  "player-b"
                                  (m/make-player-state "player-b"
                                                       (bigint 100)
                                                       1),
                                  "player-c"
                                  ;; Is it necessary to have drop-count for SNG game?
                                  (assoc (m/make-player-state "player-)"
                                                              (bigint 100)
                                                              2
                                                              :player-status/wait
                                                              :dropout)
                                         :drop-count
                                         1)},
              :btn               1,
              :op-player-ids     ["player-a" "player-b"],
              :encrypt-player-id nil,
              :shuffle-player-id "player-a",
              :dispatch-event    [c/shuffle-timeout-delay
                                  (m/make-event :system/shuffle-timeout)]}]
            (t/async done
              (go
               (t/is (= state-after (<! (sut/handle-event state-before event))))
               (done)))))))))

;; Event type: system/start-tournament-game
;; Event type: client/shuffle-cards
;; Event type: client/encrypt-cards
;; Event type: client/share-keys
;; Event type: system/key-share-timeout

;; Event type: system/shuffle-timeout
(t/deftest test-shuffle-timeout
  (t/testing "Err:"
    (t/testing "Invalid status"
      (let [state {}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
                (sut/handle-event state (m/make-event :system/shuffle-timeout)))))))

  (t/testing "Ok"
    (let [state-before {:status            :game-status/shuffle,
                        :player-map
                        {"player-a" (m/make-player-state "player-a" (bigint 100) 0),
                         "player-b" (m/make-player-state "player-b" (bigint 100) 0),
                         "player-c" (m/make-player-state "player-c" (bigint 100) 0)},
                        :shuffle-player-id "player-a"}
          state-after  {:status            :game-status/init,
                        :player-map
                        {"player-a" (m/make-player-state "player-a"
                                                         (bigint 100)
                                                         0
                                                         :player-status/fold
                                                         :dropout),
                         "player-b" (m/make-player-state "player-b" (bigint 100) 0),
                         "player-c" (m/make-player-state "player-c" (bigint 100) 0)},
                        :shuffle-player-id "player-a",
                        :dispatch-event    [c/reset-timeout-delay (m/make-event :system/reset)]}]
      (t/is (= state-after
               (sut/handle-event state-before (m/make-event :system/shuffle-timeout)))))))

;; Event type: system/encrypt-timeout

(t/deftest test-encrypt-timeout
  (t/testing "Err:"
    (t/testing "Invalid status"
      (let [state {}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
                (sut/handle-event state (m/make-event :system/encrypt-timeout)))))))

  (t/testing "Ok"
    (let [state-before {:status            :game-status/encrypt,
                        :player-map
                        {"player-a" (m/make-player-state "player-a" (bigint 100) 0),
                         "player-b" (m/make-player-state "player-b" (bigint 100) 0),
                         "player-c" (m/make-player-state "player-c" (bigint 100) 0)},
                        :encrypt-player-id "player-a"}
          state-after  {:status            :game-status/init,
                        :player-map
                        {"player-a" (m/make-player-state "player-a"
                                                         (bigint 100)
                                                         0
                                                         :player-status/fold
                                                         :dropout),
                         "player-b" (m/make-player-state "player-b" (bigint 100) 0),
                         "player-c" (m/make-player-state "player-c" (bigint 100) 0)},
                        :encrypt-player-id "player-a",
                        :dispatch-event    [c/reset-timeout-delay (m/make-event :system/reset)]}]
      (t/is (= state-after
               (sut/handle-event state-before (m/make-event :system/encrypt-timeout)))))))

;; Event type: system/player-action-timeout
;; Event type: client/ready


(t/deftest
  test-client-ready
  ;; (t/testing "Invalid rsa pub"
  ;;   (let [state-before {:status :game-status/play}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid RSA public key"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:ed-pub "ed-pub",
  ;;                                              :sig    "sig"}))))))

  ;; (t/testing "Invalid ed pub"
  ;;   (let [state-before {:status :game-status/play}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid ED25519 public key"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:rsa-pub "rsa-pub",
  ;;                                              :sig     "sig"}))))))

  ;; (t/testing "Invalid signature"
  ;;   (let [state-before {:status :game-status/play}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid signature"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:rsa-pub "rsa-pub",
  ;;                                              :ed-pub  "ed-pub"}))))))

  ;; (t/testing "Invalid player ID"
  ;;   (let [state-before {:status :game-status/play}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:rsa-pub "rsa-pub",
  ;;                                              :sig     "sig",
  ;;                                              :ed-pub  "ed-pub"}
  ;;                                             nil))))))

  ;; (t/testing "Invalid player status"
  ;;   (let [state-before {:status     :game-status/play,
  ;;                       :player-map {"player-a" (m/make-player-state
  ;;                                                "player-a"
  ;;                                                (bigint 10)
  ;;                                                0
  ;;                                                :player-status/fold
  ;;                                                :normal)}}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid player status"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:rsa-pub "rsa-pub",
  ;;                                              :sig     "sig",
  ;;                                              :ed-pub  "ed-pub"}
  ;;                                             "player-a"))))))

  ;; (t/testing "Can't replace existing keys"
  ;;   (let [state-before {:status      :game-status/play,
  ;;                       :player-map  {"player-a" (m/make-player-state
  ;;                                                 "player-a"
  ;;                                                 (bigint 10)
  ;;                                                 0)},
  ;;                       :rsa-pub-map {"player-a" "old-rsa-pub"}}]
  ;;     (t/is (thrown-with-msg? ExceptionInfo #"Can't update public key"
  ;;             (sut/handle-event state-before
  ;;                               (m/make-event :client/fix-keys
  ;;                                             {:rsa-pub "rsa-pub",
  ;;                                              :sig     "sig",
  ;;                                              :ed-pub  "ed-pub"}
  ;;                                             "player-a"))))))
)

;; Event type: client/fix-keys

;; TODO: missing the ok case
(t/deftest test-client-share-keys
  (t/testing "Err:"
    (t/testing "Invalid status"
      (let [state-before {}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
                (sut/handle-event state-before
                                  (m/make-event
                                   :client/share-keys))))))

    (t/testing "Invalid secret nonce"
      (let [state-before {:status           :game-status/key-share,
                          :secret-nonce-map {"player-a" "nonce-a"}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid secret nonce"
                (sut/handle-event state-before
                                  (m/make-event
                                   :client/share-keys
                                   {:secret-nonce "nonce-b"}
                                   "player-a"))))))

    (t/testing "Invalid share key"
      (let [state-before {:status             :game-status/key-share,
                          :require-key-idents #{["player-a" :community-card 0]
                                                ["player-a" :community-card 1]
                                                ["player-b" :community-card 3]},
                          :secret-nonce-map   {"player-a" "nonce-a"}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid share key"
                (sut/handle-event state-before
                                  (m/make-event
                                   :client/share-keys
                                   {:secret-nonce "nonce-a",
                                    :share-keys   {["player-a" :community-card 0] "secret-0",
                                                   ["player-a" :community-card 1] "secret-1",
                                                   ["player-a" :community-card 2] "secret-2"}}
                                   "player-a"))))))

    (t/testing "Empty share key"
      (let [state-before {:status             :game-status/key-share,
                          :require-key-idents #{["player-a" :community-card 0]
                                                ["player-a" :community-card 1]
                                                ["player-b" :community-card 3]},
                          :secret-nonce-map   {"player-a" "nonce-a"}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Empty share key"
                (sut/handle-event state-before
                                  (m/make-event
                                   :client/share-keys
                                   {:secret-nonce "nonce-a",
                                    :share-keys   {}}
                                   "player-a"))))))

    ;; TODO
    ;; (t/testing "Invalid signature"
    ;;   (let [state-before {:status             :game-status/key-share,
    ;;                       :require-key-idents #{["player-a" :community-card 0]
    ;;                                             ["player-a" :community-card 1]
    ;;                                             ["player-b" :community-card 3]},
    ;;                       :secret-nonce-map   {"player-a" "nonce-a"}}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Empty share key"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event
    ;;                                :client/share-keys
    ;;                                {:secret-nonce "nonce-a",
    ;;                                 :share-keys   {}}
    ;;                                "player-a"))))))



    ;; (t/testing "Invalid rsa pub"
    ;;   (let [state-before {:status :game-status/play}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid RSA public key"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:ed-pub "ed-pub",
    ;;                                              :sig    "sig"}))))))

    ;; (t/testing "Invalid ed pub"
    ;;   (let [state-before {:status :game-status/play}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid ED25519 public key"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:rsa-pub "rsa-pub",
    ;;                                              :sig     "sig"}))))))

    ;; (t/testing "Invalid signature"
    ;;   (let [state-before {:status :game-status/play}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid signature"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:rsa-pub "rsa-pub",
    ;;                                              :ed-pub  "ed-pub"}))))))

    ;; (t/testing "Invalid player ID"
    ;;   (let [state-before {:status :game-status/play}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:rsa-pub "rsa-pub",
    ;;                                              :sig     "sig",
    ;;                                              :ed-pub  "ed-pub"}
    ;;                                             nil))))))

    ;; (t/testing "Invalid player status"
    ;;   (let [state-before {:status     :game-status/play,
    ;;                       :player-map {"player-a" (m/make-player-state
    ;;                                                "player-a"
    ;;                                                (bigint 10)
    ;;                                                0
    ;;                                                :player-status/fold
    ;;                                                :normal)}}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Invalid player status"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:rsa-pub "rsa-pub",
    ;;                                              :sig     "sig",
    ;;                                              :ed-pub  "ed-pub"}
    ;;                                             "player-a"))))))

    ;; (t/testing "Can't replace existing keys"
    ;;   (let [state-before {:status      :game-status/play,
    ;;                       :player-map  {"player-a" (m/make-player-state
    ;;                                                 "player-a"
    ;;                                                 (bigint 10)
    ;;                                                 0)},
    ;;                       :rsa-pub-map {"player-a" "old-rsa-pub"}}]
    ;;     (t/is (thrown-with-msg? ExceptionInfo #"Can't update public key"
    ;;             (sut/handle-event state-before
    ;;                               (m/make-event :client/fix-keys
    ;;                                             {:rsa-pub "rsa-pub",
    ;;                                              :sig     "sig",
    ;;                                              :ed-pub  "ed-pub"}
    ;;                                             "player-a"))))))
  ))

;; Event type: system/alive
(t/deftest test-system-alive
  (t/testing "Err:"
    (t/testing "Player ID is missing"
      (let [state-before {:player-map {"player-a" (m/make-player-state
                                                   "player-a"
                                                   (bigint 100)
                                                   0)}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state-before (m/make-event :system/alive))))))

    (t/testing "Invalid player ID"
      (let [state-before {:player-map {"player-a" (m/make-player-state
                                                   "player-a"
                                                   (bigint 100)
                                                   0)}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state-before (m/make-event :system/alive {} "player-b"))))))

    (t/testing "Invalid game status"
      (let [state-before {:player-map {"player-a" (m/make-player-state
                                                   "player-a"
                                                   (bigint 100)
                                                   0
                                                   :player-status/wait
                                                   :dropout)}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
                (sut/handle-event state-before (m/make-event :system/alive {} "player-a")))))))

  (t/testing "Ok"
    (let [state-before
          {:status     :game-status/play,
           :player-map {"player-a" (m/make-player-state
                                    "player-a"
                                    (bigint 100)
                                    0
                                    :player-status/wait
                                    :dropout)}}
          state-after
          {:status          :game-status/play,
           :player-map      {"player-a" (m/make-player-state
                                         "player-a"
                                         (bigint 100)
                                         0
                                         :player-status/wait
                                         :normal)},
           :reserve-timeout true}]
      (t/is
       (= state-after
          (sut/handle-event state-before (m/make-event :system/alive {} "player-a")))))))

;; Event type: system/dropout
(t/deftest test-system-dropout
  (t/testing "Err:"
    (t/testing "Player ID is missing"
      (let [state-before {:player-map {"player-a" (m/make-player-state
                                                   "player-a"
                                                   (bigint 100)
                                                   0)}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state-before (m/make-event :system/dropout))))))

    (t/testing "Invalid player ID"
      (let [state-before {:player-map {"player-a" (m/make-player-state
                                                   "player-a"
                                                   (bigint 100)
                                                   0)}}]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state-before (m/make-event :system/dropout {} "player-b")))))))

  (t/testing "Ok"
    (let [state-before {:player-map {"player-a" (m/make-player-state
                                                 "player-a"
                                                 (bigint 100)
                                                 0)}}
          state-after  {:player-map      {"player-a" (m/make-player-state
                                                      "player-a"
                                                      (bigint 100)
                                                      0
                                                      :player-status/wait
                                                      :dropout)},
                        :reserve-timeout true}]
      (t/is
       (= state-after
          (sut/handle-event state-before (m/make-event :system/dropout {} "player-a")))))))

;; Event type: client/leave

(t/deftest test-client-leave
  (t/testing "Err:"
    (t/testing "Invalid player id"
      (let [state {}
            event (m/make-event :client/leave)]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state event))))

      (let [state {:player-map {"player-a" (m/make-player-state "player-a" (bigint 100) 0)}}
            event (m/make-event :client/leave {} "player-b")]
        (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
                (sut/handle-event state event)))))

    (t/testing "Can't leave a running SNG"
      (let [state {:game-type  :sng,
                   ;; indicate the game is already started
                   :start-time 1000000,
                   :player-map {"player-a" (m/make-player-state "player-a" (bigint 100) 0),
                                "player-b" (m/make-player-state "player-b" (bigint 100) 0)}}
            event (m/make-event :client/leave {} "player-a")]
        (t/is (thrown-with-msg? ExceptionInfo #"Can't leave game"
                (sut/handle-event state event))))))

  (t/testing "Ok:"
    (t/testing "Game is not running, leave immediately"
      (let [state-before  {:game-type  :cash,
                           :player-map {"player-a" (m/make-player-state "player-a" (bigint 100) 0),
                                        "player-b" (m/make-player-state "player-b" (bigint 100) 0)}}

            state-after   {:game-type       :cash,
                           :player-map      {"player-a" (m/make-player-state "player-a"
                                                                             (bigint 100)
                                                                             0
                                                                             :player-status/fold
                                                                             :leave),
                                             "player-b" (m/make-player-state "player-b"
                                                                             (bigint 100)
                                                                             0)},
                           :reserve-timeout true}

            event         (m/make-event :client/leave {} "player-a")

            st-b-init     (assoc state-before :status :game-status/init)
            st-a-init     (assoc state-after :status :game-status/init)
            st-b-settle   (assoc state-before :status :game-status/settle)
            st-a-settle   (assoc state-after :status :game-status/settle)
            st-b-showdown (assoc state-before :status :game-status/showdown)
            st-a-showdown (assoc state-after :status :game-status/showdown)]
        (t/are [st-b sta]
         (= sta (sut/handle-event st-b event))

         st-b-init     st-a-init
         st-b-settle   st-a-settle
         st-b-showdown st-a-showdown)))

    (t/testing "The last player win immediately"
      (let [state-before {:game-type  :cash,
                          :status     :game-status/key-share,
                          :base-sb    (bigint 50),
                          :sb         (bigint 50),
                          :base-bb    (bigint 100),
                          :bb         (bigint 100),
                          :pots       [{:owner-ids #{"player-a" "player-b"},
                                        :amount    (bigint 200)}],
                          :street-bet (bigint 100),
                          :bet-map    {"player-a" (bigint 100),
                                       "player-b" (bigint 100)},
                          :player-map {"player-a" (m/make-player-state "player-a"
                                                                       (bigint 100)
                                                                       0),
                                       "player-b" (m/make-player-state "player-b"
                                                                       (bigint 100)
                                                                       1)}}
            state-after  {:game-type        :cash,
                          :status           :game-status/settle,
                          :base-sb          (bigint 50),
                          :sb               (bigint 50),
                          :base-bb          (bigint 100),
                          :bb               (bigint 100),
                          :rake-map         nil,
                          :ed-pub-map       nil,
                          :rsa-pub-map      nil,
                          :sig-map          nil,
                          :prize-map        {"player-b" (bigint 400)},
                          :pots
                          [{:owner-ids  #{"player-a" "player-b"},
                            :amount     (bigint 400),
                            :winner-ids #{"player-b"}}],
                          :street-bet
                          (bigint 100),
                          :bet-map
                          nil,
                          :chips-change-map
                          {"player-a" (bigint -200),
                           "player-b" (bigint 200)},
                          :player-map
                          {"player-b" (m/make-player-state "player-b"
                                                           (bigint 500)
                                                           1)},
                          :winning-type     :last-player,
                          :dispatch-event
                          [c/reset-timeout-delay (m/make-event :system/reset)]}
            event        (m/make-event :client/leave {} "player-a")]
        (t/is (= state-after
                 (dissoc (sut/handle-event state-before event)
                         :display
                         :api-requests)))))))

;; Event type: player/fold
;; Event type: player/call
;; Event type: player/check
;; Event type: player/raise
;; Event type: player/bet
;; Event type: system/next-game
;; Event type: system/resit-table
;; Event type: system/blinds-out
