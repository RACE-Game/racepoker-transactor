(ns depk.transactor.game.event-handler-test
  (:require
   [depk.transactor.game.event-handler :as sut]
   [depk.transactor.game.models :as m]
   [cljs.core.async :refer [go <!]]
   [cljs.test :as t
              :include-macros true]))

(t/deftest system-sync-state
  (t/testing "failure when game-state is not prepare"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status :game-status/play))]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
              (sut/handle-event state (m/make-event :system/sync-state state))))))

  (t/testing "success with enough players to start"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status :game-status/init))
          expected-player-map {"100" (m/make-player-state "100" 1000 0)}
          event (m/make-event
                 :system/sync-state
                 state
                 {:players [{:pubkey 100, :chips 1000}]})]
      (t/is (= {:player-map      expected-player-map,
                :dispatch-events nil}
               (-> (sut/handle-event state event)
                   (select-keys [:player-map :dispatch-events]))))))

  (t/testing "success with enough players to start"
    (let [state      (-> (m/make-game-state {:btn 0} {})
                         (assoc :status :game-status/init))
          player-map {"100" (m/make-player-state "100" 1000 0),
                      "101" (m/make-player-state "101" 10000 2)}
          event      (m/make-event
                      :system/sync-state
                      state
                      {:players [{:pubkey "100", :chips 1000} nil {:pubkey "101", :chips 10000}]})]
      (t/is (= {:player-map      player-map,
                :dispatch-events {1000 (m/make-event :system/start-game state {:btn 2})}}
               (-> (sut/handle-event state event)
                   (select-keys [:player-map :dispatch-events])))))))

(def state-with-players
  (-> (m/make-game-state {:btn 0} {})
      (assoc :player-map
             {100 (m/make-player-state 100 10000 0),
              101 (m/make-player-state 101 10000 1),
              102 (m/make-player-state 102 10000 2)}
             :btn        0)))

(t/deftest system-start-game

  (t/testing "failure"
    (let [state (-> state-with-players
                    (assoc :status :game-status/play))
          event (m/make-event :system/start-game state {:btn 1})]

      (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
              (-> (sut/handle-event state event)
                  (select-keys [:player-map :dispatch-events :shuffle-player-id]))))))

  (t/async done
    (go
     (t/testing "success"
       (let [state (-> state-with-players
                       (assoc :status :game-status/init))
             event (m/make-event :system/start-game state {:btn 1})]

         (t/is (= {:player-map        (:player-map state),
                   :shuffle-player-id 101,
                   :dispatch-events   nil}
                  (-> (<! (sut/handle-event state event))
                      (select-keys [:player-map :dispatch-events :shuffle-player-id]))))))
     (done))))


(def state-game-started
  (-> (m/make-game-state {:btn 0} {})
      (assoc :player-map    {100 (m/make-player-state 100 10000 0),
                             101 (m/make-player-state 101 10000 1),
                             102 (m/make-player-state 102 10000 2)}
             :btn           0
             :status        :game-status/shuffle
             :prepare-cards [{:data      "INIT DATA",
                              :op        :init,
                              :player-id nil}]
             :shuffle-player-id 100)))

(t/deftest player-shuffle-cards

  (t/testing "failure"
    (let [state state-game-started
          event (m/make-event :client/shuffle-cards state {:data "ENCRYPTED DATA"} 101)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
              (sut/handle-event state event)))))

  (t/testing "success"
    (let [state state-game-started
          event (m/make-event :client/shuffle-cards state {:data "ENCRYPTED DATA"} 100)]
      (t/is (= {:prepare-cards     [{:data      "INIT DATA",
                                     :op        :init,
                                     :player-id nil}
                                    {:data      "ENCRYPTED DATA",
                                     :op        :shuffle,
                                     :player-id 100}],
                :shuffle-player-id 101}
               (-> (sut/handle-event state event)
                   (select-keys [:prepare-cards :shuffle-player-id]))))))

  (t/testing "success, all clients"
    (let [state   state-game-started
          event-1 (m/make-event :client/shuffle-cards
                                state
                                {:data "ENCRYPTED DATA 1"}
                                100)
          event-2 (m/make-event :client/shuffle-cards
                                state
                                {:data "ENCRYPTED DATA 2"}
                                101)
          event-3 (m/make-event :client/shuffle-cards
                                state
                                {:data "ENCRYPTED DATA 3"}
                                102)]
      (t/is
       (= {:prepare-cards     [{:data      "INIT DATA",
                                :op        :init,
                                :player-id nil}
                               {:data      "ENCRYPTED DATA 1",
                                :op        :shuffle,
                                :player-id 100}
                               {:data      "ENCRYPTED DATA 2",
                                :op        :shuffle,
                                :player-id 101}
                               {:data      "ENCRYPTED DATA 3",
                                :op        :shuffle,
                                :player-id 102}],
           :shuffle-player-id nil,
           :encrypt-player-id 100,
           :status            :game-status/encrypt}
          (-> state
              (sut/handle-event event-1)
              (sut/handle-event event-2)
              (sut/handle-event event-3)
              (select-keys [:prepare-cards :shuffle-player-id :encrypt-player-id :status])))))))


(def state-cards-shuffled
  (-> (m/make-game-state {:btn 0} {})
      (assoc :player-map    {100 (m/make-player-state 100 10000 0),
                             101 (m/make-player-state 101 10000 1),
                             102 (m/make-player-state 102 10000 2)}
             :btn           0
             :status        :game-status/encrypt
             :prepare-cards []
             :encrypt-player-id 100)))

(t/deftest client-encrypt-cards

  (t/testing "failure"
    (let [state state-cards-shuffled
          event (m/make-event :client/encrypt-cards state {:data "ENCRYPTED DATA"} 101)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid player id"
              (sut/handle-event state event)))))

  (t/testing "success"
    (t/async done
      (go
       (let [state state-cards-shuffled
             event (m/make-event :client/encrypt-cards state {:data "ENCRYPTED DATA"} 100)]
         (t/is (= {:prepare-cards     [{:data      "ENCRYPTED DATA",
                                        :op        :encrypt,
                                        :player-id 100}],
                   :encrypt-player-id 101}
                  (-> (<! (sut/handle-event state event))
                      (select-keys [:prepare-cards :encrypt-player-id])))))
       (done))))

  (t/testing "success, all clients"
    (t/async done
      (go
       (let [state   state-cards-shuffled
             event-1 (m/make-event :client/encrypt-cards
                                   state
                                   {:data "ENCRYPTED DATA 1"}
                                   100)
             event-2 (m/make-event :client/encrypt-cards
                                   state
                                   {:data "ENCRYPTED DATA 2"}
                                   101)
             event-3 (m/make-event :client/encrypt-cards
                                   state
                                   {:data "ENCRYPTED DATA 3"}
                                   102)]
         (t/is
          (= {:prepare-cards      [{:data      "ENCRYPTED DATA 1",
                                    :op        :encrypt,
                                    :player-id 100}
                                   {:data      "ENCRYPTED DATA 2",
                                    :op        :encrypt,
                                    :player-id 101}
                                   {:data      "ENCRYPTED DATA 3",
                                    :op        :encrypt,
                                    :player-id 102}],
              :encrypt-player-id  nil,
              :status             :game-status/key-share,
              :after-key-share    :init-street,
              :require-key-idents #{[100 :hole-card 0 101]
                                    [102 :hole-card 0 101]
                                    [100 :hole-card 1 101]
                                    [102 :hole-card 1 101]
                                    [100 :hole-card 2 102]
                                    [101 :hole-card 2 102]
                                    [100 :hole-card 3 102]
                                    [101 :hole-card 3 102]
                                    [102 :hole-card 4 100]
                                    [101 :hole-card 4 100]
                                    [102 :hole-card 5 100]
                                    [101 :hole-card 5 100]}}
             (let [s1 (<! (sut/handle-event state event-1))
                   s2 (<! (sut/handle-event s1 event-2))
                   s3 (<! (sut/handle-event s2 event-3))]
               (-> s3
                   (select-keys [:require-key-idents :prepare-cards :encrypt-player-id
                                 :encrypt-player-id
                                 :status :after-key-share]))))))
       (done)))))

(def state-before-share-keys
  (-> (m/make-game-state {:btn 0} {})
      (assoc :player-map {100 (m/make-player-state 100 10000 0),
                          101 (m/make-player-state 101 10000 1),
                          102 (m/make-player-state 102 10000 2)}
             :btn        2
             :status     :game-status/key-share
             ;; need six cards
             :require-key-idents #{;; card 0 for 100
                                   [101 :hole-card 0 100]
                                   [102 :hole-card 0 100]
                                   ;; card 1 for 100
                                   [101 :hole-card 1 100]
                                   [102 :hole-card 1 100]
                                   ;; card 2 for 101
                                   [100 :hole-card 2 101]
                                   [102 :hole-card 2 101]
                                   ;; card 3 for 101
                                   [100 :hole-card 3 101]
                                   [102 :hole-card 3 101]
                                   ;; card 0 for 102
                                   [100 :hole-card 4 102]
                                   [101 :hole-card 4 102]
                                   ;; card 1 for 102
                                   [100 :hole-card 5 102]
                                   [101 :hole-card 5 102]})))

(t/deftest client-share-keys
  (t/testing "failure with invalid key ident"
    (let [state state-before-share-keys
          event (m/make-event :client/share-keys
                              state
                              {:share-keys {[100 :hole-card 5 100] "KEY"}}
                              100)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid share key"
              (sut/handle-event state event)))))

  (t/testing "failure with invalid player-id"
    (let [state state-before-share-keys
          event (m/make-event :client/share-keys
                              state
                              {:share-keys {[100 :hole-card 5 102] "KEY"}}
                              101)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid share key"
              (sut/handle-event state event)))))

  (t/testing "failure with duplicated key ident"
    (let [state (-> state-before-share-keys
                    (assoc :share-key-map {[100 :hole-card 5 102] "KEY"}))
          event (m/make-event :client/share-keys
                              state
                              {:share-keys {[100 :hole-card 5 102] "KEY"}}
                              100)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid share key"
              (sut/handle-event state event)))))

  (t/testing "failure with invalid state"
    (let [state (-> state-before-share-keys
                    (assoc :status :game-status/play))
          event (m/make-event :client/share-keys
                              state
                              {:share-keys {[100 :hole-card 5 102] "KEY"}}
                              100)]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid game status"
              (sut/handle-event state event)))))

  (t/testing "success"
    (let [state (-> state-before-share-keys)
          event (m/make-event :client/share-keys
                              state
                              {:share-keys {[100 :hole-card 5 102] "KEY"}}
                              100)]
      (t/is (= {:share-key-map {[100 :hole-card 5 102] "KEY"},
                :status        :game-status/key-share}
               (-> state
                   (sut/handle-event event)
                   (select-keys [:share-key-map :status]))))))

  (t/testing "success all"
    (let [state   (-> state-before-share-keys
                      (assoc :after-key-share :init-street))
          event-1 (m/make-event :client/share-keys
                                state
                                {:share-keys {[100 :hole-card 2 101] "KEY",
                                              [100 :hole-card 3 101] "KEY",
                                              [100 :hole-card 4 102] "KEY",
                                              [100 :hole-card 5 102] "KEY"}}
                                100)
          event-2 (m/make-event :client/share-keys
                                state
                                {:share-keys {[101 :hole-card 0 100] "KEY",
                                              [101 :hole-card 4 102] "KEY",
                                              [101 :hole-card 1 100] "KEY",
                                              [101 :hole-card 5 102] "KEY"}}
                                101)
          event-3 (m/make-event :client/share-keys
                                state
                                {:share-keys {[102 :hole-card 1 100] "KEY",
                                              [102 :hole-card 0 100] "KEY",
                                              [102 :hole-card 2 101] "KEY",
                                              [102 :hole-card 3 101] "KEY"}}
                                102)]
      (t/is (= {:share-key-map {[101 :hole-card 0 100] "KEY",
                                [102 :hole-card 0 100] "KEY",
                                [101 :hole-card 1 100] "KEY",
                                [102 :hole-card 1 100] "KEY",
                                [100 :hole-card 2 101] "KEY",
                                [102 :hole-card 2 101] "KEY",
                                [100 :hole-card 3 101] "KEY",
                                [102 :hole-card 3 101] "KEY",
                                [100 :hole-card 4 102] "KEY",
                                [101 :hole-card 4 102] "KEY",
                                [100 :hole-card 5 102] "KEY",
                                [101 :hole-card 5 102] "KEY"},
                :status        :game-status/play}
               (-> state
                   (sut/handle-event event-1)
                   (sut/handle-event event-2)
                   (sut/handle-event event-3)
                   (select-keys [:share-key-map :status])))))))

(t/deftest client-leave
  (t/testing "success, not the player in action"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :player-map       {100 {:status    :player-status/acted,
                                                   :player-id 100},
                                              200 {:status    :player-status/in-action,
                                                   :player-id 200}}
                           :status           :game-status/play
                           :action-player-id 200))]
      (t/is (= {:player-map {100 {:status        :player-status/fold,
                                  :player-id     100,
                                  :online-status :leave},
                             200 {:status    :player-status/in-action,
                                  :player-id 200}}}
               (-> state
                   (sut/handle-event (m/make-event :client/leave
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:player-map]))))))

  (t/testing "success, the player in action"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :player-map
                           {100 {:status    :player-status/in-action,
                                 :player-id 100},
                            200 {:status    :player-status/wait,
                                 :player-id 200},
                            300 {:status    :player-status/wait,
                                 :player-id 300}}))]
      (t/is (= {:player-map       {100 {:status        :player-status/fold,
                                        :player-id     100,
                                        :online-status :leave},
                                   200 {:status    :player-status/in-action,
                                        :player-id 200},
                                   300 {:status    :player-status/wait,
                                        :player-id 300}},
                :action-player-id 200}
               (-> state
                   (sut/handle-event (m/make-event :client/leave
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:player-map :action-player-id])))))))

(t/deftest player-fold
  (t/testing "success, ask next wait player for action"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100},
                                              101 {:status    :player-status/acted,
                                                   :player-id 101},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102}}
                           :status           :game-status/play
                           :action-player-id 100
                           :bet-map          {100 2000,
                                              101 3000,
                                              102 1000}))]
      (t/is (= {:player-map {100 {:status    :player-status/fold,
                                  :player-id 100},
                             101 {:status    :player-status/acted,
                                  :player-id 101},
                             102 {:status    :player-status/in-action,
                                  :player-id 102}}}
               (-> state
                   (sut/handle-event (m/make-event :player/fold
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:player-map])))))))

(t/deftest player-call
  (t/testing "success, normal call"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       3000
                           :action-player-id 100
                           :bet-map          {100 0,
                                              101 1500,
                                              102 3000}))]
      (t/is (= {:player-map {100 {:status    :player-status/acted,
                                  :player-id 100,
                                  :chips     7000},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    {100 3000,
                             101 1500,
                             102 3000}}
               (-> state
                   (sut/handle-event (m/make-event :player/call
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:bet-map :player-map]))))))

  (t/testing "success, call allin"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     2000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       3000
                           :action-player-id 100
                           :bet-map          {100 0,
                                              101 1500,
                                              102 3000}))]
      (t/is (= {:player-map {100 {:status    :player-status/allin,
                                  :player-id 100,
                                  :chips     0},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    {100 2000,
                             101 1500,
                             102 3000}}
               (-> state
                   (sut/handle-event (m/make-event :player/call
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:bet-map :player-map])))))))

(t/deftest player-check
  (t/testing "success, player has no bet"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       nil
                           :action-player-id 100))]
      (t/is (= {:player-map {100 {:status    :player-status/acted,
                                  :player-id 100,
                                  :chips     10000},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    nil}
               (-> state
                   (sut/handle-event (m/make-event :player/check
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:bet-map :player-map]))))))
  (t/testing "success, player has bet"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       2000
                           :action-player-id 100
                           :bet-map          {100 2000,
                                              101 1000,
                                              102 2000}))]
      (t/is (= {:player-map {100 {:status    :player-status/acted,
                                  :player-id 100,
                                  :chips     10000},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    {100 2000,
                             101 1000,
                             102 2000}}
               (-> state
                   (sut/handle-event (m/make-event :player/check
                                                   state
                                                   {}
                                                   100))
                   (select-keys [:bet-map :player-map])))))))

(t/deftest player-bet
  (t/testing "fail, can't bet"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       1000
                           :action-player-id 100))]
      (t/is (thrown-with-msg? ExceptionInfo #"Player can't bet"
              (-> state
                  (sut/handle-event (m/make-event :player/bet
                                                  state
                                                  {:amount 1000}
                                                  100)))))))
  (t/testing "fail, invalid amount"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       0
                           :action-player-id 100))]
      (t/is (thrown-with-msg? ExceptionInfo #"Invalid amount"
              (-> state
                  (sut/handle-event (m/make-event :player/bet
                                                  state
                                                  {:amount 1.1}
                                                  100)))))))
  (t/testing "fail, bet too small"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       0
                           :min-raise        1000
                           :action-player-id 100))]
      (t/is (thrown-with-msg? ExceptionInfo #"Player bet too small"
              (-> state
                  (sut/handle-event (m/make-event :player/bet
                                                  state
                                                  {:amount 500}
                                                  100)))))))
  (t/testing "success"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       0
                           :min-raise        1000
                           :action-player-id 100))]
      (t/is (= {:player-map {100 {:status    :player-status/acted,
                                  :player-id 100,
                                  :chips     8000},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    {100 2000},
                :min-raise  2000,
                :street-bet 2000}
               (-> state
                   (sut/handle-event (m/make-event :player/bet
                                                   state
                                                   {:amount 2000}
                                                   100))
                   (select-keys [:bet-map :player-map :street-bet :min-raise])))))))


(t/deftest player-raise
  (t/testing "fail, can't raise"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       0
                           :action-player-id 100))]
      (t/is (thrown-with-msg? ExceptionInfo #"Player can't raise"
              (-> state
                  (sut/handle-event (m/make-event :player/raise
                                                  state
                                                  {:amount 1000}
                                                  100)))))))

  (t/testing "fail, raise too small"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       2000
                           :min-raise        1000
                           :bet-map          {100 1200}
                           :action-player-id 100))]
      (t/is (thrown-with-msg? ExceptionInfo #"Player raise too small"
              (-> state
                  (sut/handle-event (m/make-event :player/raise
                                                  state
                                                  {:amount 1000}
                                                  100))
                  (select-keys [:bet-map :player-map]))))))
  (t/testing "success"
    (let [state (-> (m/make-game-state {:btn 0} {})
                    (assoc :status           :game-status/play
                           :player-map       {100 {:status    :player-status/in-action,
                                                   :player-id 100,
                                                   :chips     10000},
                                              101 {:status    :player-status/wait,
                                                   :player-id 101,
                                                   :chips     10000},
                                              102 {:status    :player-status/wait,
                                                   :player-id 102,
                                                   :chips     10000}}
                           :street-bet       2000
                           :min-raise        1000
                           :bet-map          {100 1200}
                           :action-player-id 100))]
      (t/is (= {:player-map {100 {:status    :player-status/acted,
                                  :player-id 100,
                                  :chips     8200},
                             101 {:status    :player-status/in-action,
                                  :player-id 101,
                                  :chips     10000},
                             102 {:status    :player-status/wait,
                                  :player-id 102,
                                  :chips     10000}},
                :bet-map    {100 3000},
                :min-raise  1000,
                :street-bet 3000}
               (-> state
                   (sut/handle-event (m/make-event :player/raise
                                                   state
                                                   {:amount 1800}
                                                   100))
                   (select-keys [:bet-map :player-map :street-bet :min-raise])))))))
