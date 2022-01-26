(ns depk.transactor.game.event-handler.misc-test
  (:require
   [cljs.core.async :refer [go <!]]
   [cljs.core.async.interop :refer [<p!]]
   [depk.transactor.game.event-handler.misc :as sut]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.encrypt :as e]
   [cljs.test :as t
              :include-macros true]))

(t/deftest next-position-player

  (t/is (= [1 101]
           (sut/next-position-player
            {:player-map {100 {:position 0},
                          101 {:position 1}}}
            0)))

  (t/is (= [0 100]
           (sut/next-position-player
            {:player-map {100 {:position 0},
                          101 {:position 1}}}
            1)))

  (t/is (= [4 103]
           (sut/next-position-player
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            3)))

  (t/is (= [1 100]
           (sut/next-position-player
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            4)))

  (t/is (= [4 103]
           (sut/next-position-player
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          103 {:position 4}}}
            3))))

(t/deftest get-player-by-position
  (t/is (= {:position 3}
           (sut/get-player-by-position
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            3))))

(t/deftest next-player-id
  (t/is (= 102
           (sut/next-player-id
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            101)))

  (t/is (= 100
           (sut/next-player-id
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            103))))

(t/deftest btn-player-id?
  (t/is (true? (sut/btn-player-id?
                {:btn        2,
                 :player-map {100 {:position 1},
                              101 {:position 2},
                              102 {:position 3},
                              103 {:position 4}}}
                101))))

(t/deftest list-players-in-order
  (t/is (= [{:position 3}
            {:position 4}
            {:position 1}
            {:position 2}]
           (sut/list-players-in-order
            {:player-map {100 {:position 1},
                          101 {:position 2},
                          102 {:position 3},
                          103 {:position 4}}}
            2)))
  (t/is (= [{:position 3, :status :player-status/acted}
            {:position 4, :status :player-status/wait}
            {:position 1, :status :player-status/acted}]
           (sut/list-players-in-order
            {:player-map {100 {:position 1, :status :player-status/acted},
                          101 {:position 2, :status :player-status/fold},
                          102 {:position 3, :status :player-status/acted},
                          103 {:position 4, :status :player-status/wait}}}
            2
            #{:player-status/acted :player-status/wait}))))


(t/deftest get-player-hole-card-indices
  (t/is (= {100 [4 5],
            101 [0 1],
            102 [2 3]}
           (sut/get-player-hole-card-indices
            {:player-map {100 {:player-id 100,
                               :position  0},
                          101 {:player-id 101,
                               :position  1},
                          102 {:player-id 102,
                               :position  2}},
             :btn        0}))))

(t/deftest valid-key-ident?
  (t/is (true? (sut/valid-key-ident?
                {:require-key-idents #{[101 :hole-card 0]
                                       [100 :hole-card 0]}}
                [101 :hole-card 0]
                101))))

(t/deftest list-missing-key-idents
  (t/is (= #{[100 :hole-card 1]}
           (sut/list-missing-key-idents
            {:require-key-idents #{[101 :hole-card 0] [100 :hole-card 1]},
             :share-key-map      {[101 :hole-card 0] "KEY"}}))))

(t/deftest list-require-key-idents
  (t/is (= #{;; card 0 for 100
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
             [101 :hole-card 5 102]}
           (sut/list-require-key-idents
            {:btn        2,
             :street     :street/preflop,
             :player-map
             {100 (m/make-player-state 100 nil 0),
              101 (m/make-player-state 101 nil 1),
              102 (m/make-player-state 102 nil 2)}})))

  (t/is (= #{[100 :community-card 6]
             [101 :community-card 6]
             [102 :community-card 6]
             [100 :community-card 7]
             [101 :community-card 7]
             [102 :community-card 7]
             [100 :community-card 8]
             [101 :community-card 8]
             [102 :community-card 8]}
           (sut/list-require-key-idents
            {:btn        2,
             :street     :street/flop,
             :player-map
             {100 (m/make-player-state 100 nil 0),
              101 (m/make-player-state 101 nil 1),
              102 (m/make-player-state 102 nil 2)}})))

  (t/is (= #{[100 :community-card 9]
             [101 :community-card 9]
             [102 :community-card 9]}
           (sut/list-require-key-idents
            {:btn        2,
             :street     :street/turn,
             :player-map
             {100 (m/make-player-state 100 nil 0),
              101 (m/make-player-state 101 nil 1),
              102 (m/make-player-state 102 nil 2)}})))

  (t/is (= #{[100 :community-card 10]
             [101 :community-card 10]
             [102 :community-card 10]}
           (sut/list-require-key-idents
            {:btn        2,
             :street     :street/river,
             :player-map
             {100 (m/make-player-state 100 nil 0),
              101 (m/make-player-state 101 nil 1),
              102 (m/make-player-state 102 nil 2)}})))

  (t/is (= #{[100 :showdown-card 0]
             [101 :showdown-card 0]
             [102 :showdown-card 0]

             [100 :showdown-card 1]
             [101 :showdown-card 1]
             [102 :showdown-card 1]

             [100 :showdown-card 2]
             [101 :showdown-card 2]
             [102 :showdown-card 2]

             [100 :showdown-card 3]
             [101 :showdown-card 3]
             [102 :showdown-card 3]}
           (sut/list-require-key-idents
            {:btn        2,
             :street     :street/showdown,
             :player-map
             {100 {:player-id 100,
                   :status    :player-status/acted},
              101 {:player-id 101,
                   :status    :player-status/acted},
              102 {:player-id 102,
                   :status    :player-status/fold}}}))))

(t/deftest update-prize-map
  (let [state {:pots [(m/make-pot #{100 101 102} 1500 #{100})
                      (m/make-pot #{101 102} 1500 #{102})
                      (m/make-pot #{102} 1000 #{102})]}]
    (t/is (= {100 1500,
              102 2500}
             (-> state
                 (sut/update-prize-map)
                 :prize-map)))))

(t/deftest update-chips-change-map
  (let [state {:pots       [(m/make-pot #{100 101 102} 1500 #{102})
                            (m/make-pot #{101 102} 1500 #{102})],
               :player-map {100 {},
                            101 {},
                            102 {}}}]
    (t/is (= {100 -500,
              101 -1250,
              102 1750}
             (-> state
                 (sut/update-chips-change-map)
                 :chips-change-map)))))

(t/deftest take-bet-from-player
  (let [player (m/make-player-state 100 500 nil)]
    (t/is (= [200 (m/make-player-state 100 300 nil) false]
             (sut/take-bet-from-player player 200)))

    (t/is (= [500 (m/make-player-state 100 0 nil) true]
             (sut/take-bet-from-player player 500)))

    (t/is (= [500 (m/make-player-state 100 0 nil) true]
             (sut/take-bet-from-player player 2000)))))

(t/deftest blind-bets
  (let [state {:player-map {100 (m/make-player-state 100 1000 0),
                            101 (m/make-player-state 101 1000 1),
                            102 (m/make-player-state 102 1000 2)},
               :btn        2,
               :sb         100,
               :bb         200}]
    (t/is (= {:player-map       {100 (m/make-player-state 100 900 0),
                                 101 (m/make-player-state 101 800 1),
                                 102 (assoc (m/make-player-state 102 1000 2)
                                            :status
                                            :player-status/in-action)},
              :bet-map          {100 100,
                                 101 200},
              :btn              2,
              :sb               100,
              :bb               200,
              :action-player-id 102,
              :street-bet       200,
              :min-raise        200}
             (sut/blind-bets state))))
  (let [state {:player-map {100 (m/make-player-state 100 1000 0),
                            101 (m/make-player-state 101 1000 1)},
               :btn        0,
               :sb         100,
               :bb         200}]
    (t/is (= {:player-map       {100 (assoc (m/make-player-state 100 900 0)
                                            :status
                                            :player-status/in-action),
                                 101 (m/make-player-state 101 800 1)},
              :bet-map          {100 100,
                                 101 200},
              :btn              0,
              :sb               100,
              :bb               200,
              :action-player-id 100,
              :street-bet       200,
              :min-raise        200}
             (sut/blind-bets state)))))

(t/deftest apply-prize-map
  (let [state {:player-map {100 {:chips 10000},
                            101 {:chips 10000},
                            102 {:chips 10000}},
               :prize-map  {100 5000,
                            101 2000}}]
    (t/is (= {:player-map {100 {:chips 15000},
                           101 {:chips 12000},
                           102 {:chips 10000}},
              :prize-map  {100 5000,
                           101 2000}}
             (sut/apply-prize-map state)))))

(t/deftest assign-winner-to-pots
  (let [state {:pots [(m/make-pot #{100 101 102} 1000)
                      (m/make-pot #{100 101} 1000)]}]
    (t/is (= {:pots [(m/make-pot #{100 101 102} 1000 #{102})
                     (m/make-pot #{100 101} 1000 #{101})]}
             (sut/assign-winner-to-pots state [#{102} #{101} #{100}])))))

(t/deftest collect-bet-to-pots
  (let [state {:player-map {100 {:status :player-status/acted},
                            101 {:status :player-status/acted},
                            102 {:status :player-status/acted},
                            103 {:status :player-status/acted},
                            104 {:status :player-status/fold},
                            105 {:status :player-status/fold}},
               :bet-map    {100 500,
                            101 500,
                            102 800,
                            103 600,
                            104 600,
                            105 700}}]

    (t/is (= {:player-map {100 {:status :player-status/acted},
                           101 {:status :player-status/acted},
                           102 {:status :player-status/acted},
                           103 {:status :player-status/acted},
                           104 {:status :player-status/fold},
                           105 {:status :player-status/fold}},
              :bet-map    nil,
              :pots       [(m/make-pot #{100 101 102 103} 3000)
                           (m/make-pot #{103 102} 400)
                           (m/make-pot #{102} 300)]}
             (sut/collect-bet-to-pots state)))

    ;; with existing pots
    (t/is (= {:player-map {100 {:status :player-status/acted},
                           101 {:status :player-status/acted},
                           102 {:status :player-status/acted},
                           103 {:status :player-status/acted},
                           104 {:status :player-status/fold},
                           105 {:status :player-status/fold}},
              :bet-map    nil,
              :pots       [(m/make-pot #{100 101 102 103} 200)
                           (m/make-pot #{100 101 102 103} 3000)
                           (m/make-pot #{103 102} 400)
                           (m/make-pot #{102} 300)]}
             (sut/collect-bet-to-pots
              (assoc state :pots [(m/make-pot #{100 101 102 103} 200)]))))))

(t/deftest prepare-showdown
  (let [state {:player-map {100 (assoc (m/make-player-state 100 10000 0)
                                       :status
                                       :player-status/fold),
                            101 (assoc (m/make-player-state 101 10000 1)
                                       :status
                                       :player-status/acted),
                            102 (assoc (m/make-player-state 102 0 2)
                                       :status
                                       :player-status/allin)},
               :bet-map    {101 10000,
                            102 10000},
               :btn        2,
               :pots       [(m/make-pot #{101 102} 10000)]}]
    (t/is (= {:street             :street/showdown,
              :status             :game-status/key-share,
              :after-key-share    :settle,
              :require-key-idents #{[101 :showdown-card 2]
                                    [102 :showdown-card 2]
                                    [100 :showdown-card 2]

                                    [101 :showdown-card 3]
                                    [102 :showdown-card 3]
                                    [100 :showdown-card 3]

                                    [101 :showdown-card 4]
                                    [102 :showdown-card 4]
                                    [100 :showdown-card 4]

                                    [101 :showdown-card 5]
                                    [102 :showdown-card 5]
                                    [100 :showdown-card 5]}}
             (-> (sut/prepare-showdown state)
                 (select-keys [:street :status :after-key-share :require-key-idents]))))))

(t/deftest prepare-runner
  (let [state {:player-map {100 (assoc (m/make-player-state 100 10000 0)
                                       :status
                                       :player-status/fold),
                            101 (assoc (m/make-player-state 101 10000 1)
                                       :status
                                       :player-status/acted),
                            102 (assoc (m/make-player-state 102 0 2)
                                       :status
                                       :player-status/allin)},
               :bet-map    {101 10000,
                            102 10000},
               :btn        2,
               :pots       [(m/make-pot #{101 102} 10000)],
               :street     :street/turn}]
    (t/is (= {:street             :street/showdown,
              :status             :game-status/key-share,
              :after-key-share    :runner,
              :require-key-idents #{[101 :showdown-card 2]
                                    [102 :showdown-card 2]
                                    [100 :showdown-card 2]

                                    [101 :showdown-card 3]
                                    [102 :showdown-card 3]
                                    [100 :showdown-card 3]

                                    [101 :showdown-card 4]
                                    [102 :showdown-card 4]
                                    [100 :showdown-card 4]

                                    [101 :showdown-card 5]
                                    [102 :showdown-card 5]
                                    [100 :showdown-card 5]

                                    [100 :community-card 9]
                                    [102 :community-card 6]
                                    [102 :community-card 10]
                                    [100 :community-card 7]
                                    [100 :community-card 6]
                                    [100 :community-card 8]
                                    [101 :community-card 7]
                                    [100 :community-card 10]
                                    [102 :community-card 7]
                                    [101 :community-card 6]
                                    [102 :community-card 8]
                                    [101 :community-card 8]
                                    [101 :community-card 10]
                                    [101 :community-card 9]
                                    [102 :community-card 9]}}
             (-> (sut/prepare-runner state)
                 (select-keys [:street :status :after-key-share :require-key-idents]))))))

(t/deftest single-player-win
  (let [state {:player-map {100 {:status :player-status/fold, :chips 10000},
                            101 {:status :player-status/acted, :chips 10000},
                            102 {:status :player-status/fold, :chips 10000}},
               :pots       [(m/make-pot #{100 101 102} 3000)
                            (m/make-pot #{101} 2000)]}]
    (t/is (= {:player-map       {100 {:status :player-status/fold,
                                      :chips  10000},
                                 101 {:status :player-status/acted,
                                      :chips  15000},
                                 102 {:status :player-status/fold,
                                      :chips  10000}},
              :prize-map        {101 5000},
              :chips-change-map {100 -1000,
                                 101 2000,
                                 102 -1000},
              :pots             [(m/make-pot #{100 101 102} 3000 #{101})
                                 (m/make-pot #{101} 2000 #{101})],
              :status           :game-status/settle,
              :bet-map          nil,
              :api-requests     [{:api-request/type :settle-finished-game,
                                  :chips-change-map {100 -1000,
                                                     101 2000,
                                                     102 -1000}}]}
             (sut/single-player-win state 101)))

    ;; with blind bets
    (let [state-with-bet-map (assoc state
                                    :bet-map {100 50, 101 100}
                                    :pots    [])]
      (t/is (= {:player-map       {100 {:status :player-status/fold,
                                        :chips  10000},
                                   101 {:status :player-status/acted,
                                        :chips  10150},
                                   102 {:status :player-status/fold,
                                        :chips  10000}},
                :prize-map        {101 150},
                :chips-change-map {100 -50,
                                   101 50,
                                   102 0},
                :bet-map          nil,
                :pots             [(m/make-pot #{100 101} 150 #{101})],
                :status           :game-status/settle,
                :api-requests     [{:api-request/type :settle-finished-game,
                                    :chips-change-map {100 -50,
                                                       101 50,
                                                       102 0}}]}
               (sut/single-player-win state-with-bet-map 101))))

    ;; with normal bets
    (let [state-with-bet-map (assoc state
                                    :bet-map
                                    {100 500,
                                     101 1000,
                                     102 500})]
      (t/is (= {:player-map       {100 {:status :player-status/fold,
                                        :chips  10000},
                                   101 {:status :player-status/acted,
                                        :chips  17000},
                                   102 {:status :player-status/fold,
                                        :chips  10000}},
                :prize-map        {101 7000},
                :chips-change-map {100 -1500,
                                   101 3000,
                                   102 -1500},
                :bet-map          nil,
                :pots             [(m/make-pot #{100 101 102} 3000 #{101})
                                   (m/make-pot #{101} 2000 #{101})
                                   (m/make-pot #{100 101 102} 2000 #{101})],
                :status           :game-status/settle,
                :api-requests     [{:api-request/type :settle-finished-game,
                                    :chips-change-map {100 -1500,
                                                       101 3000,
                                                       102 -1500}}]}
               (sut/single-player-win state-with-bet-map 101))))))

(t/deftest next-state-case
  (t/testing "blind bets"
    (t/is (= [:blind-bets]
             (sut/next-state-case
              {:player-map {100 (m/make-player-state 100 1000 0),
                            101 (m/make-player-state 101 1000 1),
                            102 (m/make-player-state 102 1000 2)},
               :btn        2,
               :street     :street/preflop,
               :sb         100,
               :bb         200}))))

  (t/testing "single player win"
    (t/is
     (= [:single-player-win 100]
        (sut/next-state-case
         {:player-map {100 (m/make-player-state 100 1000 0),
                       101 (assoc (m/make-player-state 101 1000 1) :status :player-status/fold),
                       102 (assoc (m/make-player-state 102 1000 2) :status :player-status/fold)},
          :btn        2,
          :bet-map    {100 300},
          :street     :street/preflop,
          :sb         100,
          :bb         200}))))

  (t/testing "ask player for action"
    (t/is
     (= [:ask-player-for-action 101]
        (sut/next-state-case
         {:player-map       {100 (m/make-player-state 100 1000 0),
                             101 (assoc (m/make-player-state 101 1000 1)
                                        :status
                                        :player-status/wait)},
          :btn              0,
          :bet-map          {100 200},
          :street           :street/preflop,
          :street-bet       200,
          :min-raise        200,
          :sb               100,
          :bb               200,
          :action-player-id 100})))
    (t/is
     (= [:ask-player-for-action 101]
        (sut/next-state-case
         {:player-map       {100 (m/make-player-state 100 1000 0),
                             101 (assoc (m/make-player-state 101 1000 1)
                                        :status
                                        :player-status/acted),
                             102 (assoc (m/make-player-state 102 1000 2)
                                        :status
                                        :player-status/fold)},
          :btn              2,
          :bet-map          {100 400, 101 200},
          :street           :street/preflop,
          :sb               100,
          :street-bet       400,
          :min-raise        200,
          :bb               200,
          :action-player-id 100})))
    (t/is
     (= [:ask-player-for-action 101]
        (sut/next-state-case
         {:player-map       {100 (m/make-player-state 100 1000 0),
                             101 (assoc (m/make-player-state 101 1000 1)
                                        :status
                                        :player-status/wait),
                             102 (assoc (m/make-player-state 102 1000 2)
                                        :status
                                        :player-status/wait)},
          :btn              2,
          :bet-map          {100 200, 101 200},
          :street           :street/preflop,
          :sb               100,
          :street-bet       400,
          :min-raise        200,
          :bb               200,
          :action-player-id 100}))))

  (t/testing "runner"
    (t/is
     (= [:runner]
        (sut/next-state-case
         {:player-map       {100 (assoc (m/make-player-state 100 0 0)
                                        :status
                                        :player-status/allin),
                             101 (assoc (m/make-player-state 101 0 1)
                                        :status
                                        :player-status/allin),
                             102 (assoc (m/make-player-state 102 1000 2)
                                        :status
                                        :player-status/acted)},
          :btn              2,
          :bet-map          {100 400, 101 300, 102 400},
          :street           :street/turn,
          :sb               100,
          :street-bet       400,
          :min-raise        200,
          :bb               200,
          :action-player-id 100}))))

  (t/testing "change street"
    (t/is
     (= [:change-street :street/river]
        (sut/next-state-case
         {:player-map       {100 (assoc (m/make-player-state 100 1000 0)
                                        :status
                                        :player-status/acted),
                             101 (assoc (m/make-player-state 101 1000 1)
                                        :status
                                        :player-status/acted),
                             102 (assoc (m/make-player-state 102 1000 2)
                                        :status
                                        :player-status/fold)},
          :btn              2,
          :bet-map          {100 400, 101 400},
          :street           :street/turn,
          :sb               100,
          :street-bet       400,
          :min-raise        200,
          :bb               200,
          :action-player-id 100}))))

  (t/testing "showdown"
    (t/is
     (= [:showdown]
        (sut/next-state-case
         {:player-map       {100 (assoc (m/make-player-state 100 1000 0)
                                        :status
                                        :player-status/acted),
                             101 (assoc (m/make-player-state 101 1000 1)
                                        :status
                                        :player-status/acted),
                             102 (assoc (m/make-player-state 102 1000 2)
                                        :status
                                        :player-status/fold)},
          :btn              2,
          :bet-map          {100 400, 101 400},
          :street           :street/river,
          :sb               100,
          :street-bet       400,
          :min-raise        200,
          :bb               200,
          :action-player-id 100})))))

(t/deftest settle
  (t/async done
    (go
     (let [state
           {:community-cards [[:h :a] [:d :a] [:c :a] [:s :j] [:s :t]],
            :pots            [(m/make-pot #{100 101} 2000)],
            :card-ciphers    "9e3f-3a42-0056-2ac3",
            :share-key-map   {[100 :showdown-card 0] "24918f4ffc066105629863d68a92596b",
                              [100 :showdown-card 1] "c35ad7e22a9a7164e7980ad3c16b39c7",
                              [100 :showdown-card 2] "69478fb7684ca45644630ce173c76200",
                              [100 :showdown-card 3] "294dfbaeb33849ab44689163a335ebcc",

                              [101 :showdown-card 0] "af65251070d5a0386b2e7c97881a978c",
                              [101 :showdown-card 1] "72dc6cfefd32565c610b5cd892bc1f7d",
                              [101 :showdown-card 2] "d5f9ea1a7f4c0ca3eab9e239cea0ae8f",
                              [101 :showdown-card 3] "867f57da487c4f9672359c70ac528271"},
            :player-map      {100 (assoc (m/make-player-state 100 10000 0)
                                         :status
                                         :player-status/acted),
                              101 (assoc (m/make-player-state 101 10000 1)
                                         :status
                                         :player-status/acted)},
            :btn             1}]
       (t/is
        (=
         {:prize-map        {100 2000},
          :chips-change-map {100 1000,
                             101 -1000},
          :showdown-map     {100 {:category   :four-of-a-kind,
                                  :picks      [[:s :a] [:h :a] [:d :a] [:c :a] [:s :k]],
                                  :value      [7 14 14 14 14 13],
                                  :player-id  100,
                                  :hole-cards [[:s :a] [:s :k]]},

                             101 {:category   :full-house,
                                  :picks      [[:h :a] [:d :a] [:c :a] [:s :q] [:d :q]],
                                  :value      [6 14 14 14 12 12],
                                  :player-id  101,
                                  :hole-cards [[:s :q] [:d :q]]}},
          :pots             [(m/make-pot #{100 101} 2000 #{100})]}
         (let [state (<! (sut/settle state))]
           (select-keys state [:showdown-map :pots :prize-map :chips-change-map])))))
     (done))))
