(ns depk.transactor.constant)

(def version "31")

(def max-player-num 9)

(def continue-start-game-delay 100)
(def default-start-game-delay 5000)
(def long-start-game-delay 30000)
(def key-share-timeout-delay 20000)     ; 20 secs
(def shuffle-timeout-delay 15000)       ; 15 secs
(def encrypt-timeout-delay 10000)       ; 10 secs
(def player-action-timeout-delay 30000) ; 30 secs
(def droupout-player-action-timeout-delay 5000)
(def reset-timeout-delay 1000)
(def sng-next-game-timeout-delay 30000)
(def blinds-out-reset-delay 20000)
(def max-drop-count 2)
(def tournament-start-delay
  (if goog.DEBUG
    (* 5 1000)
    (* 60 1000)))

(def increase-blinds-interval (* 5 60 1000))
(def shutdown-game-delay 5000)

(def level-info-map
  {:nl100  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl200  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl500  {:sb (js/BigInt 2), :bb (js/BigInt 5)},
   :nl1000 {:sb (js/BigInt 5), :bb (js/BigInt 10)}})

(def blinds-structure
  [[25 50 5]
   [50 100 5]
   [100 200 5]
   [150 300 5]
   [200 400 5]
   [250 500 5]
   [300 600 5]
   [400 800 5]
   [500 1000 5]
   [600 1200 5]
   [700 1400 5]
   [800 1600 5]
   [1000 2000 5]
   [1200 2400 5]
   [1400 2800 5]
   [1600 3200 5]
   [1800 3600 5]
   [2200 4400 5]
   [2600 5200 5]
   [3000 6000 5]
   [3400 6800 5]
   [3800 7600 5]
   [4600 9200 5]
   [5400 10800 5]
   [6200 12400 5]
   [7000 14000 5]
   [7800 15600 5]
   [9400 18800 5]])
