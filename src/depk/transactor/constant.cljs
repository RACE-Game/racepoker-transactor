(ns depk.transactor.constant)

(def version "29")

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
(def tournament-start-delay (* 60 1000))

(def increase-blinds-interval (* 5 60 1000))
(def shutdown-game-delay 5000)

(def level-info-map
  {:nl100  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl200  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl500  {:sb (js/BigInt 2), :bb (js/BigInt 5)},
   :nl1000 {:sb (js/BigInt 5), :bb (js/BigInt 10)}})

(def blinds-structure
  [[50 100 5]
   [100 200 5]
   [150 300 5]
   [300 600 5]
   [400 800 5]
   [500 1000 5]
   [650 1150 5]
   [800 1600 5]
   [900 1800 5]
   [1000 2000 5]
   [1100 2200 5]
   [1200 2400 5]
   ;; 1 hour
   [1400 2800 5]
   [1600 3200 5]
   [1800 3600 5]
   [2000 4000 5]
   [2250 4500 5]
   [2500 5000 5]
   [3000 6000 5]
   [3500 7000 5]
   [4000 8000 5]
   [4500 9000 5]
   [5000 10000 5]
   ;; 2 hour
   [5500 11000 5]
   [6000 12000 5]
   [6500 13000 5]
   [7000 14000 5]
   [7500 15000 5]
   [8000 16000 5]
   [8500 17000 5]
   [9000 18000 5]
   [9500 19000 5]
   [10000 20000 5]])
