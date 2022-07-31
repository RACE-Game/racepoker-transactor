(ns depk.transactor.constant)

(def version "37")

(def max-player-num 9)

(def continue-start-game-delay 2000)
(def default-start-game-delay 5000)
(def long-start-game-delay 30000)
(def key-share-timeout-delay 20000)     ; 20 secs
(def shuffle-timeout-delay 15000)       ; 15 secs
(def encrypt-timeout-delay 10000)       ; 10 secs
(def player-action-timeout-delay 30000) ; 30 secs
(def droupout-player-action-timeout-delay 5000)
(def sit-out-player-action-timeout-delay 1000)
(def reset-timeout-delay 1000)
(def sng-next-game-timeout-delay 30000)
(def blinds-out-reset-delay 20000)
(def max-drop-count 1)
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

(def sb-list
  [50 100 150 200 300 400 600 800 1000
   1200 1600 2000 2400 2800 3600 4400 5200 6000 7500 9000
   10500 12000 15000 18000 21000 24000 30000 36000 42000])

(def blinds-structure
  (->> sb-list
       (mapv (fn [sb]
               [sb (* sb 2) 5]))))
