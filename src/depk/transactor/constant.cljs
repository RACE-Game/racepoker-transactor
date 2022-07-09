(ns depk.transactor.constant)

(def version "21")

(def max-player-num 9)

(def continue-start-game-delay 100)
(def default-start-game-delay 5000)
(def key-share-timeout-delay 60000)
(def shuffle-timeout-delay 15000)
(def encrypt-timeout-delay 10000)
(def player-action-timeout-delay 30000)
(def droupout-player-action-timeout-delay 5000)
(def reset-timeout-delay 1000)
(def sng-next-game-timeout-delay 30000)
(def max-drop-count 2)

(def increase-blinds-interval (* 5 60 1000))
(def shutdown-game-delay 5000)

(def level-info-map
  {:nl100  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl200  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl500  {:sb (js/BigInt 2), :bb (js/BigInt 5)},
   :nl1000 {:sb (js/BigInt 5), :bb (js/BigInt 10)}})
