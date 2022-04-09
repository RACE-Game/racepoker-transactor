(ns depk.transactor.constant)

(def continue-start-game-delay 100)
(def default-start-game-delay 8000)
(def key-share-timeout-delay 3000)
(def shuffle-timeout-delay 15000)
(def encrypt-timeout-delay 3000)
(def player-action-timeout-delay 30000)
(def reset-timeout-delay 2000)

(def instruction-header-settle [3 1])

(def level-info-map
  {:nl100  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl200  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl500  {:sb (js/BigInt 2), :bb (js/BigInt 5)},
   :nl1000 {:sb (js/BigInt 5), :bb (js/BigInt 10)}})
