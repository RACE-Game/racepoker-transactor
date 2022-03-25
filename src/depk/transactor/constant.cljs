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
  {:NL100  {:sb 1, :bb 2},
   :NL200  {:sb 1, :bb 2},
   :NL500  {:sb 2, :bb 5},
   :NL1000 {:sb 5, :bb 10}})
