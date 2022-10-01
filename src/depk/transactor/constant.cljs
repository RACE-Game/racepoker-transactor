(ns depk.transactor.constant)

(def version "50")

(def max-player-num 9)

(def default-start-game-delay 5500)
(def key-share-timeout-delay 20000)     ; 20 secs
(def shuffle-timeout-delay 10000)       ; 10 secs
(def encrypt-timeout-delay 10000)       ; 10 secs
(def postflop-player-action-timeout-delay 25000) ; 25 secs
(def preflop-player-action-timeout-delay 12000)  ; 12 secs
(def preflop-raised-action-timeout-delay 15000)  ; 15 secs
(def dropout-player-action-timeout-delay 5000)
(def sit-out-player-action-timeout-delay 1000)
(def reset-timeout-delay 50)
(def sng-next-game-timeout-delay 30000)
(def blinds-out-delay 12000)
(def max-drop-count 1)
(def tournament-start-delay
  (if goog.DEBUG
    (* 30 1000)
    (* 300 1000)))

(def blinds-interval-normal (* 15 60 1000))
(def blinds-interval-turbo (* 10 60 1000))
(def blinds-interval-hyper (* 5 60 1000))

(def shutdown-game-delay 5000)

(def level-info-map
  {:nl100  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl200  {:sb (js/BigInt 1), :bb (js/BigInt 2)},
   :nl500  {:sb (js/BigInt 2), :bb (js/BigInt 5)},
   :nl1000 {:sb (js/BigInt 5), :bb (js/BigInt 10)}})

(def sb-list
  [50 100 150 200 300 400 600 800 1000
   1200 1600 2000 2400 2800 3600 4400 5200 6000 7500 9000
   10500 12000 15000 18000 21000 24000 30000 36000 42000
   50000 58000 66000 72000
   80000
   90000
   100000
   110000
   120000
   140000
   160000
   180000
   200000
   240000
   280000
   320000
   360000
   400000
   440000
   500000
   560000
   620000
   680000
   800000
   1000000])

(def blinds-structure
  (->> sb-list
       (mapv (fn [sb]
               [sb (* sb 2)]))))
