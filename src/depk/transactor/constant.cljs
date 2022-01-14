(ns depk.transactor.constant)

(def instruction-header-settle [3 1])

;; 1 chips amount = 0.01 assets amount
(def level-info-map
  {:NL10  {:sb 5,
           :bb 10},
   :NL20  {:sb 10,
           :bb 20},
   :NL50  {:sb 20,
           :bb 50},
   :NL100 {:sb 50,
           :bb 100}})
