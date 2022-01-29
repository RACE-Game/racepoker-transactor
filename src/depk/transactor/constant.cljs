(ns depk.transactor.constant)

(def instruction-header-settle [3 1])

(def level-info-map
  {:NL100  {:sb 1, :bb 2},
   :NL200  {:sb 1, :bb 2},
   :NL500  {:sb 2, :bb 5},
   :NL1000 {:sb 5, :bb 10}})
