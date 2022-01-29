(ns depk.transactor.constant)

(def instruction-header-settle [3 1])

(def level-info-map
  {:NL10  {:sb 0.05,
           :bb 0.1},
   :NL20  {:sb 0.1,
           :bb 0.2},
   :NL50  {:sb 0.2,
           :bb 0.5},
   :NL100 {:sb 0.5,
           :bb 1}})
