(ns depk.transactor.main
  (:require
   [depk.transactor.main-server :refer [main-server]]
   [depk.transactor.main-worker :refer [main-worker]]
   ["worker_threads" :refer [isMainThread]]))

(defn main
  [& args]
  (if isMainThread
    (main-server (first args))
    (main-worker)))
