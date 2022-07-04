(ns depk.transactor.main
  (:require
   [depk.transactor.main-server :refer [main-server]]
   [depk.transactor.main-game :refer [start-game-worker]]
   [depk.transactor.main-tournament :refer [start-tournament-worker]]
   [depk.transactor.main-tournament-game :refer [start-tournament-game-worker]]
   ["worker_threads" :refer [isMainThread workerData]]))

(defn main
  [& args]
  (cond
    isMainThread
    (main-server (first args))

    (= (aget workerData "worker") "game")
    (start-game-worker)

    (= (aget workerData "worker") "tournament")
    (start-tournament-worker)

    (= (aget workerData "worker") "tournament-game")
    (start-tournament-game-worker)

    :else
    (println "Invalid worker init data, ignore.")))
