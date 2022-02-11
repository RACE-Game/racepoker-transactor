(ns depk.transactor.game.api.mem-store
  "In memory storage, for testing."
  (:require
   [depk.transactor.util :refer [go-try <!?]]
   [depk.transactor.game.api.protocols :as p]
   [cljs.core.async :as a]))

(defrecord MemStoreApi [store])

(extend-type MemStoreApi
  p/IStoreApi
  (-save-game-history [this game-id game-no records]
    (let [store (:store this)]
      (swap! store assoc-in [game-id game-no]
             {:game-id game-id
              :game-no game-no
              :records records})))

  (-fetch-game-histories [this game-id]
    (let [store (:store this)]
      (-> @store
          (get game-id)
          vals
          (->> (sort-by :game-no >))))))

(defn make-mem-store-api []
  (let [store (atom {})]
    (->MemStoreApi store)))
