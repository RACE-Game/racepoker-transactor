(ns depk.transactor.store
  (:require [depk.transactor.store.protocol :as p]
            [depk.transactor.event.protocol :as ep]
            [depk.transactor.store.fake :as fake]))

(defn make-fake-store-api []
  (fake/make-fake-store-api))
