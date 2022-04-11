(ns depk.transactor.store.fake
  (:require
   [depk.transactor.store.protocol :as p]
   [depk.transactor.log :as log]
   [depk.transactor.event.protocol :as ep]))

(defrecord FakeStoreApi [])

(defn make-fake-store-api
  []
  (log/debug "Use fake store api.")
  (->FakeStoreApi))

(extend-type FakeStoreApi
 p/IStoreApi
 (p/-save-game-history [this game-id game-no records])

 (p/-fetch-game-histories [this game-id])

 ep/IAttachable
 (ep/-input [_this]
   nil)

 (ep/-output [_this]
   nil)

 ep/IComponent
 (ep/-start [_this _opts]))
