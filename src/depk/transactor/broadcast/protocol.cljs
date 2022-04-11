(ns depk.transactor.broadcast.protocol)

(defprotocol IBroadcaster
  (-get-snapshot [this]))
