(ns depk.transactor.broadcast.protocol)

(defprotocol IBroadcaster
  (-get-snapshot [this])
  (-get-game-account-snapshot [this]))
