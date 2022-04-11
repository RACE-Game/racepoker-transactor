(ns depk.transactor.store.protocol
  "Protocol for store API.")

(defprotocol IStoreApi
  (-save-game-history [this game-id game-no records])
  (-fetch-game-histories [this game-id]))
