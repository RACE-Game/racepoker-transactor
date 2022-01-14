(ns depk.transactor.game.api.protocols)

(defprotocol IStoreApi
  (-save-card-ciphers [this game-id card-ciphers])
  (-save-key [this game-id player-id key-type key-data])
  (-save-player-actions [this game-id player-actions]))

(defprotocol IChainApi
  (-settle-finished-game [this game-id chips-change-map player-status-map])
  (-settle-failed-game [this game-id player-status-map])
  (-fetch-game-account [this game-id]))
