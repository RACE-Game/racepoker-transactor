(ns depk.transactor.game.api.protocols)

(defprotocol IStoreApi
  (-save-game-history [this game-id game-no records])
  (-fetch-game-histories [this game-id]))

(defprotocol IChainApi
  (-settle-finished-game [this game-id chips-change-map player-status-map])
  (-settle-failed-game [this game-id player-status-map])
  (-fetch-game-account [this game-id])
  (-fetch-mint-info [this mint-address]))
