(ns depk.transactor.chain.protocol
  "Protocol for chain API.")

(defprotocol IChainApi
  (-settle-finished-game [this game-id chips-change-map player-status-map expected-player-map])
  (-settle-failed-game [this game-id player-status-map expected-player-map])
  (-set-winner [this game-id winner-id])
  (-fetch-game-account [this game-id])
  (-fetch-mint-info [this mint-address]))
