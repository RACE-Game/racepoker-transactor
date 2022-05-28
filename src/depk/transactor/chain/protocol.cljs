(ns depk.transactor.chain.protocol
  "Protocol for chain API.")

(defprotocol IChainApi
  (-settle [this game-id game-account-state settle-serial settle-map]
    "Settle player left events.")
  (-set-winner [this game-id game-account-state settle-serial ranking]
    "Set the winner for SNG game.")
  (-fetch-game-account [this game-id opts]
    "Get the state of game account.")
  (-fetch-mint-info [this mint-address]
    "Get the state of mint account."))
