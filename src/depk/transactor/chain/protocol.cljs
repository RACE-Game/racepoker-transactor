(ns depk.transactor.chain.protocol
  "Protocol for chain API.")

(defprotocol IChainApi
  (-settle
   [this game-id game-account-state settle-serial settle-map]
   "Settle player left events.")
  (-set-winner
   [this game-id game-account-state settle-serial ranking]
    "Set the winner for SNG game.")
  (-start-tournament
    [this tournament-id tournament-account-state settle-serial]
    "Start the tournament, set status to playing.")
  (-settle-tournament
   [this tournament-id tournament-account-state settle-serial ranks]
   "Settle tournament state.")
  (-fetch-game-account
   [this game-id opts]
   "Get the state of game account.")
  (-fetch-mint-info
   [this mint-address]
   "Get the state of mint account.")
  (-fetch-tournament-account
   [this tournament-id opts]
   "Get the state of tournament account."))
