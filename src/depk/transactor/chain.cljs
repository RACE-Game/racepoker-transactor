(ns depk.transactor.chain
  (:require
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.chain.solana   :as solana]))

(defn settle
  "Settle a finished game.

  game-id: pubkey, the pubkey of game account.
  settle-map: a map of settles.

  settle is a map of:
  - settle-type: leave, empty, no-update
  - amount: the withdraw amount
  "
  [chain-api game-id settle-serial settle-map]
  (p/-settle chain-api game-id settle-serial settle-map))

(defn set-winner
  "Set a winner for SNG game.

  game-id: pubkey, the pubkey of game account.
  winner-id: pubkey, the winner id."
  [chain-api game-id settle-serial winner-id]
  (p/-set-winner chain-api game-id settle-serial winner-id))

(defn fetch-game-account
  "Fetch account of a game."
  [chain-api game-id opts]
  (p/-fetch-game-account chain-api game-id opts))

(defn fetch-mint-info
  "Fetch mint information of a game."
  [chain-api mint-address]
  (p/-fetch-mint-info chain-api mint-address))

(defn make-solana-api
  []
  (solana/make-solana-api))
