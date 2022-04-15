(ns depk.transactor.chain
  (:require [depk.transactor.chain.protocol :as p]
            [depk.transactor.event.protocol :as ep]
            [depk.transactor.chain.solana :as solana]))

(defn settle-finished-game
  "Settle a finished game.

  game-id: pubkey, the pubkey of game account.
  player-state-map: map, player-id to its state, which contains chips and status."
  [chain-api game-id chips-change-map player-status-map expected-player-map]
  (p/-settle-finished-game chain-api game-id chips-change-map player-status-map expected-player-map))

(defn settle-failed-game
  "Settle a failed game.

  game-id: pubkey, the pubkey of game account.
  player-state-map: map, player-id to its state, which contains chips and status."
  [chain-api game-id player-status-map expected-player-map]
  (p/-settle-failed-game chain-api game-id player-status-map expected-player-map))

(defn fetch-game-account
  "Fetch account of a game."
  [chain-api game-id]
  (p/-fetch-game-account chain-api game-id))

(defn fetch-mint-info
  "Fetch mint information of a game."
  [chain-api mint-address]
  (p/-fetch-mint-info chain-api mint-address))

(defn make-solana-api []
  (solana/make-solana-api))
