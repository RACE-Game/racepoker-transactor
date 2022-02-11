(ns depk.transactor.game.api
  "APIs for make transaction on Arweave and Solana."
  (:require [depk.transactor.game.api.protocols :as p]
            [depk.transactor.game.api.solana :as solana]
            [depk.transactor.game.api.mem-store :as mem-store]
            [depk.transactor.util :as u]
            [cljs.core.async :refer [go-loop <!]]))

(defn account-not-found!
  [account-id]
  (throw (ex-info "Account not found" {:account-id account-id})))

;; Arweave - Storage, used for public data

(defn save-game-history
  "Save game history.

  A history is a game-id, game-no and a list of records.

  game-id: pubkey, the pubkey of game account.
  game-no: a serial number.
  records: a list of [event, state]."
  [store-api game-id game-no records]
  (p/-save-game-history store-api game-id game-no records))

(defn fetch-game-histories
  "Get game histories by game-id."
  [store-api game-id]
  (p/-fetch-game-histories store-api game-id))

;; Solana - Blockchain

(defn settle-finished-game
  "Settle a finished game.

  game-id: pubkey, the pubkey of game account.
  player-state-map: map, player-id to its state, which contains chips and status."
  [chain-api game-id chips-change-map player-status-map]
  (p/-settle-finished-game chain-api game-id chips-change-map player-status-map))

(defn settle-failed-game
  "Settle a failed game.

  game-id: pubkey, the pubkey of game account.
  player-state-map: map, player-id to its state, which contains chips and status."
  [chain-api game-id player-status-map]
  (p/-settle-failed-game chain-api game-id player-status-map))

(defn fetch-game-account
  "Fetch account of a game."
  [chain-api game-id]
  (p/-fetch-game-account chain-api game-id))

(defn make-solana-api []
  (solana/->SolanaApi))

(defn make-mem-store-api []
  (mem-store/make-mem-store-api))
