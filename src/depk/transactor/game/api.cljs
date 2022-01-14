(ns depk.transactor.game.api
  "APIs for make transaction on Arweave and Solana."
  (:require [depk.transactor.game.api.protocols :as p]
            [depk.transactor.game.api.solana :as solana]
            [depk.transactor.util :as u]
            [cljs.core.async :refer [go-loop <!]]))

(defn account-not-found!
  [account-id]
  (throw (ex-info "Account not found" {:account-id account-id})))

;; Arweave - Storage, used for public data

(defn save-card-ciphers
  "Save encrypted cards for a game.

  game-id: pubkey, the pubkey of game account.
  card-ciphers: string, encrypted cards data."
  [store-api game-id card-ciphers]
  (p/-save-card-ciphers store-api game-id card-ciphers))

(defn save-key
  "Save key for decryption.

  game-id: pubkey, the pubkey of game account.
  player-id: pubkey, the pubkey of player's account.
  key-type: keyword, where the key should be used for.
  key-data: string, encrypted key for RSA decryption."
  [store-api game-id player-id key-type key-data]
  (p/-save-key store-api game-id player-id key-type key-data))

(defn save-player-actions
  "Save all player actions of a game.

  game-id: pubkey, the pubkey of game account.
  player-actions: [[player-id, player-action]], a list of player-id and its action."
  [store-api game-id player-actions]
  (p/-save-player-actions store-api game-id player-actions))

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
