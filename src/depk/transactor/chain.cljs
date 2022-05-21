(ns depk.transactor.chain
  (:require
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.chain.solana   :as solana]))

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
