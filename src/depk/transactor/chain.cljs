(ns depk.transactor.chain
  (:require
   [depk.transactor.log :as log]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.chain.solana :as solana]
   [depk.transactor.game.synchronizer :as synchronizer]
   [depk.transactor.game.submitter :as submitter]))

(defn fetch-game-account
  "Fetch account of a game."
  [chain-api game-id opts]
  (log/log "🔍" nil "Fetch game account: %s" game-id)
  (p/-fetch-game-account chain-api game-id opts))

(defn fetch-mint-info
  "Fetch mint information of a game."
  [chain-api mint-address]
  (log/log "🔍" nil "Fetch mint info: %s" mint-address)
  (p/-fetch-mint-info chain-api mint-address))

(defn fetch-tournament-account
  "Fetch account of a tournament."
  [chain-api tournament-id opts]
  (log/log "🔍" nil "Fetch tournament account: %s" tournament-id)
  (p/-fetch-tournament-account chain-api tournament-id opts))

(defn fetch-tournament-list
  "Fetch list of tournaments."
  [chain-api reg-center-address]
  (p/-fetch-tournament-list chain-api reg-center-address))

(defn make-solana-api
  []
  (solana/make-solana-api))

(defn make-synchronizer
  [chain-api]
  (synchronizer/make-synchronizer chain-api))

(defn make-submitter
  [chain-api]
  (submitter/make-submitter chain-api))
