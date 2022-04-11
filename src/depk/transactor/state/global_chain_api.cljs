(ns depk.transactor.state.global-chain-api
  (:require
   [depk.transactor.chain :as chain]
   [mount.core :as mount]))

(mount/defstate global-chain-api
  :start
  (chain/make-solana-api))
