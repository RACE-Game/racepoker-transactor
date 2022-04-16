(ns depk.transactor.state.global-chain-api
  (:require
   [depk.transactor.chain :as chain]
   [mount.core :as mount]))

(mount/defstate global-chain-api
  :start
  (chain/make-solana-api))


(comment
  (chain/set-winner
   @global-chain-api
   "6R1A1mddhrJw5CKQfXCsq2gkPvvotr3xoCHCV2tpnF4r"
   "DfgtACV9VzRUKUqRjuzxRHmeycyomcCzfRHgVyDPha9F"))
