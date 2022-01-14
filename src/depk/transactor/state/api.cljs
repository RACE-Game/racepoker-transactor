(ns depk.transactor.state.api
  (:require [mount.core :as mount]
            [depk.transactor.game.api :as api]))

(mount/defstate chain-api
  :start
  (api/make-solana-api))
