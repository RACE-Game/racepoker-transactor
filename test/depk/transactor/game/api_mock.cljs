(ns depk.transactor.game.api-mock
  (:require
   [depk.transactor.game.api :as sut]
   [depk.transactor.game.api.protocols :as p]))

(defrecord MockChainApi [])

(extend-type MockChainApi
 p/IChainApi
 (-settle-finished-game [this game-id chips-change-map player-status-map])
 (-settle-failed-game [this game-id player-status-map])
 (-fetch-game-account [this game-id]
   (if (= "NONEXIST GAME ID" game-id)
     (sut/account-not-found! game-id)
     {:players              [],
      :stack-account-pubkey "",
      :mint-pubkey          "",
      :sb                   100,
      :bb                   200,
      :min-buyin            5000,
      :max-buyin            20000
      :is-initialized       true})))

(defrecord MockStoreApi [])

(extend-type MockStoreApi
  p/IStoreApi
  (-save-game-history [this game-id game-no records])
  (-fetch-game-histories [this game-id]))
