(ns depk.transactor.chain.internal
  "Internal chain API.

  Used for tournaments.
  This API transport will receive the events from games, and deliver them to the reconciler."
  (:require
   [cljs.core.async :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]))


(defn start-internal-tournament-api
  [chain-api game-id input output]
)

(defrecord InternalTournamentApi [input output])

(extend-type InternalTournamentApi

 p/IChainApi
 ;; Only implement part of the protocol
 (-settle [this game-id game-account-state settle-serial settle-map])

 (-fetch-game-account
   [this game-id opts])

 (-fetch-mint-info
   [this mint-address])

 ep/IAttachable
 (ep/-input [this]
   (:input this))

 (ep/-output [this]
   (:output this))

 (ep/-interest-event-types
   [_this]
   [:system/settle])

 ep/IComponent
 (ep/-start [this opts]
   (let [{:keys [input output]} this
         {:keys [game-id]}      opts])
   (start-internal-tournament-api this game-id input output)
   nil))
