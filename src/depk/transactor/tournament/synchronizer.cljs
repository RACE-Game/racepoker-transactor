(ns depk.transactor.tournament.synchronizer
  (:require
   [cljs.core.async :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as event-p]
   [depk.transactor.log :as log]
   [depk.transactor.tournament.models :as m]))

(defn start
  [{:keys [chain-api output]} {:keys [tournament-id init-state]}]
  (a/go-loop [buyin-serial (:buyin-serial init-state)]
    (let [state (a/<! (p/-fetch-tournament-account chain-api
                                                   tournament-id
                                                   {:commitment "finalized"}))]
      (when (and state
                 (< buyin-serial (:buyin-serial state))
                 (= :registering (:status state)))
        (log/infof "👀️Read tournament[%s] state, %s -> %s"
                   tournament-id
                   buyin-serial
                   (:buyin-serial state))
        (a/>! output
              {:type :system/sync-tournament-state,
               :data {:state (m/make-tournament-state tournament-id state)}})

        (when-not (= :completed (:status state))
          (recur (max buyin-serial (:buyin-serial state))))))))

(defrecord TournamentSynchronizer [chain-api output])

(extend-type TournamentSynchronizer
 event-p/IAttachable
 (-input [_this]
   nil)
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   nil)
 event-p/IComponent
 (-start [this opts]
   (start this opts)))

(defn make-tournament-synchronizer
  [chain-api]
  (->TournamentSynchronizer chain-api
                            (a/chan)))
