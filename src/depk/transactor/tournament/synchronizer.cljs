(ns depk.transactor.tournament.synchronizer
  (:require
   [cljs.core.async :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as event-p]
   [depk.transactor.log :as log]
   [depk.transactor.tournament.models :as m]))

(defn start
  [{:keys [chain-api output]} {:keys [tournament-id init-state]}]
  (a/go-loop [buyin-serial -1]
    (let [state (a/<! (p/-fetch-tournament-account chain-api
                                                   tournament-id
                                                   {:commitment "finalized"}))]
      (when (and state (= :registering (:status state)))
        (when (< buyin-serial (:buyin-serial state))
          (log/log "👀️"
                   tournament-id
                   "Synchronizer got new tournament state, %s -> %s"
                   buyin-serial
                   (:buyin-serial state))
          (a/>! output
                {:type :system/sync-tournament-state,
                 :data {:state (m/make-tournament-state tournament-id state)}}))

        (if (#{:playing :completed} (:status state))
          (log/log "💤️" tournament-id "Synchronizer quit")
          (do
            (a/<! (a/timeout 10000))
            (recur (max buyin-serial (:buyin-serial state)))))))))

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
   (log/log "🎉" (:tournament-id opts) "Start synchronizer")
   (start this opts)))

(defn make-tournament-synchronizer
  [chain-api]
  (->TournamentSynchronizer chain-api
                            (a/chan)))
