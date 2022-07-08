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
          (log/infof "👀️Read tournament[%s] state, %s -> %s"
                     tournament-id
                     buyin-serial
                     (:buyin-serial state))
          (a/>! output
                {:type :system/sync-tournament-state,
                 :data {:state (m/make-tournament-state tournament-id state)}}))

        (if (#{:playing :completed} (:status state))
          (log/infof "💤️Synchronizer quit for tournament[%s]" tournament-id)
          (do
            (a/<! (a/timeout 5000))
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
   (log/infof "🏁Start synchronizer for tournament[%s]" (:tournament-id opts))
   (start this opts)))

(defn make-tournament-synchronizer
  [chain-api]
  (->TournamentSynchronizer chain-api
                            (a/chan)))
