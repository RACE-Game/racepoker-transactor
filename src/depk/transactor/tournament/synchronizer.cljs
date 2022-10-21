(ns depk.transactor.tournament.synchronizer
  (:require
   [cljs.core.async :as a]
   [depk.transactor.util :as u]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as event-p]
   [depk.transactor.log :as log]
   [depk.transactor.tournament.models :as m]))

(defn start
  [{:keys [chain-api output]} {:keys [tournament-id init-state]}]
  (a/go-loop [buyin-serial  -1
              settle-serial -1]
    (let [state (a/<! (p/-fetch-tournament-account chain-api
                                                   tournament-id
                                                   {:commitment "finalized"}))]
      (cond
        ;; Retry
        (not state)
        (recur buyin-serial settle-serial)

        ;;
        (#{:playing :registering} (:status state))
        (do
          (when (or (< buyin-serial (:buyin-serial state))
                    (< settle-serial (:settle-serial state)))
            (log/log "👀️"
                     tournament-id
                     "Synchronizer got new tournament state, %s -> %s | %s -> %s"
                     buyin-serial
                     (:buyin-serial state)
                     settle-serial
                     (:settle-serial state))
            (a/>! output
                  {:type :system/sync-tournament-state,
                   :data {:state (m/make-tournament-state tournament-id state)}}))
          (a/<! (a/timeout 5000))
          (recur (max buyin-serial (:buyin-serial state))
                 (max settle-serial (:settle-serial state))))

        ;; Tournament is finished
        :else
        (do (a/<! (a/timeout 5000))
            (log/log "💤️" tournament-id "Synchronizer quit"))))))

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
