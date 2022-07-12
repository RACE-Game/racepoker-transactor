(ns depk.transactor.tournament.handle
  "Tournament handle is used to control a set of components of a tournament."
  (:require
   [cljs.core.async :as a]
   [depk.transactor.tournament.models :as m]
   [depk.transactor.log :as log]
   [depk.transactor.event :as event]
   [depk.transactor.chain :as chain]
   [depk.transactor.tournament.broadcaster :as broadcaster]
   [depk.transactor.tournament.synchronizer :as synchronizer]
   [depk.transactor.tournament.submitter :as submitter]
   [depk.transactor.tournament.reconciler :as reconciler]))

(defrecord TournamentHandle
  [event-bus
   chain-api
   broadcaster
   reconciler])

(defn make-tournament-handle
  [tournament-id post-msg-fn]
  (log/log "🎉" tournament-id "Create tournament handle")
  (a/go
   (let [chain-api        (chain/make-solana-api)
         tournament-state (a/<! (chain/fetch-tournament-account chain-api
                                                                tournament-id
                                                                {:commitment "finalized"}))
         init-state       (m/make-tournament-state tournament-id tournament-state)
         broadcaster      (broadcaster/make-tournament-broadcaster post-msg-fn)
         synchronizer     (synchronizer/make-tournament-synchronizer chain-api)
         submitter        (submitter/make-tournament-submitter chain-api)
         reconciler       (reconciler/make-tournament-reconciler)
         event-bus        (event/make-mem-event-bus)
         opts             {:tournament-id tournament-id,
                           :init-state    init-state}]
     ;; Attach components to event bus
     (event/attach reconciler event-bus)
     (event/attach submitter event-bus)
     (event/attach broadcaster event-bus)
     (event/attach synchronizer event-bus)
     ;; Start components
     (event/start-component event-bus opts)
     (event/start-component broadcaster opts)
     (event/start-component reconciler opts)
     (event/start-component synchronizer opts)
     (event/start-component submitter opts)

     (log/log "🎉" tournament-id "Tournament handle started")
     (->TournamentHandle event-bus chain-api broadcaster reconciler))))

(defn worker-handle?
  [x]
  (instance? TournamentHandle x))

(defn send-event
  [game-handle event]
  (event/send (:event-bus game-handle) event))
