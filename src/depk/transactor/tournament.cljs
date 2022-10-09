(ns depk.transactor.tournament
  "Tournament APIs"
  (:require
   [depk.transactor.util :refer [go-try]]
   [depk.transactor.log :as log]
   [depk.transactor.tournament.worker :as worker]
   [depk.transactor.worker-manager :as manager]))

(defn error-tournament-not-exist!
  [tournament-id]
  (throw (ex-info "tournament not exist" {:tournament-id tournament-id})))

(defn launch-tournament
  "Load a tournament into transactor.

  Once the tournament is loaded, people can join.
  The tournament will start at the specific time."
  [tournament-manager tournament-id]
  {:pre [(string? tournament-id)]}
  (go-try
   ;; (log/log "🏆" tournament-id "Start tournament")
   (manager/try-start tournament-manager
                      tournament-id
                      worker/make-worker)))

(defn state
  "Return the state of a tournament.
  It includes:
      - ranks, a list of players with their chips
      - games, a map from id to games states
        this interface is used for an overall information
        so table state will be shrinked"
  [tournament-manager tournament-id]
  {:pre [(string? tournament-id)]}
  (when-let [tournament-worker (manager/find-worker-unchecked tournament-manager tournament-id)]
    (:serialized-state (worker/get-snapshot tournament-worker))))
