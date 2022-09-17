(ns depk.transactor.tournament.models)

(def init-ante (js/BigInt 0))
(def init-sb (js/BigInt 50))
(def init-bb (js/BigInt 100))

(defrecord Tournament
  [tournament-id

   ;; A list of play & his chips.
   ranks
   num-players

   ;; A list of games
   ;; Each item is a map that contains:
   ;;     - game-id
   ;;     - players
   games
   ;; table size
   size
   ;; Tournament start-time
   start-time

   ;; The start time for the tournament
   started?])

(defn parse-raw-tournament-account-state
  "Parse tournament account state, remove usless fields."
  [tournament-account-state]
  (letfn [(parse-players [players]
                         (vec (for [p players]
                                (when p
                                  {:pubkey (str (:pubkey p)),
                                   :chips  (:start-chips tournament-account-state)}))))]
    (-> tournament-account-state
        (update :ranks parse-players)
        (select-keys [:name :ranks :buyin-serial :settle-serial :start-time :status :start-chips
                      :blinds-mode]))))

(defn make-tournament-state
  [tournament-id tournament-account-state]
  (let [parsed-account-state (parse-raw-tournament-account-state tournament-account-state)]
    (->> (map->Tournament
          {:tournament-id            tournament-id,
           :start-chips              (:start-chips tournament-account-state),
           :ranks                    (:ranks parsed-account-state),
           :status                   (:status tournament-account-state),
           :start-time               (:start-time tournament-account-state),
           :blinds-mode              (:blinds-mode tournament-account-state),
           :size                     (:size tournament-account-state),
           :num-players              (:num-players tournament-account-state),
           :tournament-account-state parsed-account-state})
         (into {}))))
