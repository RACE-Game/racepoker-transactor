(ns depk.transactor.tournament.reconciler
  "The progress for a whole tournament:
  0. Wait tournament to start
  start-time is a unix timestamp, that indicates the starting time for the tournament.
  Before this timestamp, reconciler waits the start.

  1. Mark the tournament is playing
  At start-time, submit the transaction to set the tournament status to `:playing`.

  2. Update ranks, tables and merge tables whenever possible
  During the game, whenever possible, tables will be merged.
  In the tournament, each tables has the size 6, except the final table, which has size 9.

  3. Contract will automatically set the status to completed
  When there's only one player remainning.
  "
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.event.protocol :as event-p]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

(def init-chips (js/BigInt 10000))

(let [n (atom 0)]
  (defn gen-serial-number
    []
    (swap! n inc)))

(defn current-unix-timestamp
  []
  (int (/ (.getTime (js/Date.)) 1000)))

(defn get-game
  [state game-id]
  (->> state
       :games
       (filter #(= game-id (:game-id %)))
       first))

(defn make-pseudo-game-account-state
  "We create a pseudo account state to adapt the tournament game model to on-chain game model."
  [tournament-id start-time size]
  {:players       [nil nil nil nil nil nil nil nil nil],
   :game-id       (str tournament-id "#" (gen-serial-number)),
   :size          size,
   :settle-serial 0,
   :buyin-serial  0,
   :start-time    (* start-time 1000),
   :mint-pubkey   "11111111111111111111111111111111"})

(defn- sit-in-game
  "Let player sit in game, return the new game."
  [{:keys [buyin-serial], :as game} player & [init?]]
  (let [idx (count (take-while some? (:players game)))
        new-buyin-serial (inc buyin-serial)]
    (if (>= idx (:size game))
      (throw (ex-info "No seat available" {:game game, :player player}))
      (-> game
          (assoc-in [:players idx] (if init? player (assoc player :buyin-serial new-buyin-serial)))
          (assoc :buyin-serial new-buyin-serial)))))

(defn- assign-players-to-games
  "Assign players to games"
  [tournament-id num-games ranks start-time size]
  (let [init (vec
              (repeatedly num-games
                          (partial make-pseudo-game-account-state
                                   tournament-id
                                   start-time
                                   size)))]
    (loop [ret      init
           idx      0
           [r & rs] ranks]
      (if r
        (recur
         (update ret
                 idx
                 sit-in-game
                 (-> r
                     (update :pubkey str)
                     (assoc :chips init-chips))
                 true)

         (let [new-idx (inc idx)]
           (if (= new-idx (count ret))
             0
             new-idx))
         rs)
        ret))))

(defn start-tournament
  "Start the tournament.

  Return [new-state, new-event]
  Create the tables for all players."
  [tournament-id state]
  (let [{:keys [ranks num-players status start-time size]} state
        ranks     (filter some? ranks)
        ;; calculate the number of tables
        num-games (quot (+ (dec size) num-players) size)

        ;; assign players to games
        games     (assign-players-to-games tournament-id num-games ranks start-time size)
        new-state (assoc state
                         :games    games
                         :started? true
                         :status   :playing)
        evts      (cond->
                    [{:type :system/tournament-broadcast,
                      :data {:state new-state,
                             :event {:type :system/start-tournament,
                                     :data {:games games}}}}]

                    (= :registering status)
                    (conj {:type :system/submit-start-tournament,
                           :data {}}))]

    (log/log "🌠" tournament-id "Start tournament, creating %s tables" num-games)
    [new-state evts]))

(defmulti apply-event
  (fn [_state event]
    (:type event)))

;; Tournament game settlement

(defn- count-game-players
  [game]
  (count (filter some? (:players game))))

(defn update-player
  [settle-map player & [set-nil?]]
  (if-let [{:keys [settle-type amount]}
           (get settle-map (:pubkey player))]
    (let [player (case settle-type
                   :chips-add
                   (update player :chips + amount)

                   :chips-sub
                   (update player :chips - amount)

                   :no-update
                   player)]
      (when-not (and set-nil? (= (:chips player) (js/BigInt 0)))
        player))
    player))

(defn update-games
  [{:keys [state], :as ctx} game-id settle-map]
  (let [games     (->> (:games state)
                       (mapv (fn [g]
                               (if (not= (:game-id g) game-id)
                                 g
                                 (let [players (->> (:players g)
                                                    (mapv #(update-player settle-map % true)))]
                                   (-> g
                                       (assoc :players players)
                                       (update :settle-serial inc)))))))

        new-state (assoc state :games games)]
    (-> ctx
        (assoc :state new-state)
        (assoc :updated-game-id game-id))))

(defn make-notify-events
  "Make notify events.

  1. Always refresh the state snapshot.
  2. For tables cancelled, notify the resit.
  3. For the updated table, if not being cancelled, notify the next game."
  [{:keys [state updated-game-id resit-map resit-game-id], :as ctx}]
  (let [updated-game (get-game state updated-game-id)
        sync-state-game-ids (remove #{updated-game-id} (vals resit-map))
        finish?      (= 1 (count (filter #(> (:chips %) (js/BigInt 0)) (:ranks state))))
        events       (cond-> [
                              ;; Refresh the state snapshot
                              {:type :system/tournament-broadcast,
                               :data {:state state}}]

                       ;; Notify game to continue
                       updated-game
                       (conj {:type :system/tournament-broadcast,
                              :data {:event {:type :system/next-game,
                                             :data {:game-id            updated-game-id,
                                                    :game-account-state updated-game,
                                                    :finish?            finish?}}}})

                       ;; Notify game to resit
                       (seq resit-map)
                       (conj {:type :system/tournament-broadcast,
                              :data {:event {:type :system/resit-table,
                                             :data {:game-id   resit-game-id,
                                                    :resit-map resit-map,
                                                    :finish?   true}}}})

                       ;; Tell other games new players are coming
                       sync-state-game-ids
                       (into (map (fn [gid]
                                    {:type :system/tournament-broadcast,
                                     :data {:event {:type :system/sync-state,
                                                    :data {:game-account-state (get-game state gid),
                                                           :game-id            gid}}}})
                                  sync-state-game-ids)))]
    (update ctx :events into events)))

(defn update-ranks
  [{:keys [state], :as ctx} settle-map]
  (let [{:keys [ranks num-players]} state
        alive         (take-while #(> (:chips %) (js/BigInt 0)) ranks)
        eliminated    (take-last (- num-players (count alive)) ranks)
        updated-alive (->> alive
                           (mapv (partial update-player settle-map))
                           (sort-by :chips >))
        new-ranks     (vec (concat updated-alive eliminated))]
    (update ctx :state assoc :ranks new-ranks)))

(defn resit-players
  [{:keys [state updated-game-id], :as ctx}]
  (let [{:keys [games size tournament-id]} state
        sorted-games (sort-by count-game-players games)
        resit-game (first sorted-games)

        [resit-map new-games]
        (when (or (= 1 (count-game-players resit-game))
                  (= (:game-id resit-game) updated-game-id))
          (loop [ps        (filter some? (:players (first sorted-games)))
                 games     (vec (next sorted-games))
                 idx       0
                 resit-map {}]
            (if-not (seq ps)
              [resit-map games]
              (if (= idx (count games))
                [nil nil]
                (let [game      (nth games idx)
                      cnt       (- size (count-game-players game))
                      [ps-to-sit ps-rest] (split-at cnt ps)
                      new-game  (reduce sit-in-game game ps-to-sit)
                      resit-map (reduce #(assoc %1 (:pubkey %2) (:game-id new-game))
                                        resit-map
                                        ps-to-sit)]
                  (recur ps-rest
                         (assoc games idx new-game)
                         (inc idx)
                         resit-map))))))]
    (log/log "🌠" tournament-id "Re-sit players: %s" (prn-str resit-map))
    (if (seq resit-map)
      (-> ctx
          (update :state assoc :games new-games)
          (assoc :resit-map resit-map :resit-game-id (:game-id (first sorted-games))))
      ctx)))

(defn maybe-resit
  [{:keys [state], :as ctx}]
  (let [{:keys [games]} state]
    (cond
      ;; Final table
      (= 1 (count games))
      ctx

      ;; Try merge into normal tables
      :else
      (resit-players ctx))))

(defn maybe-settle
  [{:keys [state], :as ctx}]
  (let [{:keys [ranks tournament-id]} state
        alive-players (filter #(> (:chips %) (js/BigInt 0)) ranks)]
    (log/log "🐻" tournament-id "Alive players: %s" (count alive-players))
    (if (= 1 (count alive-players))
      (-> ctx
          (update :state assoc :status :completed)
          (update :events
                  conj
                  {:type :system/settle-tournament,
                   :data {:ranks (mapv :pubkey (:ranks state))}}))
      ctx)))


(defmethod apply-event :system/start-tournament
  [{:keys [tournament-id], :as state} _]
  (start-tournament tournament-id state))

(defmethod apply-event :system/start-tournament-games
  [{:keys [games], :as state} _]
  [state
   [{:type :system/tournament-broadcast,
     :data {:event {:type :system/start-tournament-games,
                    :data {:games games}}}}]])

(defmethod apply-event :system/sync-tournament-state
  [old-state
   {{:keys [state]} :data}]
  (if (= :registering (:status old-state))
    [state
     [{:type :system/tournament-broadcast,
       :data {:state state}}]]
    [old-state nil]))

(defmethod apply-event :system/tournament-game-settle
  [state
   {:keys [game-id],
    {:keys [settle-map]} :data}]
  (let [ctx (-> {:state state, :events []}
                (update-games game-id settle-map)
                (update-ranks settle-map)
                (maybe-settle)
                (maybe-resit)
                (make-notify-events))
        {:keys [state events]} ctx]
    [state events]))

(defn start-reconciler
  [{:keys [input output]} {:keys [tournament-id init-state]}]
  (let [current-time (current-unix-timestamp)
        {:keys [start-time status]} init-state]
    (if (#{:playing :registering} status)
      (a/go
       (if (> current-time start-time)
         ;; Start directly
         (a/>! input {:type :system/start-tournament})

         ;; Trigger start event after timeout
         (let [secs (- start-time current-time)]
           (log/log "🌠" tournament-id "Schedule tournament starting after %s seconds" secs)
           (a/<! (a/timeout (* 1000 secs)))
           (a/>! input {:type :system/start-tournament})))
       (a/<! (a/timeout 60000))
       (a/>! input {:type :system/start-tournament-games}))
      (do
        (log/log "🌠" tournament-id "Tournament already completed")
        (a/close! input))))

  (a/go-loop [state init-state]
    (if-let [evt (a/<! input)]
      (let [[new-state evts] (apply-event state evt)]
        (log/log "🌠" tournament-id "Tournament reconciler received event: %s" (:type evt))
        (log/log "🌠" tournament-id
                 "Tournament reconciler emits event: %s" (mapv (comp :type :event :data) evts))
        (a/<! (a/onto-chan! output evts false))
        (when-not (= :completed (:status new-state))
          (recur new-state)))
      (do
        (log/log "💤️" tournament-id "Reconciler quit")
        (a/close! output)))))

(defrecord TournamentReconciler [input output game-map ranks])

(extend-type TournamentReconciler
 event-p/IAttachable
 (-input [this]
   (:input this))
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   #{:system/tournament-game-settle
     :system/sync-tournament-state})
 event-p/IComponent
 (-start [this {:keys [tournament-id], :as opts}]
   (log/log "🎉" tournament-id "Start tournament reconciler")
   (start-reconciler this opts)))

(defn make-tournament-reconciler
  []
  (->TournamentReconciler (a/chan)
                          (a/chan)
                          (atom {})
                          (atom [])))

;; Some dirty tests

(comment
  (let [init-state {:tournament-id "t",
                    :num-players   4,
                    :status        :playing,
                    :ranks         [{:pubkey       "p1",
                                     :chips        (js/BigInt 10000),
                                     :rebuy        0,
                                     :buyin-serial 0}
                                    {:pubkey       "p2",
                                     :chips        (js/BigInt 10000),
                                     :rebuy        0,
                                     :buyin-serial 1}
                                    {:pubkey       "p3",
                                     :chips        (js/BigInt 10000),
                                     :rebuy        0,
                                     :buyin-serial 2}
                                    {:pubkey       "p4",
                                     :chips        (js/BigInt 10000),
                                     :rebuy        0,
                                     :buyin-serial 3}]}]
    (prn (start-tournament "t" init-state)))

  ;; chips-only update
  (let [state {:tournament-id "t",
               :status        :playing,
               :ranks         [{:pubkey "p1", :chips (js/BigInt 10000)}
                               {:pubkey "p2", :chips (js/BigInt 10000)}
                               {:pubkey "p3", :chips (js/BigInt 10000)}
                               {:pubkey "p4", :chips (js/BigInt 10000)}],
               :games         [{:game-id "t#100",
                                :size    3,
                                :players [{:pubkey "p1", :chips (js/BigInt 10000)}
                                          {:pubkey "p2", :chips (js/BigInt 10000)}
                                          nil]}
                               {:game-id "t#200",
                                :size    3,
                                :players [{:pubkey "p3", :chips (js/BigInt 10000)}
                                          {:pubkey "p4", :chips (js/BigInt 10000)}
                                          nil]}]}

        [state events] (apply-event state
                                    {:type    :system/tournament-game-settle,
                                     :game-id "t#100",
                                     :data    {:settle-map {"p1" {:settle-type :chips-add,
                                                                  :amount      (js/BigInt 500)},
                                                            "p2" {:settle-type :chips-sub,
                                                                  :amount      (js/BigInt 500)}}}})]
    (println "state---------")
    (prn state)
    (println "events--------")
    (prn events))


  ;; eliminated -> final table
  (let [state {:tournament-id "t",
               :status        :playing,
               :ranks         [{:pubkey "p1", :chips (js/BigInt 10000)}
                               {:pubkey "p2", :chips (js/BigInt 10000)}
                               {:pubkey "p3", :chips (js/BigInt 10000)}
                               {:pubkey "p4", :chips (js/BigInt 10000)}],
               :games         [{:game-id "t#100",
                                :size    3,
                                :players [{:pubkey "p1", :chips (js/BigInt 10000)}
                                          {:pubkey "p2", :chips (js/BigInt 10000)}]}
                               {:game-id "t#200",
                                :size    3,
                                :players [{:pubkey "p3", :chips (js/BigInt 10000)}
                                          {:pubkey "p4", :chips (js/BigInt 10000)}]}]}

        [state events]
        (apply-event state
                     {:type    :system/tournament-game-settle,
                      :game-id "t#100",
                      :data    {:settle-map {"p1" {:settle-type :chips-add,
                                                   :amount      (js/BigInt 10000)},
                                             "p2" {:settle-type :chips-sub,
                                                   :amount      (js/BigInt 10000)}}}})]
    (println "state---------")
    (prn state)
    (println "events--------")
    (prn events))

  ;; eliminated -> halt
  (let [state {:tournament-id "t",
               :status        :playing,
               :ranks         [{:pubkey "p1", :chips (js/BigInt 10000)}
                               {:pubkey "p2", :chips (js/BigInt 10000)}
                               {:pubkey "p3", :chips (js/BigInt 10000)}
                               {:pubkey "p4", :chips (js/BigInt 10000)}
                               {:pubkey "p5", :chips (js/BigInt 10000)}
                               {:pubkey "p6", :chips (js/BigInt 10000)}],
               :games         [{:game-id "t#100",
                                :size    3,
                                :players [{:pubkey "p1", :chips (js/BigInt 10000)}
                                          {:pubkey "p2", :chips (js/BigInt 10000)}]}
                               {:game-id "t#200",
                                :size    3,
                                :players [{:pubkey "p3", :chips (js/BigInt 10000)}
                                          {:pubkey "p4", :chips (js/BigInt 10000)}]}
                               {:game-id "t#300",
                                :size    3,
                                :players [{:pubkey "p5", :chips (js/BigInt 10000)}
                                          {:pubkey "p6", :chips (js/BigInt 10000)}]}]}

        [state events]
        (apply-event state
                     {:type    :system/tournament-game-settle,
                      :game-id "t#100",
                      :data    {:settle-map {"p1" {:settle-type :chips-add,
                                                   :amount      (js/BigInt 10000)},
                                             "p2" {:settle-type :chips-sub,
                                                   :amount      (js/BigInt 10000)}}}})]
    (println "state---------")
    (prn state)
    (println "events--------")
    (prn events))

  ;; eliminated -> merge
  (let [state {:tournament-id "t",
               :status        :playing,
               :ranks         [{:pubkey "p1", :chips (js/BigInt 10000)}
                               {:pubkey "p2", :chips (js/BigInt 10000)}
                               {:pubkey "p3", :chips (js/BigInt 10000)}
                               {:pubkey "p4", :chips (js/BigInt 10000)}
                               {:pubkey "p5", :chips (js/BigInt 0)}
                               {:pubkey "p6", :chips (js/BigInt 10000)}],
               :games         [{:game-id "t#100",
                                :size    3,
                                :players [{:pubkey "p1", :chips (js/BigInt 10000)}
                                          {:pubkey "p2", :chips (js/BigInt 10000)}]}
                               {:game-id "t#200",
                                :size    3,
                                :players [{:pubkey "p3", :chips (js/BigInt 10000)}
                                          {:pubkey "p4", :chips (js/BigInt 10000)}]}
                               {:game-id "t#300",
                                :size    3,
                                :players [{:pubkey "p6", :chips (js/BigInt 10000)}]}]}

        [state events]
        (apply-event state
                     {:type    :system/tournament-game-settle,
                      :game-id "t#100",
                      :data    {:settle-map {"p1" {:settle-type :chips-add,
                                                   :amount      (js/BigInt 10000)},
                                             "p2" {:settle-type :chips-sub,
                                                   :amount      (js/BigInt 10000)}}}})]
    (println "state---------")
    (prn state)
    (println "events--------")
    (prn events))

  (let [tournament-id "t"
        num-games     2
        ranks         [{:pubkey "p1"}
                       {:pubkey "p2"}
                       {:pubkey "p3"}
                       {:pubkey "p4"}]]
    (println
     (assign-players-to-games tournament-id num-games ranks))))
