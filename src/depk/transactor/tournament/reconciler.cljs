(ns depk.transactor.tournament.reconciler
  "The progress for a whole tournament:
  0. Wait tournament to start
  start-time is a unix timestamp, that indicates the starting time for the tournament.
  Before this timestamp, reconciler waits the start.

  1. Mark the tournament is playing
  At start-time, submit the transaction to set the tournament status to `:playing`.

  2. Update ranks, tables and merge tables when the tournament is finished."
  (:require
   [cljs.core.async          :as a]
   [depk.transactor.constant :as c]
   [depk.transactor.event.protocol :as event-p]
   [depk.transactor.log      :as log]))

;;; Event broadcasts

(defn broadcast-state
  [state]
  {:type :system/tournament-broadcast,
   :data {:state state}})

(defn broadcast-next-game
  [game-id game-account-state finish?]
  {:type :system/tournament-broadcast,
   :data {:event {:type :system/next-game,
                  :data {:game-id            game-id,
                         :game-account-state game-account-state,
                         :finish?            finish?}}}})

(defn broadcast-resit-table
  [game-id resit-map finish?]
  {:type :system/tournament-broadcast,
   :data {:event {:type :system/resit-table,
                  :data {:game-id   game-id,
                         :resit-map resit-map,
                         :finish?   finish?}}}})

(defn broadcast-sync-state
  [game-id game-account-state]
  {:type :system/tournament-broadcast,
   :data {:event {:type :system/sync-state,
                  :data {:game-id            game-id,
                         :game-account-state game-account-state}}}})

(defn broadcast-start-tournament
  [state]
  (let [{:keys [start-time blinds-mode]} state
        ;; on-chain start-time is unix timestamp, we multiply 1000 here to convert it to ms
        ;; timestamp
        start-time (+ c/tournament-start-delay (* start-time 1000))]
    {:type :system/tournament-broadcast,
     :data {:state state,
            :event {:type :system/start-tournament,
                    :data {:games       (:games state),
                           :start-time  start-time,
                           :blinds-mode blinds-mode}}}}))

;;; Helpers

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
  [tournament-id size]
  {:players       [nil nil nil nil nil nil nil nil nil],
   :game-id       (str tournament-id "#" (gen-serial-number)),
   :size          size,
   :settle-serial 0,
   :buyin-serial  0,
   :mint-pubkey   "11111111111111111111111111111111"})

(defn- pop-game-player
  "Pop one player from game, return [player, new-game]."
  [{:keys [players], :as game}]
  (let [idx (count (take-while nil? players))]
    [(nth players idx)
     (update game :players assoc idx nil)]))

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
  [tournament-id num-games ranks size]
  (let [init (vec
              (repeatedly num-games
                          (partial make-pseudo-game-account-state
                                   tournament-id
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
  [{:keys [state updated-game-id game-resit-map], :as ctx}]
  (let [updated-game (get-game state updated-game-id)
        sync-state-game-ids (remove #{updated-game-id} (mapcat vals (vals game-resit-map)))
        finish?      (= 1 (count (filter #(> (:chips %) (js/BigInt 0)) (:ranks state))))
        events       (cond-> [
                              ;; Refresh the state snapshot
                              (broadcast-state state)]

                       ;; Notify game to continue
                       updated-game
                       (conj (broadcast-next-game updated-game-id updated-game finish?))

                       ;; Notify game to resit
                       (seq game-resit-map)
                       (into (map (fn [[game-id resit-map]]
                                    (let [game    (get-game state game-id)
                                          finish? (= 0 (count-game-players game))]
                                      (broadcast-resit-table game-id resit-map finish?)))
                                  game-resit-map))

                       ;; Tell other games new players are coming
                       sync-state-game-ids
                       (into (map (fn [gid]
                                    (broadcast-sync-state gid (get-game state gid)))
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
        sorted-games    (vec (sort-by count-game-players games))
        game-with-least (first sorted-games)
        game-with-most  (last sorted-games)
        ;; curr-game    (first sorted-games)
        ;; other-games  (vec (next sorted-games))

        [game-resit-map new-games]
        (cond
          ;; No-op with final table
          (= 1 (count games))
          nil

          ;; Updated game is the game with least players
          ;; If there's enough seats at another tables, current table will be closed.
          (and (= updated-game-id (:game-id game-with-least))
               (<= (count-game-players game-with-least)
                   (transduce (map (fn [g] (- size (count-game-players g))))
                              +
                              0
                              (next sorted-games))))
          (loop [ps        (filter some? (:players (first sorted-games)))
                 games     (vec (next sorted-games))
                 idx       0
                 resit-map {}]
            (if-not (seq ps)
              [{(:game-id game-with-least) resit-map} games]
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
                       resit-map))))

          ;; Updated game is the game with most players
          ;; Will try to balance the players with table with least players by moving 1 player.
          (and (= updated-game-id (:game-id game-with-most))
               (> (count-game-players game-with-most) (inc (count-game-players game-with-least))))
          (let [[p-to-sit new-game-with-most] (pop-game-player game-with-most)
                new-game-with-least (sit-in-game game-with-least p-to-sit)
                new-games (-> sorted-games
                              (assoc 0 new-game-with-least)
                              (assoc (dec (count games)) new-game-with-most))]
            [{(:game-id game-with-most) {(:pubkey p-to-sit) (:game-id game-with-least)}}
             new-games]))

        resit-map       (first (vals game-resit-map))]

    (log/log "🌠" tournament-id "Resit: %s" resit-map)

    (if (seq game-resit-map)
      (-> ctx
          (update :state assoc :games new-games)
          (update-in [:state :resit-map] merge resit-map)
          (assoc :game-resit-map game-resit-map))
      ctx)))

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

;;; Event handlers

(defmulti apply-event
  (fn [_state event]
    (:type event)))

;; The starting event for reconciler
(defmethod apply-event :system/start-tournament
  [{:keys [tournament-id], :as state} _]
  (let [{:keys [ranks num-players status size]} state
        ranks     (filter some? ranks)
        ;; calculate the number of tables
        num-games (quot (+ (dec size) num-players) size)

        ;; assign players to games
        new-state (cond-> state
                    (= :playing status)
                    (assoc :games (assign-players-to-games tournament-id num-games ranks size)))

        evts      (cond-> []

                    (= :registering status)
                    (conj {:type :system/submit-start-tournament,
                           :data {}})

                    (= :playing status)
                    (conj
                     (broadcast-state new-state)
                     (broadcast-start-tournament new-state)))]
    (log/log "🌠" tournament-id "Start tournament, creating %s tables" num-games)
    [new-state evts]))

;; Sync on-chain tournament state
(defmethod apply-event :system/sync-tournament-state
  [{:keys [tournament-id], :as old-state}
   {{:keys [state]} :data}]
  (cond
    ;; Send start tournament game when StartTournament transaction is finalized
    ;; Create games
    ;; So frontend can join.
    (and (= :registering (:status old-state))
         (= :playing (:status state)))
    (let [{:keys [ranks num-players size]} state
          ranks     (filter some? ranks)
          num-games (quot (+ (dec size) num-players) size)
          games     (assign-players-to-games tournament-id num-games ranks size)
          new-state (assoc state :games games)]
      (log/log "🌠"
               (:tournament-id old-state)
               "StartTournament transaction finalized. Send start games.")
      [new-state
       [(broadcast-state new-state)
        (broadcast-start-tournament new-state)]])

    ;; Update state during the registration
    (= :registering (:status old-state))
    (do
      (log/log "🌠" (:tournament-id old-state) "Update tournament state.")
      [state
       [(broadcast-state state)]])

    ;; No-op after started.
    :else
    [old-state nil]))

;; Handle the settlement, which is submitted by a tournament game.
(defmethod apply-event :system/tournament-game-settle
  [state
   {:keys [game-id],
    {:keys [settle-map]} :data}]
  (let [ctx (-> {:state state, :events []}
                (update-games game-id settle-map)
                (update-ranks settle-map)
                (maybe-settle)
                (resit-players)
                (make-notify-events))
        {:keys [state events]} ctx]
    [state events]))

;;; Reconciler definition
(defn start-reconciler
  [{:keys [input output]} {:keys [tournament-id init-state]}]
  (let [current-time (current-unix-timestamp)
        {:keys [start-time status]} init-state]

    ;; Starting
    ;; The event loop will start with `system/start-tournament` event.
    ;; If current time is before the start time, schedule a start event;
    ;; If current time is after the start time, start immediately;
    ;; Do nothing if the tournament is already completed.
    (if (#{:playing :registering} status)
      (a/go
       (if (> current-time start-time)
         ;; Start directly
         (do
           (log/log "🌠" tournament-id "Starting tournament")
           (a/>! input {:type :system/start-tournament}))

         ;; Trigger start event after timeout
         (let [secs (- start-time current-time)]
           (log/log "🌠" tournament-id "Schedule tournament starting after %s seconds" secs)
           (a/<! (a/timeout (* 1000 secs)))
           (a/>! input {:type :system/start-tournament}))))
      (do
        (log/log "🌠" tournament-id "Tournament already completed")
        (a/close! input))))

  ;; Event loop
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
