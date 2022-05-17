(ns depk.transactor.game.event-handler.misc
  (:require
   [depk.transactor.game.models :as m]
   [depk.transactor.game.encrypt :as e]
   [depk.transactor.game.evaluator :as evaluator]
   [depk.transactor.util :as    u
                         :refer [go-try <!?]]
   [depk.transactor.constant :as c]
   [cljs.core.async :as a]
   [clojure.set :as set]
   [goog.string :as gstr]
   [depk.transactor.log :as log]))

(defn- update-vals*
  [f m]
  (->> (for [[k v] m]
         [k (f v)])
       (into {})))

;; errors

(defn player-already-alive!
  [state event]
  (throw (ex-info "Player already alive"
                  {:state state,
                   :event event})))

(defn state-already-merged!
  [state event]
  (throw (ex-info (gstr/format "State already merged: %s" (:game-id state))
                  {:state state,
                   :event event})))

(defn invalid-game-status!
  [state event]
  (throw (ex-info (gstr/format "Invalid game status: %s" (:status state))
                  {:state state,
                   :event event})))

(defn invalid-player-id!
  [state event]
  (throw (ex-info "Invalid player id"
                  {:state state,
                   :event event})))

(defn invalid-share-key!
  [state event]
  (throw (ex-info "Invalid share key"
                  {:state state,
                   :event event})))

(defn empty-share-keys!
  [state event]
  (throw (ex-info "Empty share keys"
                  {:state state,
                   :event event})))

(defn empty-released-keys!
  [state event]
  (throw (ex-info "Released keys"
                  {:state state,
                   :event event})))

(defn cant-leave-game!
  [state event]
  (throw (ex-info "Can't leave game"
                  {:state state,
                   :event event})))

(defn cant-update-rsa-pub!
  [state event]
  (throw (ex-info "Can't update RSA public key"
                  {:state state,
                   :event event})))

(defn invalid-rsa-pub!
  [state event]
  (throw (ex-info "Invalid RSA public key"
                  {:state state,
                   :event event})))

(defn invalid-sig!
  [state event]
  (throw (ex-info "Invalid signature"
                  {:state state,
                   :event event})))

(defn community-cards-decryption-failed!
  [err-data]
  (throw (ex-info "Community cards decryption failed" err-data)))

(defn showdown-cards-decryption-failed!
  [err-data]
  (throw (ex-info "Showdown cards decryption failed" err-data)))

(defn player-not-in-action!
  [state event]
  (throw (ex-info "Player not in action"
                  {:state state,
                   :event event})))

(defn player-cant-check!
  [state event]
  (throw (ex-info "Player can't check"
                  {:state state,
                   :event event})))

(defn player-cant-bet!
  [state event]
  (throw (ex-info "Player can't bet"
                  {:state state,
                   :event event})))

(defn player-cant-raise!
  [state event]
  (throw (ex-info "Player can't raise"
                  {:state state,
                   :event event})))

(defn player-bet-too-small!
  [state event]
  (throw (ex-info "Player bet too small"
                  {:state state,
                   :event event})))

(defn player-raise-too-small!
  [state event]
  (throw (ex-info "Player raise too small"
                  {:state state,
                   :event event})))

(defn invalid-amount!
  [state event]
  (throw (ex-info "Invalid amount"
                  {:state state,
                   :event event})))

(defn sng-finished!
  [state event]
  (throw (ex-info "SNG finished!" {:state state, :event event})))

(defn invalid-next-state-case!
  [state]
  (throw (ex-info "Invalid next state case"
                  {:state state})))

;; helpers

(def suits #{:d :s :h :c})

(def kinds #{:a :2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k})

(defn remove-eliminated-players
  "Remove all players who have no chips"
  [{:keys [player-map], :as state}]
  (let [eliminated-pids (->> player-map
                             vals
                             (filter #(= (js/BigInt 0) (:chips %)))
                             (map :player-id))
        remove-by-pids  (fn [m]
                          (apply dissoc m eliminated-pids))]
    (log/infof "🧹Remove eliminated players: %s" eliminated-pids)
    (-> state
        (update :player-map remove-by-pids)
        (update :rsa-pub-map remove-by-pids)
        (update :sig-map remove-by-pids))))

(defn reset-sng-state
  [{:keys [winner-id base-sb base-bb], :as state}]
  (if winner-id
    (do (log/infof "🧹Clean up SNG state")
        (assoc state
               :player-map {}
               :start-time nil
               :sb         base-sb
               :bb         base-bb))
    state))

(defn remove-non-alive-players
  "Remove all players who doesn't have live online status."
  [{:keys [player-map game-type start-time], :as state}]
  (if (or (= :cash game-type)
          (and (#{:bonus :sng} game-type)
               (nil? start-time)))
    (let [dropout-pids   (->> player-map
                              vals
                              (remove #(= :normal (:online-status %)))
                              (map :player-id))
          remove-by-pids (fn [m]
                           (apply dissoc m dropout-pids))]
      (log/infof "🧹Remove non-alive players: %s" dropout-pids)
      (-> state
          (update :player-map remove-by-pids)
          (update :rsa-pub-map remove-by-pids)
          (update :sig-map remove-by-pids)))
    state))

(defn- build-settle-map
  "Build settle-map.

  settle-map is a map from player id to settle-item.

  settle-item is a map of following keys:
    * settle-type, how chips will change. can be :no-update, :chips-add, :chips-sub
    * settle-status, how player status change. can be :no-update, :empty-seat, :leave
    * amount, the amount change
  "
  [chips-change-map player-map]
  (->> (for [[pid {:keys [online-status chips]}] player-map]
         (let [settle-status (if (and (= online-status :normal)
                                      (not= chips (js/BigInt 0)))
                               :no-update
                               :leave)
               chips-change  (get chips-change-map pid (js/BigInt 0))
               settle-type   (cond
                               (> chips-change (js/BigInt 0))
                               :chips-add

                               (< chips-change (js/BigInt 0))
                               :chips-sub

                               :else
                               :no-update)
               amount        (if (< chips-change (js/BigInt 0)) (- chips-change) chips-change)]
           [pid
            {:settle-type   settle-type,
             :settle-status settle-status,
             :amount        amount}]))
       (into {})))

(defn submit-non-alive-players
  "Submit a request to remove non alive on-chain players."
  [{:keys [player-map game-account-state game-type start-time chips-change-map], :as state}]
  (if (and (or (= :cash game-type)
               (and (#{:bonus :sng} game-type)
                    (nil? start-time)))
           (some #{:dropout :leave} (map :online-status (vals player-map))))
    (do
      (log/infof "🧹Submit remove players, players: %s"
                 (->> (vals player-map)
                      (remove (comp #{:normal} :online-status))
                      (map :player-id)))

      (let [settle-map (build-settle-map chips-change-map player-map)
            request
            {:type :system/settle,
             :data {:settle-map    settle-map,
                    :rake          (js/BigInt 0),
                    :settle-serial (:settle-serial game-account-state)}}]
        (log/infof "✈️Submit game result for players' leaving")
        ;; send settlement for leaving players
        (update state :api-requests conj request)))
    state))

(defn valid-card?
  [card]
  (and (vector? card)
       (= 2 (count card))
       (suits (first card))
       (kinds (second card))))

(def default-deck-of-cards
  (->> (for [s suits
             k kinds]
         [s k])
       (into [])))

(defn next-position-player
  "Return [position player] for the player on next position."
  [state position]
  (let [{:keys [player-map]} state
        pos-pid (->> player-map
                     (map (juxt (comp :position val) key))
                     (sort-by first))]
    (or (first (drop-while #(<= (first %) position) pos-pid))
        (first pos-pid))))

(defn next-player-id
  "Return next player state."
  [state player-id]
  (let [pos (get-in state [:player-map player-id :position])]
    (second (next-position-player state pos))))

(defn btn-player-id?
  "Return if player-id is the BTN."
  [state player-id]
  (= (:btn state) (get-in state [:player-map player-id :position])))

(defn player-online?
  [player]
  (= (:online-status player) :normal))

(defn get-player-by-position
  "Find player state in player-map by position."
  [state position]
  (->> state
       :player-map
       vals
       (filter #(= position (:position %)))
       first))

(defn list-players-in-order
  "Return a player state list in position order.

  The player at last-pos will be listed at the last."
  ([state]
   (list-players-in-order state (:btn state)))
  ([state last-pos]
   (list-players-in-order state last-pos (constantly true)))
  ([state last-pos filter-or-status-set]
   (let [{:keys [player-map]} state
         f (if (set? filter-or-status-set)
             (comp filter-or-status-set :status)
             filter-or-status-set)]
     (->> player-map
          vals
          (filter f)
          (sort-by (fn [{:keys [position]}]
                     (if (> position last-pos)
                       (- position last-pos)
                       (+ 10000 position))))))))

(defn set-operation-player-ids
  "Return a list of player ids, participate in encryption.

  Dropout players will not be counted."
  [state]
  (let [{:keys [btn]} state
        player-ids    (->> (map :player-id (list-players-in-order state btn player-online?))
                           (take 3))]
    (log/infof "🫱Op player ids: %s" player-ids)
    (assoc state :op-player-ids player-ids)))

(defn next-op-player-id
  [state current-player-id]
  (let [{:keys [op-player-ids]} state]
    (if (not current-player-id)
      (first op-player-ids)
      (let [rest (next (drop-while #(not= % current-player-id) op-player-ids))]
        (if (seq rest)
          (first rest)
          (first op-player-ids))))))

(defn with-next-op-player-id-as
  [state k]
  (let [id (next-op-player-id state (get state k))]
    (assoc state k id)))

(defn reset-player-map-status
  [{:keys [player-map], :as state}]
  (let [new-player-map (->> (for [[pid p] player-map]
                              [pid
                               (assoc p
                                      :status        :player-status/wait
                                      :online-status :dropout)])
                            (into {}))]
    (assoc state :player-map new-player-map)))

(defn increase-blinds
  [{:keys [base-sb base-bb game-type start-time], :as state}]
  (if (and (#{:sng :bonus} game-type)
           start-time)
    (let [cnt (js/BigInt
               (inc (int (/ (- (.getTime (js/Date.)) start-time)
                            c/increase-blinds-interval))))
          sb  (* base-sb cnt)
          bb  (* base-bb cnt)]
      (log/infof "🌶️Maintain blinds, start-time: %s count: % sb: %s bb: %s"
                 start-time
                 cnt
                 sb
                 bb)
      (-> state
          (assoc :sb sb
                 :bb bb)))
    state))

(defn reset-game-state
  "Reset game state. All player status will be set to wait/dropout."
  [state]
  (-> state
      (merge
       {:status             :game-status/init,
        :released-keys-map  nil,
        :street             nil,
        :card-ciphers       [],
        :after-keyshare     nil,
        :require-key-idents nil,
        :share-key-map      nil,
        :community-cards    nil,
        :min-raise          nil,
        :street-bet         nil,
        :bet-map            nil,
        :collect-bet-map    nil,
        :pots               [],
        :showdown-map       nil,
        :prize-map          nil,
        :player-actions     [],
        :winning-type       nil,
        :winner-id          nil,
        :after-key-share    nil,
        :chips-change-map   nil,
        :rake-fee           nil})))

(defn get-player-hole-card-indices
  [{:keys [btn player-map], :as state}]
  (let [pid-to-idx (->> (list-players-in-order state btn)
                        (map-indexed (fn [idx p] [(:player-id p) idx]))
                        (into {}))
        idxs       (partition 2 (range))]
    (->> (for [[pid] player-map]
           [pid (nth idxs (get pid-to-idx pid))])
         (into {}))))

(defn dispatch-reset
  "Dispatch reset event."
  [state]
  (assoc state
         :dispatch-event
         [(if (:winner-id state)
            c/sng-next-game-timeout-delay
            c/reset-timeout-delay)
          (m/make-event :system/reset state {})]))

(defn next-btn
  [state]
  (let [[next-btn _] (next-position-player state (get state :btn 0))]
    next-btn))

(defn with-next-btn
  [state]
  (let [btn (get state :btn 0)
        [next-btn _] (next-position-player state btn)]
    (assoc state :btn next-btn)))

(defn can-quick-start?
  [{:keys [player-map game-type size]}]
  (let [all-ready? (every? #(= :normal (:online-status %)) (vals player-map))]
    (or (and (= :cash game-type)
             (and all-ready?
                  (>= (count player-map) 2)))

        (and (#{:bonus :sng} game-type)
             (and all-ready?
                  (= (count player-map) size))))))

(defn dispatch-start-game
  [state & [start-delay]]
  (-> state
      (assoc :dispatch-event
             [(cond
                start-delay start-delay
                (can-quick-start? state) c/continue-start-game-delay
                :else c/default-start-game-delay)
              (m/make-event :system/start-game state {})])))

(defn mark-dropout-players
  [state player-ids]
  (reduce (fn [state id]
            (update-in state
                       [:player-map id]
                       assoc
                       :online-status :dropout
                       ;; Setting to fold is neccessary for game state flow
                       :status        :player-status/fold))
          state
          player-ids))

(defn dispatch-encrypt-timeout
  [state]
  (assoc state
         :dispatch-event
         [c/encrypt-timeout-delay
          (m/make-event :system/encrypt-timeout state {})]))

(defn dispatch-shuffle-timeout
  [state]
  (assoc state
         :dispatch-event
         [c/shuffle-timeout-delay
          (m/make-event :system/shuffle-timeout state {})]))

(defn dispatch-key-share-timeout
  [state]
  (assoc state
         :dispatch-event
         [c/key-share-timeout-delay
          (m/make-event :system/key-share-timeout state {})]))

(defn dispatch-player-action-timeout
  [{:keys [action-player-id player-map], :as state}]
  (assoc state
         :dispatch-event
         [(if (= :normal (get-in player-map [action-player-id :online-status]))
            c/player-action-timeout-delay
            c/droupout-player-action-timeout-delay)
          (m/make-event :system/player-action-timeout state {})]))

(defn valid-key-ident?
  [state key-ident player-id]
  (and (contains? (:require-key-idents state) key-ident)
       (not (contains? (:share-key-map state) key-ident))
       (= (first key-ident) player-id)))

(defn list-missing-key-idents
  "Return a list of key idents those still not provided yet."
  [state]
  (let [{:keys [require-key-idents share-key-map]} state]
    (->> share-key-map
         keys
         set
         (set/difference require-key-idents))))

(defn take-released-keys
  "Take released keys, merge into share-key-map.

  If a client released its keys and left the game,
  its released keys should be visible to all."
  [{:keys [require-key-idents share-key-map released-keys-map],
    :as   state}]
  (let [released-share-key-map (->> require-key-idents
                                    (filter (complement (or share-key-map {})))
                                    (keep
                                     (fn [[pid _type idx :as idt]]
                                       (when-let [k (get-in released-keys-map [pid idx])]
                                         [idt k])))
                                    (into {}))]
    (log/infof "🔐Take released keys: %s" released-share-key-map)
    (-> state
        (update :share-key-map merge released-share-key-map))))

(defn- list-require-hole-cards-key-idents
  [op-player-ids player-ids card-idx-2d]
  (let [player-id-set (set op-player-ids)]
    (->> (mapcat (fn [player-id card-idxs]
                   (for [other-player-id (disj player-id-set player-id)
                         card-idx        card-idxs]
                     [other-player-id :hole-card card-idx player-id]))
          player-ids
          card-idx-2d)
         (into #{}))))

(defn- list-require-community-cards-key-idents
  [op-player-ids card-idxs]
  (->> (for [player-id op-player-ids
             card-idx  card-idxs]
         [player-id :community-card card-idx])
       (into #{})))

(defn- list-require-showdown-key-idents
  "Return key indents for all hole-cards of those players who have not fold their cards."
  [{:keys [player-map op-player-ids], :as state}]
  (let [hole-card-indices (get-player-hole-card-indices state)]
    (->> player-map
         vals
         (filter (comp #{:player-status/acted :player-status/allin} :status))
         (mapcat (fn [{:keys [player-id]}]
                   (for [idx (get hole-card-indices player-id)
                         pid op-player-ids]
                     [pid :showdown-card idx])))
         (into #{}))))

(defn list-require-key-idents
  [{:keys [btn street after-key-share op-player-ids], :as state}]
  ;; {:pre [(some? street) (int? btn)]}
  (let [player-ids (->> (list-players-in-order state btn)
                        (map :player-id))]
    (cond
      ;; runner will require all keys
      (= :runner after-key-share)
      (let [skip (* 2 (count player-ids))]
        (set/union
         (list-require-showdown-key-idents state)
         (list-require-community-cards-key-idents op-player-ids (range skip (+ skip 5)))))

      (= :street/preflop street)
      (list-require-hole-cards-key-idents op-player-ids player-ids (partition 2 (range)))

      (= :street/flop street)
      (let [skip (* 2 (count player-ids))]
        (list-require-community-cards-key-idents op-player-ids (range skip (+ skip 3))))

      (= :street/turn street)
      (let [idx (+ (* 2 (count player-ids)) 3)]
        (list-require-community-cards-key-idents op-player-ids [idx]))

      (= :street/river street)
      (let [idx (+ (* 2 (count player-ids)) 4)]
        (list-require-community-cards-key-idents op-player-ids [idx]))

      (= :street/showdown street)
      (list-require-showdown-key-idents state))))

(defn update-require-key-idents
  "Update require-key-idents in state."
  [state]
  (let [require-key-idents (list-require-key-idents state)
        into-set (fnil into #{})]
    (update state :require-key-idents into-set require-key-idents)))

(defn update-prize-map
  [{:keys [pots btn mint-info player-map base-sb], :as state}]
  (log/info "🏆Update prize map, pots:")
  (doseq [p pots]
    (log/infof "🏆-%s" (prn-str p)))

  (let [{:keys [decimals]} mint-info
        min-val            (/ base-sb (js/BigInt 10))
        prize-map          (->> pots
                                (mapcat (fn [{:keys [amount winner-ids]}]
                                          (let [cnt      (js/BigInt (count winner-ids))
                                                reminder (mod amount (* cnt min-val))
                                                prize    (/ (- amount reminder) cnt)]
                                            (cons
                                             {:reminder reminder}
                                             (for [id winner-ids]
                                               {id prize})))))
                                (apply merge-with (fnil + (js/BigInt 0) (js/BigInt 0))))
        reminder           (get prize-map :reminder (js/BigInt 0))
        prize-map          (dissoc prize-map :reminder)
        reminder-player-id (or (-> (list-players-in-order
                                    state
                                    btn
                                    (fn [{:keys [player-id status]}]
                                      (and (#{:player-status/acted :player-status/allin
                                              :player-status/wait}
                                            status)
                                           (get prize-map player-id))))
                                   first
                                   :player-id)
                               ;; It's possible to have no reminder player id
                               ;; In this case, we use the first player id
                               (ffirst player-map))
        _ (log/infof "🍀Reminder player id: %s" reminder-player-id)
        prize-map          (update prize-map
                                   reminder-player-id
                                   (fnil +
                                         (js/BigInt 0)
                                         (js/BigInt 0))
                                   reminder)]
    (assoc state :prize-map prize-map)))

(defn take-rake
  [{:keys [game-type prize-map rake street], :as state}]
  (if (and (= game-type :cash)
           (not= street :street/preflop))
    (let [total-1   (->> (vals prize-map)
                         (reduce + (js/BigInt 0)))
          prize-map (update-vals* (fn [v]
                                    (if (> v (js/BigInt 0))
                                      (/ (* v (- (js/BigInt 1000) rake)) (js/BigInt 1000))
                                      v))
                                  prize-map)
          total-2   (->> (vals prize-map)
                         (reduce + (js/BigInt 0)))
          rake-fee  (- total-1 total-2)]
      (log/infof "🧾Total Prize Before Rake: %s" total-1)
      (log/infof "🧾Total Prize After Rake: %s" total-2)
      (log/infof "🧾Rake: %s" rake-fee)
      (assoc state
             :prize-map prize-map
             :rake-fee  rake-fee))
    (assoc state :rake-fee (js/BigInt 0))))

(defn update-chips-change-map
  "Update chips-change-map in state.

  Depends on player-map and pots."
  [{:keys [prize-map player-map pots], :as state}]
  (let [init-map         (->> (for [[pid] player-map]
                                [pid (js/BigInt 0)])
                              (into {}))

        total-bet-map    (->> pots
                              (mapcat (fn [{:keys [owner-ids amount]}]
                                        (let [bet (/ amount (js/BigInt (count owner-ids)))]
                                          (for [id owner-ids]
                                            {id bet}))))
                              (apply merge-with (fnil + (js/BigInt 0) (js/BigInt 0))))

        chips-change-map (->> (merge-with (fnil - (js/BigInt 0) (js/BigInt 0))
                                          init-map
                                          total-bet-map)
                              (merge-with (fnil + (js/BigInt 0) (js/BigInt 0))
                                          prize-map))]

    (log/info "💰Players' prize map")
    (doseq [[p a] prize-map]
      (log/infof "💰-%s \t%s" p a))

    (log/info "💰Players' chips change")
    (doseq [[p c] chips-change-map]
      (log/infof "💰-%s \t%s \t%s" p (get-in player-map [p :chips]) c))

    (assoc state :chips-change-map chips-change-map)))

(defn take-bet-from-player
  "Take bet from player state, return [bet, new-player-state, allin?]"
  [{:keys [chips], :as player} bet]
  (let [[real-bet allin?] (if (< bet chips)
                            [bet false]
                            [chips true])]
    [real-bet (update player :chips - real-bet) allin?]))

(defn ask-player-for-action
  [state player-id]
  (-> state
      (assoc :action-player-id player-id)
      (assoc-in [:player-map player-id :status] :player-status/in-action)
      (dispatch-player-action-timeout)))

(defn blind-bets
  "Make blind bets for SB and BB, ask next player to action."
  [{:keys [sb bb btn player-map], :as state}]
  (let [players               (list-players-in-order state btn)
        [sb-player bb-player] (if (= 2 (count players))
                                ;; two player, BTN is SB
                                (reverse players)
                                players)

        [bb-bet bb-player bb-allin?] (take-bet-from-player bb-player bb)
        [sb-bet sb-player sb-allin?] (take-bet-from-player sb-player sb)

        new-sb-player         (if sb-allin?
                                (assoc sb-player :status :player-status/allin)
                                sb-player)
        new-bb-player         (if bb-allin?
                                (assoc bb-player :status :player-status/allin)
                                bb-player)

        sb-player-id          (:player-id new-sb-player)
        bb-player-id          (:player-id new-bb-player)

        bet-map               {sb-player-id sb-bet,
                               bb-player-id bb-bet}

        action-player-id      (if (= 2 (count players))
                                sb-player-id
                                (:player-id (nth players 2)))

        new-player-map        (assoc player-map
                                     sb-player-id
                                     new-sb-player
                                     bb-player-id
                                     new-bb-player)]
    (-> state
        (assoc
         :bet-map    bet-map
         :player-map new-player-map
         :min-raise  bb
         :street-bet bb)
        (ask-player-for-action action-player-id))))

(defn apply-prize-map
  "Update player chips by prize-map."
  [{:keys [player-map prize-map], :as state}]
  (let [new-player-map (merge-with (fn [player prize]
                                     (update player :chips + prize))
                                   player-map
                                   prize-map)]
    (assoc state :player-map new-player-map)))

(defn assign-winner-to-pots
  "set winner-ids in pots.

  winner-id-sets is a list of player-id set, ranked from strongest hands to weakest."
  [{:keys [pots], :as state} winner-id-sets]
  ;; (log/infof "Assign winner to pots: %s" winner-id-sets)
  (let [new-pots (->> pots
                      (mapv (fn [pot]
                              (loop [[id-set & rest-id-sets] winner-id-sets]
                                (let [winner-ids (set/intersection (:owner-ids pot) id-set)]
                                  (if (seq winner-ids)
                                    (assoc pot :winner-ids winner-ids)
                                    (recur rest-id-sets)))))))]
    (assoc state :pots new-pots)))

(defn collect-bet-to-pots
  "Update pots.

  When not all players have the same bet, multiple pots will be created.
  The value of :pots is a vec. The first is the main pot, and the followings are side pots."
  [{:keys [player-map bet-map pots], :as state}]

  (let [steps          (->> bet-map
                            vals
                            distinct
                            ;; Here we deal with js/BigInt
                            ;; sort-by needs a result of -/+/0 in Number
                            (sort-by identity #(js/parseInt (- %1 %2))))

        ;; some example for owner-ids:
        ;;   [[a] [a b] [a b c d]]
        ;;   [[a b] [a b c] [a b c d]]
        owner-ids-list (->> steps
                            (map (fn [step]
                                   (->> bet-map
                                        (filter #(<= step (second %)))
                                        (map key)
                                        (set)))))

        new-pots       (loop [pots []
                              acc  (js/BigInt 0)
                              [owner-ids & next-owner-ids-list] owner-ids-list]
                         (if-not owner-ids
                           (reverse pots)
                           (let [amount-1 (- (apply min (vals (select-keys bet-map owner-ids))) acc)
                                 amount   (* amount-1 (js/BigInt (count owner-ids)))]
                             (recur (conj pots (m/make-pot owner-ids amount))
                                    (+ acc amount-1)
                                    next-owner-ids-list))))

        ;; pots with only one owner is meaningless
        ;; the amount will be returned to chips
        [ret-player-id ret-bet new-pots] (let [main-pot (first new-pots)]
                                           (if (= 1 (count (:owner-ids main-pot)))
                                             [(first (:owner-ids main-pot))
                                              (:amount main-pot)
                                              (next new-pots)]
                                             [nil nil new-pots]))

        ;; Apply return bet to that player
        player-map     (if ret-player-id
                         (update-in player-map [ret-player-id :chips] + ret-bet)
                         player-map)

        ;; Merge pots by same owner-ids
        rep-pots       (->> (into (or pots []) new-pots)
                            (partition-by #(count (:owner-ids %)))
                            (map (fn [pots]
                                   (m/make-pot (:owner-ids (first pots))
                                               (transduce (map :amount) + (js/BigInt 0) pots)))))]

    ;; Log pots
    (log/info "💵Current pots:")
    (doseq [pot rep-pots]
      (log/infof "💵%s - %s" (:amount pot) (:owner-ids pot)))
    (when ret-player-id
      (log/infof "💵Return bet %s to %s" ret-bet ret-player-id))

    (-> state
        (assoc :pots rep-pots)
        (assoc :collect-bet-map bet-map)
        (assoc :player-map player-map)
        (assoc :bet-map nil))))

(defn prepare-showdown
  "Player showdown hole cards."
  [{:keys [], :as state}]
  (-> state
      (collect-bet-to-pots)
      (assoc :street          :street/showdown
             :status          :game-status/key-share
             :after-key-share :settle)
      (update-require-key-idents)
      (take-released-keys)))

(defn prepare-runner
  "Prepare runner for allin.

  Will require all clients provide their keys for community cards."
  [state]
  [{:keys [], :as state}]
  (-> state
      (collect-bet-to-pots)
      (assoc :status          :game-status/key-share
             :after-key-share :runner
             :street          :street/showdown
             :bet-map         nil)
      (update-require-key-idents)
      (take-released-keys)))

(defn- decrypt-community-cards
  [{:keys [op-player-ids player-map share-key-map card-ciphers], :as state}]
  (go-try
   (let [offset    (* 2 (count player-map))
         idxs      (->> offset
                        (iterate inc)
                        (take 5)
                        vec)
         ciphers   (e/hex->ciphers card-ciphers)
         ciphers   (mapv ciphers idxs)
         aes-keys  (<!?
                    (->> op-player-ids
                         (mapcat (fn [id]
                                   (vec
                                    (for [idx idxs]
                                      (e/import-aes-key
                                       (get share-key-map [id :community-card idx]))))))
                         (a/map vector)))
         keys-2d   (partition 5 aes-keys)
         decrypted (<!? (e/decrypt-ciphers-with-keys-2d ciphers keys-2d))
         cards     (e/decrypted-ciphers->cards decrypted)]
     (when (or (not= (count ciphers) (count cards))
               (not (every? valid-card? cards)))
       (community-cards-decryption-failed! {:count-ciphers (count ciphers),
                                            :count-cards   (count cards),
                                            :cards         cards}))

     cards)))

(defn- decrypt-showdown-card-map
  [{:keys [op-player-ids card-ciphers share-key-map player-map], :as state}]
  (go-try
   (let [orig-ciphers      (e/hex->ciphers card-ciphers)

         ;; pick the ciphers we need
         ;; all community cards and showdown hole cards
         ;; rel is a [[cipher idx player-id]]
         hole-card-indices (get-player-hole-card-indices state)
         rel               (->> player-map
                                vals
                                (filter (comp #{:player-status/acted :player-status/allin} :status))
                                (mapcat (fn [{:keys [player-id]}]
                                          (for [idx (get hole-card-indices player-id)]
                                            [(nth orig-ciphers idx) idx player-id]))))

         ciphers           (mapv first rel)
         idxs              (mapv second rel)
         ids               (mapv last rel)

         aes-keys          (<!? (->> op-player-ids
                                     (mapcat (fn [id]
                                               (vec
                                                (for [idx idxs]
                                                  (e/import-aes-key
                                                   (get share-key-map [id :showdown-card idx]))))))
                                     (a/map vector)))

         keys-2d           (partition (count rel) aes-keys)
         decrypted         (<!? (e/decrypt-ciphers-with-keys-2d ciphers keys-2d))
         cards             (e/decrypted-ciphers->cards decrypted)]

     ;; (js/console.log "orig-ciphers: " orig-ciphers)
     ;; (js/console.log "hole-card-indices: " hole-card-indices)
     ;; (js/console.log "rel: " rel)
     ;; (js/console.log "player-map: " player-map)
     ;; (js/console.log "state: " state)

     (when (or (not= (count ciphers) (count cards))
               (not (every? valid-card? cards)))
       (showdown-cards-decryption-failed! {:count-ciphers (count ciphers),
                                           :count-cards   (count cards),
                                           :cards         cards}))

     (->> (mapv vector ids cards)
          (group-by first)
          (update-vals* #(mapv last %))))))

(defn- submit-game-result-cash
  [{:keys [chips-change-map player-map game-account-state rake-fee], :as state}]
  (let [settle-map (build-settle-map chips-change-map player-map)

        request
        {:type :system/settle,
         :data {:settle-map    settle-map,
                :rake          rake-fee,
                :settle-serial (:settle-serial game-account-state)}}]
    (log/infof "✈️Submit game result for CASH game")
    (-> state
        (update :api-requests conj request)
        (assoc :rake-fee nil))))

(defn- submit-game-result-sng
  [{:keys [player-map], :as state}]
  (let [live-players (->> player-map
                          vals
                          (filter #(> (:chips %) (js/BigInt 0))))]
    (if (= 1 (count live-players))
      (let [winner-id (->> live-players
                           first
                           :player-id)
            request   {:type :system/set-winner,
                       :data {:winner-id winner-id}}]
        (log/infof "✈️Submit game result for SNG game")
        (-> state
            (update :api-requests conj request)
            (assoc :winner-id winner-id)))
      state)))

(defn- submit-game-result
  "Add request to :api-requests, submit game result."
  [{:keys [game-type], :as state}]
  (case game-type
    :cash
    (submit-game-result-cash state)

    (:sng :bonus :tournament)
    (submit-game-result-sng state)))

(defn terminate
  "Terminate current game.

  Bet will be returned and all non-fold users are marked as winner."
  [{:keys [player-map bet-map], :as state} non-compliant-player-ids]
  (let [winner-player-ids (->> player-map
                               (vals)
                               (filter (fn [p]
                                         (and (not= :player-status/fold (:status p))
                                              (= :normal (:online-status p)))))
                               (map :player-id)
                               (filter (complement non-compliant-player-ids))
                               (into #{}))

        ;; If no one shared its keys.
        ;; Then everyone is the winner.
        winner-player-ids (if (seq winner-player-ids)
                            winner-player-ids
                            (set (keys player-map)))]

    (log/infof "🩹Terminate game, winner ids: %s, revert bet-map: %s"
               winner-player-ids
               bet-map)
    (-> state
        (assign-winner-to-pots [winner-player-ids])
        (update-prize-map)
        (take-rake)
        (apply-prize-map)
        (update-chips-change-map)
        (submit-game-result)
        (remove-non-alive-players)
        (dispatch-reset)
        ;; TODO, new status/type ?
        (assoc :status       :game-status/settle
               :winning-type :last-player))))

(defn- compare-value
  [v1 v2]
  (->> (map compare v2 v1)
       (remove #{0})
       first))

(defn settle
  [{:keys [community-cards], :as state} winning-type]
  (go-try
   (let [showdown-card-map (<!? (decrypt-showdown-card-map state))
         community-cards   (or community-cards (<!? (decrypt-community-cards state)))
         showdown          (->> (for [[player-id hole-cards] showdown-card-map]
                                  (let [res (evaluator/evaluate-cards (concat hole-cards
                                                                              community-cards))]
                                    [player-id
                                     (assoc res
                                            :player-id  player-id
                                            :hole-cards hole-cards)]))
                                (into {}))
         winner-id-sets    (->> showdown
                                (group-by (comp :value second))
                                (sort-by first compare-value)
                                (mapv #(set (mapv first (second %)))))]

     (-> state
         (assoc :showdown-map showdown)
         (assign-winner-to-pots winner-id-sets)
         (update-prize-map)
         (take-rake)
         (apply-prize-map)
         (update-chips-change-map)
         (submit-game-result)
         (dispatch-reset)
         (assoc :status       :game-status/showdown
                :winning-type winning-type)))))

(defn single-player-win
  "Single player win the game."
  [state player-id]
  ;; We have to use bet-sum to calculate the total prize
  ;; because pots are not complete here.
  (-> state
      (collect-bet-to-pots)
      (assign-winner-to-pots [#{player-id}])
      (update-prize-map)
      (take-rake)
      (apply-prize-map)
      (update-chips-change-map)
      (submit-game-result)
      (remove-non-alive-players)
      (dispatch-reset)
      (assoc :status       :game-status/settle
             :winning-type :last-player
             :bet-map      nil)))

(defn change-street
  "Goto to street and reset some states."
  [{:keys [bb player-map], :as state} street]
  (let [new-player-map (->> (for [[player-id player-state] player-map]
                              [player-id
                               (if (= :player-status/acted (:status player-state))
                                 (assoc player-state :status :player-status/wait)
                                 player-state)])
                            (into {}))]
    (-> state
        (collect-bet-to-pots)
        (assoc
         :status           :game-status/key-share
         :after-key-share  :init-street
         :street           street
         :player-map       new-player-map
         :min-raise        bb
         :action-player-id nil
         :street-bet       nil)
        (update-require-key-idents)
        (take-released-keys))))

(defn next-street
  [street]
  (case street
    :street/preflop :street/flop
    :street/flop    :street/turn
    :street/turn    :street/river
    nil))

(defn next-state-case
  "Ask next player for action or goto next stage when no player can act."
  [{:keys [street-bet street bet-map action-player-id player-map status btn], :as state}]
  (let [;; a list of player ids who did not provide their keys
        non-compliant-player-ids (->> (list-missing-key-idents state)
                                      (map first)
                                      (into #{}))

        remain-players           (->> player-map
                                      vals
                                      (filter (comp #{:player-status/allin :player-status/acted
                                                      :player-status/wait :player-status/in-action}
                                                    :status)))

        players-to-act           (->>
                                  (list-players-in-order state
                                                         (get-in player-map
                                                                 [action-player-id :position]
                                                                 btn)
                                                         #{:player-status/acted
                                                           :player-status/wait}))

        ;; next action player is who haven't bet or bet less than street-bet
        next-action-player       (->> players-to-act
                                      (filter #(or (< (get bet-map (:player-id %) 0) street-bet)
                                                   (= :player-status/wait (:status %))))
                                      first)

        new-street               (next-street street)

        allin-players            (->> remain-players
                                      (filter (comp #{:player-status/allin} :status)))]

    (cond
      ;; missing keys, game has to be terminated
      ;; currently, terminate means a draw game for non-fold players
      (seq non-compliant-player-ids)
      [:terminate non-compliant-player-ids]

      ;; no bets yet, blind bets
      (and (= :street/preflop street)
           (nil? bet-map))
      [:blind-bets]

      ;; only one player in game
      (= 1 (count player-map))
      [:single-player-win (:player-id (first (vals player-map)))]

      ;; single player win, all others fold
      (= 1 (count remain-players))
      [:single-player-win (:player-id (first remain-players))]

      ;; next player to act
      next-action-player
      [:ask-player-for-action (:player-id next-action-player)]

      ;; enough players allin to start the runner
      (and
       (not= :game-status/runner status)
       (>= (+ 1 (count allin-players))
           (count remain-players)))
      [:runner]

      ;; all player has the same bets, and acted
      new-street
      [:change-street new-street]

      :else
      [:showdown])))

(defn next-state
  [state]
  (let [[c v :as x] (next-state-case state)]
    (log/infof "💡Next state: %s %s" c v)
    (case c
      :terminate         (terminate state v)
      :blind-bets        (blind-bets state)
      :single-player-win (single-player-win state v)
      :ask-player-for-action (ask-player-for-action state v)
      :change-street     (change-street state v)
      :showdown          (prepare-showdown state)
      :runner            (prepare-runner state)
      (invalid-next-state-case! state))))

(defn merge-joined-players
  [o n]
  (mapv #(or %1 %2) (or o (repeat 9 nil)) n))

(def empty-players (vec (repeat c/max-player-num nil)))

(defn add-joined-player
  "Add joined players found in game-account-state."
  [state]
  (let [{:keys [player-map joined-players]} state]
    (log/infof "🚌Maintain players. Joined-players: %s" joined-players)
    (if (seq joined-players)
      (-> state
          (assoc :player-map
                 (merge (m/players->player-map joined-players)
                        player-map))
          (assoc :joined-players nil))
      state)))

(defn merge-sync-state
  "Merge game account state, add new players."
  [state game-account-state]
  (let [old-buyin-serial  (get-in state [:game-account-state :buyin-serial] 0)
        {:keys [players]} game-account-state
        joined-players    (->> players
                               (mapv (fn [p]
                                       (when (< old-buyin-serial (:buyin-serial p))
                                         p))))]

    (log/infof "😀Joined players: %s" (map :pubkey joined-players))

    (-> state
        (assoc :game-account-state game-account-state)
        (update :joined-players merge-joined-players joined-players))))

(defn reserve-timeout
  [state]
  (assoc state :reserve-timeout true))
