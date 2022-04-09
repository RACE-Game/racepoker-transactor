(ns depk.transactor.game.event-handler.misc
  (:require
   [depk.transactor.game.models :as m]
   [depk.transactor.game.encrypt :as e]
   [depk.transactor.game.evaluator :as evaluator]
   [depk.transactor.util :as    u
                         :refer [go-try <!?]]
   [depk.transactor.constant :as c]
   [cljs.core.async :as a]
   [clojure.set :as set]))

;; errors

(defn player-already-alive!
  [state event]
  (throw (ex-info "Player already alive"
                  {:state state,
                   :event event})))

(defn invalid-game-status!
  [state event]
  (throw (ex-info "Invalid game status"
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

;; helpers

(def suits #{:d :s :h :c})

(def kinds #{:a :2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k})

(defn kick-dropout-players
  "Remove all players who not send alive events"
  [{:keys [player-map], :as state}]
  (let [dropout-players (->> player-map
                             vals
                             (filter #(not= :normal (:online-status %))))
        request         {:api-request/type  :settle-failed-game,
                         :player-status-map (->> (for [[id p] player-map]
                                                   [id
                                                    (if (= :normal (:online-status p))
                                                      :normal
                                                      :leave)
                                                    (:online-status p :leave)])
                                                 (into {}))}]
    ;; send settlement for leaving players
    (-> state
        (update :player-map (fn [m] (apply dissoc m (map :player-id dropout-players))))
        (update :api-requests conj request))))

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
  ([state last-pos status-filter]
   (let [{:keys [player-map]} state]
     (->> player-map
          vals
          (filter (comp status-filter :status))
          (sort-by (fn [{:keys [position]}]
                     (if (> position last-pos)
                       (- position last-pos)
                       (+ 10000 position))))))))

(defn reset-game-state
  "Reset game state based on `event-type`."
  [state]
  (assoc
   state
   :status             :game-status/init
   :released-keys-map  nil
   :street             nil
   :card-ciphers       []
   :after-keyshare     nil
   :require-key-idents nil
   :share-key-map      nil
   :community-cards    nil
   :min-raise          nil
   :street-bet         nil
   :bet-map            nil
   ;; these two keys will be reset by event loop
   ;; :dispatch-event     nil
   ;; :api-requests       nil
   :pots               []
   :showdown-map       nil
   :prize-map          nil
   :player-actions     []
   :winning-type       nil
   :after-key-share    nil))

(defn get-player-hole-card-indices
  [{:keys [btn player-map], :as state}]
  (let [pid-to-idx (->> (list-players-in-order state btn)
                        (map-indexed (fn [idx p] [(:player-id p) idx]))
                        (into {}))
        idxs       (partition 2 (range))]
    (->> (for [[pid] player-map]
           [pid (nth idxs (get pid-to-idx pid))])
         (into {}))))

(defn- dispatch-reset
  "Dispatch reset event."
  [state]
  (assoc state
         :dispatch-event
         [c/reset-timeout-delay
          (m/make-event :system/reset state {})]))

(defn dispatch-start-game
  [state & [start-delay]]
  (let [{:keys [next-start-ts]} state
        btn (get state :btn 0)
        [next-btn _] (next-position-player state btn)]
    (-> state
        (assoc :dispatch-event
               [(or start-delay c/default-start-game-delay)
                (m/make-event :system/start-game state {:btn next-btn})]))))

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
  [state]
  (assoc state
         :dispatch-event
         [c/player-action-timeout-delay
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
  [{:keys [require-key-idents share-key-map player-map released-keys-map],
    :as   state}]
  (let [released-player-ids    (->> player-map
                                    (vals)
                                    (filter #(not= :normal (:status %)))
                                    (map :player-id))

        released-share-key-map (->> require-key-idents
                                    (filter (complement (or share-key-map {})))
                                    (keep
                                     (fn [[pid _type idx :as idt]]
                                       (when-let [k (get-in released-keys-map [pid idx])]
                                         [idt k])))
                                    (into {}))]
    (-> state
        (update :share-key-map merge released-share-key-map))))

(defn- list-require-hole-cards-key-idents
  [player-ids card-idx-2d]
  (let [player-id-set (set player-ids)]
    (->> (mapcat (fn [player-id card-idxs]
                   (for [other-player-id (disj player-id-set player-id)
                         card-idx        card-idxs]
                     [other-player-id :hole-card card-idx player-id]))
          player-ids
          card-idx-2d)
         (into #{}))))

(defn- list-require-community-cards-key-idents
  [player-ids card-idxs]
  (->> (for [player-id player-ids
             card-idx  card-idxs]
         [player-id :community-card card-idx])
       (into #{})))

(defn- list-require-showdown-key-idents
  "Return key indents for all hole-cards of those players who have not fold their cards."
  [{:keys [player-map], :as state}]
  (let [hole-card-indices (get-player-hole-card-indices state)
        player-ids        (keys player-map)]
    (->> player-map
         vals
         (filter (comp #{:player-status/acted :player-status/allin} :status))
         (mapcat (fn [{:keys [player-id]}]
                   (for [idx (get hole-card-indices player-id)
                         pid player-ids]
                     [pid :showdown-card idx])))
         (into #{}))))

(defn list-require-key-idents
  [{:keys [btn street after-key-share], :as state}]
  ;; {:pre [(some? street) (int? btn)]}
  (let [player-ids (->> (list-players-in-order state btn)
                        (map :player-id))]
    (cond
      ;; runner will require all keys
      (= :runner after-key-share)
      (let [skip (* 2 (count player-ids))]
        (set/union
         (list-require-showdown-key-idents state)
         (list-require-community-cards-key-idents player-ids (range skip (+ skip 5)))))

      (= :street/preflop street)
      (list-require-hole-cards-key-idents player-ids (partition 2 (range)))

      (= :street/flop street)
      (let [skip (* 2 (count player-ids))]
        (list-require-community-cards-key-idents player-ids (range skip (+ skip 3))))

      (= :street/turn street)
      (let [idx (+ (* 2 (count player-ids)) 3)]
        (list-require-community-cards-key-idents player-ids [idx]))

      (= :street/river street)
      (let [idx (+ (* 2 (count player-ids)) 4)]
        (list-require-community-cards-key-idents player-ids [idx]))

      (= :street/showdown street)
      (list-require-showdown-key-idents state))))

(defn update-require-key-idents
  "Update require-key-idents in state."
  [state]
  (let [require-key-idents (list-require-key-idents state)
        into-set (fnil into #{})]
    (update state :require-key-idents into-set require-key-idents)))

(defn update-prize-map
  "Update prize-map in state.

  Depends on pots."
  [{:keys [pots], :as state}]
  (let [prize-map (->> pots
                       (mapcat (fn [{:keys [amount winner-ids]}]
                                 (let [prize (/ amount (count winner-ids))]
                                   (for [id winner-ids]
                                     {id prize}))))
                       (apply merge-with +))]
    (assoc state :prize-map prize-map)))

(defn update-chips-change-map
  "Update chips-change-map in state.

  Depends on player-map and pots."
  [{:keys [pots player-map bet-map], :as state}]
  (let [chips-change-map (reduce
                          (fn [acc pot]
                            (let [{:keys [owner-ids winner-ids amount]} pot
                                  bet-amount   (/ amount (count owner-ids))
                                  prize-amount (/ amount (count winner-ids))]
                              (as-> acc $
                                (reduce (fn [acc owner-id]
                                          (update acc owner-id - bet-amount))
                                        $
                                        owner-ids)
                                (reduce (fn [acc winner-id]
                                          (update acc winner-id + prize-amount))
                                        $
                                        winner-ids))))
                          (->> (for [[pid] player-map]
                                 [pid 0])
                               (into {}))
                          pots)
        chips-change-map (merge-with (fnil - 0)
                                     chips-change-map
                                     bet-map)]
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
  (let [players          (list-players-in-order state btn)
        [sb-player bb-player] (if (= 2 (count players))
                                ;; two player, BTN is SB
                                (reverse players)
                                players)

        [bb-bet new-bb-player] (take-bet-from-player bb-player bb)
        [sb-bet new-sb-player] (take-bet-from-player sb-player sb)

        sb-player-id     (:player-id new-sb-player)
        bb-player-id     (:player-id new-bb-player)

        bet-map          {sb-player-id sb-bet,
                          bb-player-id bb-bet}

        action-player-id (if (= 2 (count players))
                           sb-player-id
                           (:player-id (nth players 2)))

        new-player-map   (assoc player-map
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
  (let [new-pots (->> pots
                      (mapv (fn [pot]
                              (loop [[id-set & rest-id-sets] winner-id-sets]
                                (let [winner-ids (set/intersection (:owner-ids pot) id-set)]
                                  (if (seq winner-ids)
                                    (assoc pot :winner-ids winner-ids)
                                    (recur rest-id-sets)))))))]
    (assoc state :pots new-pots)))

(defn collect-bet-to-pots
  "Update pots, reset bets.

  When not all players have the same bet, multiple pots will be created."
  [{:keys [player-map bet-map], :as state}]
  (let [steps    (->> bet-map
                      vals
                      distinct
                      sort)
        pots
        (loop [ret     []
               counted 0
               [step & rest-steps] steps]
          (if-not step
            ret
            (let [owner-ids (->> bet-map
                                 (filter (fn [[player-id bet]]
                                           (and (>= bet step)
                                                (not= :player-status/fold
                                                      (get-in player-map [player-id :status])))))
                                 (map key)
                                 (into #{}))

                  total     (->> bet-map
                                 (map (fn [[_ bet]]
                                        (min bet step)))
                                 (reduce + 0))
                  amount    (- total counted)
                  pot       (m/make-pot owner-ids amount)]
              (recur (if (= (count owner-ids) (count (:owner-ids (peek ret))))
                       (conj (pop ret) (update pot :amount + (:amount (peek ret))))
                       (conj ret pot))
                     total
                     rest-steps))))
        into-vec (fnil into [])]
    (-> state
        (update :pots into-vec pots))))

(defn prepare-showdown
  "Player showdown hole cards."
  [{:keys [], :as state}]
  (-> state
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

(defn- decrypt-card
  "Decrypt card with public share key."
  [idx share-key-map])

(defn- update-vals
  [f m]
  (->> (for [[k v] m]
         [k (f v)])
       (into {})))

(defn- decrypt-community-cards
  [{:keys [player-map share-key-map card-ciphers], :as state}]
  (go-try
   (let [offset    (* 2 (count player-map))
         idxs      (->> offset
                        (iterate inc)
                        (take 5)
                        vec)
         ciphers   (e/hex->ciphers card-ciphers)
         ciphers   (mapv ciphers idxs)
         aes-keys  (<!?
                    (->> player-map
                         keys
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
  [{:keys [card-ciphers share-key-map player-map], :as state}]
  ;; (.debug js/console card-ciphers)
  ;; (.debug js/console share-key-map)
  ;; (.debug js/console player-map)
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

         aes-keys          (<!? (->> player-map
                                     (keys)
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
          (update-vals #(mapv last %))))))

(defn- submit-game-result
  "Add request to :api-requests, submit game result."
  [{:keys [chips-change-map player-map], :as state}]
  (let [request {:api-request/type  :settle-finished-game,
                 :chips-change-map  chips-change-map,
                 :player-status-map (->> (for [[id p] player-map]
                                           [id (:online-status p :normal)])
                                         (into {}))}]
    (update state :api-requests conj request)))

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
        player-map        (->> player-map
                               (map (fn [[pid p]]
                                      [pid (update p :chips + (get bet-map pid 0))]))
                               (into {}))]
    (-> state
        (assoc :bet-map nil)
        (assoc :player-map player-map)
        (assign-winner-to-pots winner-player-ids)
        (update-prize-map)
        (update-chips-change-map)
        (submit-game-result)
        (dispatch-reset)
        ;; TODO, new status/type ?
        (assoc :status       :game-status/settle
               :winning-type :last-player))))

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
                                (sort-by first >)
                                (mapv #(set (mapv first (second %)))))]

     (-> state
         (collect-bet-to-pots)
         (assoc :showdown-map showdown
                :bet-map      nil)
         (assign-winner-to-pots winner-id-sets)
         (update-prize-map)
         (update-chips-change-map)
         (submit-game-result)
         (dispatch-reset)
         (assoc :status       :game-status/showdown
                :winning-type winning-type)))))

(defn single-player-win
  "Single player win the game."
  [{:keys [pots bet-map], :as state} player-id]
  (let [bet-sum   (reduce + 0 (map val bet-map))
        prize-map {player-id (+ (transduce (map :amount) + 0 pots)
                                bet-sum)}]
    (-> state
        (assign-winner-to-pots [#{player-id}])
        (assoc :prize-map prize-map)
        (apply-prize-map)
        (update-chips-change-map)
        ;; Append a pot collected from current street
        (cond-> (pos? bet-sum)
                (update :pots conj (m/make-pot (set (keys bet-map)) bet-sum #{player-id})))
        (update-in [:chips-change-map player-id] (fnil + 0) bet-sum)
        (submit-game-result)
        (dispatch-reset)
        (assoc :status       :game-status/settle
               :winning-type :last-player
               :bet-map      nil))))

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
    (case c
      :terminate         (terminate state v)
      :blind-bets        (blind-bets state)
      :single-player-win (single-player-win state v)
      :ask-player-for-action (ask-player-for-action state v)
      :change-street     (change-street state v)
      :showdown          (prepare-showdown state)
      :runner            (prepare-runner state))))

(defn get-blind-bet
  "Get blind bet amount as [sb, bb]."
  [game-account-state mint-info]
  (let [{:keys [level]} game-account-state
        {:keys [decimals]} mint-info
        {:keys [sb bb]} (get c/level-info-map level)
        base (js/BigInt (js/Math.pow 10 decimals))]
    [(* base sb)
     (* base bb)]))

(defn merge-sync-state
  "Merge players data(from blockchain) into player-map."
  [state players game-account-state]
  (let [player-map (->> players
                        (keep-indexed
                         (fn [idx p]
                           (when p
                             (let [player-id (str (:pubkey p))]
                               (m/make-player-state player-id
                                                    (:chips p)
                                                    idx
                                                    :player-status/wait
                                                    :dropout)))))
                        (map (juxt :player-id identity))
                        (into {}))
        [sb bb]    (get-blind-bet game-account-state (:mint-info state))]
    (assoc state
           :player-map player-map
           :sb         sb
           :bb         bb
           :game-no    (:game-no game-account-state)
           :game-account-state game-account-state)))

(defn reserve-dispatch
  [state]
  (assoc state :reserve-dispatch-id true))
