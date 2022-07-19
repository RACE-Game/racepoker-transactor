(ns depk.transactor.game.event-handler
  (:require
   [depk.transactor.game.encrypt :as encrypt]
   [depk.transactor.game.event-handler.misc :as misc]
   [depk.transactor.log :as log]
   [cljs.core.async :refer [go <!]]))

(defmulti handle-event
  (fn [_state event]
    (:type event)))

(defmethod handle-event :default
  [state _event]
  state)

;; implementations

;; system/sync-state
;; receiving this event when game account reflect there's an update from onchain state
(defmethod handle-event :system/sync-state
  [{:keys [status game-id], :as state}
   {{:keys [game-account-state]} :data, :as event}]

  (log/log "📦"
           game-id
           "Receive new game account state, %s -> %s"
           (get-in state [:game-account-state :buyin-serial])
           (:buyin-serial game-account-state))

  (when (<= (:buyin-serial game-account-state)
            (get-in state [:game-account-state :buyin-serial]))
    (misc/state-already-merged! state event))

  (-> state
      (misc/merge-sync-state game-account-state)
      (misc/reserve-timeout)))

;; system/reset
;; receiving this event for reset states
(defmethod handle-event :system/reset
  [{:keys [player-map], :as state}
   {:keys [timestamp], :as event}]
  (-> state
      (misc/remove-eliminated-players)
      (misc/reset-sng-state)
      (misc/submit-non-alive-players)
      (misc/remove-non-alive-players)
      (misc/add-joined-player)
      (misc/reset-player-map-status)
      (misc/increase-blinds timestamp)            ; For SNG & Tournament
      (misc/dispatch-start-game)
      (misc/reset-game-state)))

;; system/start-game
;; Receiving this event to trigger game start.
;; Unless all players are ready, kick non-ready players
;; When there's enough players to start:
;; - set game status to shuffle
;; - generate a default deck of cards
;; - ask the first player (BTN) to shuffle the cards.
(defmethod handle-event :system/start-game
  [{:keys [status player-map game-type size start-time game-account-state halt? game-id], :as state}
   {:keys [timestamp], :as event}]

  (when-not (= :game-status/init status)
    (misc/invalid-game-status! state event))

  (log/log "🕹️"
           game-id
           "Start game, %s, number of players: %s / %s"
           (name game-type)
           (count player-map)
           size)

  (doseq [[id {:keys [online-status chips]}] player-map]
    (log/log (if (= :normal online-status)
               "😀"
               "😴")
             game-id
             "-%s %s"
             id
             chips))

  (cond

    ;; ------------------------------------------------
    ;; Tournament fail case
    ;; ------------------------------------------------
    (and (= :tournament game-type)
         (= (count player-map) 0))
    (do
      (log/log "🛑" game-id "Tournament game closed.(Tournament)")
      state)

    (and (= :tournament game-type)
         halt?)
    (do
      (log/log "🛑" game-id "Can not start, halted.(Tournament)")
      (-> state
          (misc/dispatch-reset)))


    (and (= :tournament game-type)
         (= (count player-map) 1))
    (do
      (log/log "🛑" game-id "Can not start, one player left.(Tournament)")
      ;; A single player waiting for an empty seat
      ;; No-op
      (-> state
          (misc/submit-game-result)
          (misc/dispatch-reset)))

    (and (= :tournament game-type)
         (every? #(not= :normal (:online-status %)) (vals player-map)))
    (do
      (log/log "🛑" game-id "Can not start, no player ready.(Tournament)")
      ;; A single player waiting for an empty seat
      ;; No-op
      (-> state
          (misc/submit-game-result)
          (misc/dispatch-reset)))

    (and (= :tournament game-type)
         (< (count (filter #(= :normal (:online-status %)) (vals player-map)))
            2))
    (let [winner-id (->> player-map
                         vals
                         (filter #(= :normal (:online-status %)))
                         first
                         :player-id)]
      (log/log "🔷" game-id
               "Only Player[%s] is ready, blinds out other players.(Tournament)"
               winner-id)
      (-> state
          (misc/blinds-out winner-id)))

    ;; ------------------------------------------------
    ;; SNG fail case
    ;; ------------------------------------------------
    (and (= :sng game-type)
         (nil? start-time)              ; Nil start-time means the first start
         (or (not (every? #(= :normal (:online-status %)) (vals player-map)))
             (< (count player-map) size)))
    ;; SNG type without full table
    ;; Kick the non-ready players
    (do
      (log/log "🛑" game-id "Can not start, no enough ready players.(SNG)")
      (-> state
          (misc/dispatch-reset)))

    ;; At least one player is ready.
    ;; Otherwise the SNG game can not start.
    (and (= :sng game-type)
         (every? #(not= :normal (:online-status %)) (vals player-map))
         (= :in-progress (:status game-account-state)))
    (do
      (log/log "🛑" game-id "Can not start, no one is ready.(SNG)")
      (-> state
          (misc/dispatch-reset)))

    ;; ------------------------------------------------
    ;; Common fail case
    ;; ------------------------------------------------

    ;; If the number of players is not enough for starting
    ;; Require further alive event from the only client
    (= (count player-map) 1)
    (do
      (log/log "🛑" game-id "Can not start, no enough players")
      (-> state
          (misc/dispatch-reset)))

    ;; No players
    ;; Reset game state, and do nothing
    ;; Waiting a player to join
    (zero? (count player-map))
    (do
      (log/log "🛑" game-id "Can not start, no player")
      (-> state
          (misc/dispatch-reset)))

    ;; ------------------------------------------------
    ;; Cash fail case
    ;; ------------------------------------------------

    ;; In cash game, if any client is not ready, kick it
    (and (= :cash game-type)
         (not (every? #(= :normal (:online-status %)) (vals player-map))))
    (do
      (log/log "🛑" game-id "Can not start, some one is not ready.(Cash)")
      (-> state
          (misc/dispatch-reset)))

    :else
    ;; Enough players and all players are ready
    ;; Can start immediately
    (go
     (let [next-btn   (misc/next-btn state)
           ciphers    (encrypt/cards->card-strs misc/default-deck-of-cards)
           data       (-> (<! (encrypt/encrypt-ciphers-with-default-shuffle-key ciphers))
                          (encrypt/ciphers->hex))
           start-time (if (and (nil? start-time)
                               (#{:sng :bonus} game-type))
                        timestamp
                        start-time)]
       (log/log "🔥" game-id "Start game, BTN position: %s" next-btn)
       (-> state
           (assoc :start-time start-time)
           (assoc :btn next-btn)
           ;; Set a new secret-id
           ;; This will expire old shuffle-key & encrypt-key
           (update :secret-id inc)
           (misc/set-operation-player-ids)
           (misc/with-next-op-player-id-as :shuffle-player-id)
           (assoc :prepare-cards [{:data      data,
                                   :op        :init,
                                   :player-id nil}]
                  :status        :game-status/shuffle)
           (misc/dispatch-shuffle-timeout))))))

;; client/shuffle-cards
;; receiving this event when player submit shuffled cards
;; each player will shuffle cards and encrypt with their shuffle key.
(defmethod handle-event :client/shuffle-cards
  [{:keys [op-player-ids shuffle-player-id btn prepare-cards status], :as state}
   {{:keys [data]} :data, player-id :player-id, :as event}]

  (when-not (= :game-status/shuffle status)
    (misc/invalid-game-status! state event))

  (when (or (not player-id)
            (not (= shuffle-player-id player-id)))
    (misc/invalid-player-id! state event))

  (let [new-shuffle-player-id (misc/next-op-player-id state shuffle-player-id)
        new-prepare-cards     (conj prepare-cards
                                    {:data      data,
                                     :op        :shuffle,
                                     :player-id player-id})]
    (cond
      ;; shuffle finished
      (= (first op-player-ids) new-shuffle-player-id)
      (-> state
          (assoc :prepare-cards new-prepare-cards
                 :shuffle-player-id nil
                 :encrypt-player-id new-shuffle-player-id
                 :status        :game-status/encrypt)
          (misc/dispatch-encrypt-timeout))

      :else
      (-> state
          (assoc :prepare-cards     new-prepare-cards
                 :shuffle-player-id new-shuffle-player-id)
          (misc/dispatch-shuffle-timeout)))))

;; client/encrypt-cards
;; receiving this event when player submit encrypted cards
;; each player will remove its shuffle key, and encrypt each cards with encrypt keys respectly.
(defmethod handle-event :client/encrypt-cards
  [{:keys [op-player-ids encrypt-player-id prepare-cards status], :as state}
   {{:keys [data]} :data, player-id :player-id, :as event}]

  (when-not (= :game-status/encrypt status)
    (misc/invalid-game-status! state event))

  (when (or (not player-id)
            (not (= encrypt-player-id player-id)))
    (misc/invalid-player-id! state event))

  (let [new-encrypt-player-id (misc/next-op-player-id state encrypt-player-id)
        new-prepare-cards     (conj prepare-cards
                                    {:data      data,
                                     :op        :encrypt,
                                     :player-id player-id})]
    (go

     (cond
       ;; encrypt finished
       (= (first op-player-ids) new-encrypt-player-id)
       (let [ciphers  (<! (-> (encrypt/hex->ciphers data)
                              (encrypt/decrypt-ciphers-with-default-shuffle-key)))
             new-data (encrypt/ciphers->hex ciphers)]
         (-> state
             (assoc :prepare-cards     new-prepare-cards
                    :card-ciphers      new-data
                    :encrypt-player-id nil
                    :street            :street/preflop
                    :status            :game-status/key-share
                    :after-key-share   :init-street)
             (misc/update-require-key-idents)
             (misc/dispatch-key-share-timeout)))

       :else
       (-> state
           (assoc :prepare-cards     new-prepare-cards
                  :encrypt-player-id new-encrypt-player-id)
           (misc/dispatch-encrypt-timeout))))))

;; client/share-keys
;; receiving this event when player share its keys.
(defmethod handle-event :client/share-keys
  [{:keys [status after-key-share game-id], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]

  (doseq [[key-ident _] share-keys]
    (when-not (misc/valid-key-ident? state key-ident player-id)
      (misc/invalid-share-key! state event)))

  (when-not (seq share-keys)
    (misc/empty-share-keys! state event))

  (let [new-state (update state :share-key-map merge share-keys)]
    (cond
      (not= :game-status/key-share status)
      (-> new-state
          (misc/reserve-timeout))

      (seq (misc/list-missing-key-idents new-state))
      (do
        (log/log "🔑" game-id "Wait more keys")
        (-> new-state
            (misc/reserve-timeout)))

      (= :settle after-key-share)
      (do
        (log/log "🔑" game-id "Key share complete, settle: SHOWDOWN")
        (-> new-state
            (assoc :street :street/showdown)
            (misc/settle :showdown)))

      (= :init-street after-key-share)
      (do
        (log/log "🔑" game-id "Key share complete, go next street")
        (-> new-state
            (assoc :status :game-status/play)
            (misc/next-state)))

      (= :runner after-key-share)
      (do
        (log/log "🔑" game-id "Key share complete, settle: RUNNER")
        (-> new-state
            (assoc :street :street/showdown)
            (misc/settle :runner)))

      :else
      (throw (ex-info "Invalid after-key-share" {:after-key-share after-key-share})))))

;; system/key-share-timeout
;; Receiving this event when key-share is timeout
;; Players who did not provide their keys, will be marked as dropout.
;; If it's the first time for key sharing, cancel current game
(defmethod handle-event :system/key-share-timeout
  [{:keys [status street after-key-share game-id], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when-not (= :game-status/key-share status)
    (misc/invalid-game-status! state event))

  (let [missing-key-idents (misc/list-missing-key-idents state)]
    (log/log "🔒️" game-id "Key share timeout, missing: %s" missing-key-idents)
    (let [timeout-player-ids (map first missing-key-idents)]
      (-> state
          (misc/mark-dropout-players timeout-player-ids)
          (misc/next-state)))))

;; system/shuffle-timeout
;; Receiving this event when shuffling is timeout
;; Players who did not complete their task, will be marked as dropout.
;; The game will turn back to init state, since it failed to start in case.
(defmethod handle-event :system/shuffle-timeout
  [{:keys [status shuffle-player-id game-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (= :game-status/shuffle status)
    (misc/invalid-game-status! state event))

  (log/log "🔒️" game-id "Player[%s] shuffle timeout" shuffle-player-id)

  (-> state
      (misc/mark-dropout-players [shuffle-player-id])
      (misc/dispatch-reset)))

;; system/encrypt-timeout
;; Receiving this event when encrypting is timeout
;; Players who did not complete their task, will be marked as dropout.
;; The game will turn back to init state, since it failed to start in case.
(defmethod handle-event :system/encrypt-timeout
  [{:keys [status encrypt-player-id game-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (= :game-status/encrypt status)
    (misc/invalid-game-status! state event))

  (log/log "🔒️" game-id "Player[%s] encrypt timeout" encrypt-player-id)

  (-> state
      (misc/mark-dropout-players [encrypt-player-id])
      (misc/dispatch-reset)))

;; system/player-action-timeout
(defmethod handle-event :system/player-action-timeout
  [{:keys [status action-player-id game-id], :as state}
   {{:keys [share-keys]} :data,
    :as event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (log/log "🔒️" game-id "Player[%s] action timeout" action-player-id)

  (-> state
      (assoc-in [:player-map action-player-id :status] :player-status/fold)
      (misc/add-log {:player-id action-player-id,
                     :type      :log/player-action-timeout})
      (misc/next-state)))

;; client/ready
;; A event received when a client is ready to start
(defmethod handle-event :client/ready
  [{:keys [status player-map winner-id rsa-pub-map sig-map game-id], :as state}
   {player-id :player-id,
    {:keys [rsa-pub sig]} :data,
    :as       event}]

  (when-not rsa-pub
    (misc/invalid-rsa-pub! state event))

  (when-not sig
    (misc/invalid-sig! state event))

  (when (and (get rsa-pub-map player-id)
             (or (not= (get rsa-pub-map player-id) rsa-pub)
                 (not= (get sig-map player-id) sig)))
    (misc/cant-update-rsa-pub! state event))

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when-not (#{:game-status/init} status)
    (misc/invalid-game-status! state event))

  (when winner-id (misc/sng-finished! state event))

  (log/log "✅" game-id "Player[%s] send ready" player-id)

  (-> state
      (assoc-in [:player-map player-id :online-status] :normal)
      (assoc-in [:rsa-pub-map player-id] rsa-pub)
      (assoc-in [:sig-map player-id] sig)
      (misc/dispatch-start-game)))

(defmethod handle-event :client/fix-keys
  [{:keys [player-map rsa-pub-map sig-map game-id winner-id], :as state}
   {player-id :player-id,
    {:keys [rsa-pub sig]} :data,
    :as       event}]

  (when-not rsa-pub
    (misc/invalid-rsa-pub! state event))

  (when-not sig
    (misc/invalid-sig! state event))

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when (= :player-status/fold (get-in player-map [player-id :status]))
    (misc/invalid-player-status! state event))

  (let [saved-rsa-pub (get rsa-pub-map player-id)
        saved-sig     (get sig-map player-id)]
    (when (or (and saved-rsa-pub (not= saved-rsa-pub rsa-pub))
              (and saved-sig (not= saved-sig sig)))
      (misc/cant-update-rsa-pub! state event)))

  (when winner-id (misc/sng-finished! state event))

  (log/log "💚" game-id "Player[%s] fix public keys" player-id)

  (-> state
      (assoc-in [:player-map player-id :online-status] :normal)
      (assoc-in [:rsa-pub-map player-id] rsa-pub)
      (assoc-in [:sig-map player-id] sig)
      (misc/update-require-key-idents)
      (misc/reserve-timeout)))

;; system/dropout
;; A event received when a client dropout its connection
;; All client whiout this event will be kicked when game start
(defmethod handle-event :system/dropout
  [{:keys [player-map game-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (log/log "💔️" game-id "Player[%s] drop off" player-id)

  (let [player-map (assoc-in player-map [player-id :online-status] :dropout)]
    (-> state
        (assoc :player-map player-map)
        (misc/reserve-timeout))))

;; system/alive
;; A event received when a client established
;; This command only work during the game.
(defmethod handle-event :system/alive
  [{:keys [status player-map game-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when (#{:game-status/init} status)
    (misc/invalid-game-status! state event))

  (log/log "❤️" game-id "Player[%s] alive by reconnect" player-id)

  (let [player-map (assoc-in player-map [player-id :online-status] :normal)]
    (-> state
        (assoc :player-map player-map)
        (misc/reserve-timeout))))

;; client/leave
;; A event received when a player leave game
;; 1. Current game is running
;; The player must share all the necessary keys for current game
;; Otherwise this event will not success.
;; 2. Game is not running
;; Send a claim transaction for player
(defmethod handle-event :client/leave
  [{:keys [status action-player-id game-type start-time game-id player-map], :as state}
   {{:keys [released-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (let [new-state      (-> state
                           (update-in [:player-map player-id]
                                      assoc
                                      :online-status :leave
                                      :status        :player-status/fold))

        remain-players
        (->> (:player-map new-state)
             vals
             (filter (comp #{:player-status/allin :player-status/acted
                             :player-status/wait :player-status/in-action}
                           :status)))]

    (cond
      (and (#{:bonus :sng} game-type)
           (some? start-time))
      (misc/cant-leave-game! state event)

      ;; Game is not running, can leave immetdiately
      (#{:game-status/init :game-status/settle :game-status/showdown} status)
      (do (log/log "⏪️" game-id "Player[%s] leave" player-id)
          (-> new-state
              (misc/dispatch-reset)))

      ;; The last player will win immediately
      (= 1 (count remain-players))
      (do
        (log/log "⏪️"
                 game-id
                 "Player[%s] leave, the left Player[%s] win"
                 player-id
                 (first remain-players))
        (misc/single-player-win new-state (:player-id (first remain-players))))

      ;; Can't leave during key-share
      (= status :game-status/key-share)
      (misc/cant-leave-game! state event)

      ;; Game is running, calculate next state
      :else
      (do
        (log/log "⏪️" game-id "Player[%s] leave, game continue" player-id)
        (cond-> (-> new-state
                    (update :released-keys-map assoc player-id released-keys))

          (= player-id action-player-id)
          (misc/next-state)

          (not (= player-id action-player-id))
          (misc/reserve-timeout))))))

(defmethod handle-event :player/fold
  [{:keys [status action-player-id state-id player-map], :as state}
   ;; use released keys
   {{:keys [released-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/fold)
      (misc/add-log {:player-id player-id,
                     :type      :log/player-fold})
      (misc/next-state)))

(defmethod handle-event :player/call
  [{:keys [bet-map player-map status action-player-id street-bet state-id], :as state}
   {player-id :player-id, :as event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (let [bet            (get bet-map player-id (js/BigInt 0))
        player         (get player-map player-id)
        amount-to-call (- street-bet bet)
        [bet player allin?] (misc/take-bet-from-player player amount-to-call)
        status         (if allin? :player-status/allin :player-status/acted)]
    (-> state
        (assoc-in [:player-map player-id] player)
        (update-in [:bet-map player-id] (fnil + (js/BigInt 0)) bet)
        (update-in [:total-bet-map player-id] (fnil + (js/BigInt 0) (js/BigInt 0)) bet)
        (assoc-in [:player-map player-id :status] status)
        (misc/add-log {:player-id player-id,
                       :type      :log/player-call,
                       :amount    bet})
        (misc/next-state))))

(defmethod handle-event :player/check
  [{:keys [bet-map status action-player-id street-bet state-id player-map], :as state}
   {player-id :player-id, :as event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (when-not (= street-bet (get bet-map player-id))
    (misc/player-cant-check! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/acted)
      (misc/add-log {:player-id player-id,
                     :type      :log/player-check})
      (misc/next-state)))

(defmethod handle-event :player/bet
  [{:keys [bet-map player-map bb status min-raise action-player-id street-bet state-id], :as state}
   {{:keys [amount]} :data,
    player-id        :player-id,
    :as              event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (when (get bet-map player-id)
    (misc/player-cant-bet! state event))

  (when-not (or
             (nil? street-bet)
             (= (js/BigInt 0) street-bet))
    (misc/player-cant-bet! state event))

  (when-not (and
             (= js/BigInt (type amount))
             (> amount (js/BigInt 0)))
    (misc/invalid-amount! state event))

  (when-not (or (>= amount bb)
                (= amount (get-in player-map [player-id :chips])))
    (misc/player-bet-too-small! state event))

  (let [player (get player-map player-id)
        [bet player allin?] (misc/take-bet-from-player player amount)]
    (-> state
        (assoc-in [:player-map player-id] player)
        (assoc-in [:bet-map player-id] bet)
        (update-in [:total-bet-map player-id] (fnil + (js/BigInt 0) (js/BigInt 0)) bet)
        (assoc-in [:player-map player-id :status]
                  (if allin? :player-status/allin :player-status/acted))
        (assoc :min-raise  bet
               :street-bet bet)
        (misc/add-log {:player-id player-id,
                       :type      :log/player-bet,
                       :amount    amount})
        (assoc :overwrite-this-event (if allin? :player/allin :player/bet))
        (misc/next-state))))

(defmethod handle-event :player/raise
  [{:keys [bet-map player-map min-raise status action-player-id street-bet state-id], :as state}
   {{:keys [amount]} :data,
    player-id        :player-id,
    :as              event}]

  (when (or (not player-id)
            (not (get player-map player-id)))
    (misc/invalid-player-id! state event))

  (let [curr-bet   (get bet-map player-id (js/BigInt 0))
        add-amount (- amount curr-bet)]

    (when-not (= :game-status/play status)
      (misc/invalid-game-status! state event))

    (when-not (= action-player-id player-id)
      (misc/player-not-in-action! state event))

    (when (= (js/BigInt 0) street-bet)
      (misc/player-cant-raise! state event))

    (when-not (and (= js/BigInt (type add-amount))
                   (> add-amount (js/BigInt 0)))
      (misc/invalid-amount! state event))

    (when-not (or (>= (+ curr-bet add-amount) (+ street-bet min-raise))
                  (= add-amount (get-in player-map [player-id :chips])))
      (misc/player-raise-too-small! state event))

    (let [player         (get player-map player-id)
          [bet player allin?] (misc/take-bet-from-player player add-amount)
          new-street-bet (+ curr-bet bet)
          new-min-raise  (- new-street-bet street-bet)]
      (-> state
          (assoc-in [:player-map player-id] player)
          (update-in [:bet-map player-id] (fnil + (js/BigInt 0)) bet)
          (update-in [:total-bet-map player-id] (fnil + (js/BigInt 0) (js/BigInt 0)) bet)
          (assoc-in [:player-map player-id :status]
                    (if allin? :player-status/allin :player-status/acted))
          (assoc :min-raise  new-min-raise
                 :street-bet new-street-bet)
          (misc/add-log {:player-id player-id,
                         :type      :log/player-raise,
                         :amount    amount})
          (assoc :overwrite-this-event (if allin? :player/allin :player/raise))
          (misc/next-state)))))

;;; Tournament specific

(defmethod handle-event :system/start-tournament-game
  [state event]
  (-> state
      (assoc :halt? false)
      (misc/reserve-timeout)))

;; :system/next-game & :system/resit-table
;; are the replacements for reset in TOURNAMENT

(defmethod handle-event :system/next-game
  [{:keys [status], :as state}
   {{:keys [game-account-state finish?]} :data,
    timestamp :timestamp,
    :as       event}]
  (-> state
      (assoc :halt? false)
      (assoc :status :game-status/init)
      (misc/merge-sync-state game-account-state)
      (misc/remove-eliminated-players)
      (misc/submit-non-alive-players)
      (misc/remove-non-alive-players)
      (misc/add-joined-player)
      (misc/reset-player-map-status)
      (misc/increase-blinds timestamp)            ; For SNG & Tournament
      (misc/dispatch-start-game)
      (misc/reset-game-state)
      (cond->
        finish?
        (assoc :player-map  {}
               :rsa-pub-map {}
               :sig-map     {}
               :halt?       true))))

(defmethod handle-event :system/resit-table
  [{:keys [game-id], :as state}
   {{:keys [resit-map finish?]} :data,
    timestamp :timestamp,
    :as       event}]
  (log/log "🪑" game-id "Receive re-sit notification: %s" (prn-str resit-map))
  (-> state
      (assoc :resit-map resit-map)
      (misc/remove-eliminated-players)
      (misc/submit-non-alive-players)
      (misc/remove-non-alive-players)
      (misc/add-joined-player)
      (misc/reset-player-map-status)
      (misc/increase-blinds timestamp)            ; For SNG & Tournament
      (misc/reset-game-state)
      (misc/remove-players (keys resit-map))
      (misc/dispatch-start-game)
      (cond->
        finish?
        (assoc :halt? true))))
