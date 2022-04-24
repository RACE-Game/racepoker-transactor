(ns depk.transactor.game.event-handler
  (:require
   [depk.transactor.game.models :as m]
   [depk.transactor.game.encrypt :as encrypt]
   [depk.transactor.game.event-handler.misc :as misc]
   [depk.transactor.constant :as c]
   [depk.transactor.log :as log]
   [depk.transactor.event.protocol :as ep]
   [cljs.core.async :refer [go <!]]
   ["uuid" :as uuid]))


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
  [{:keys [status game-no], :as state}
   {{:keys [game-account-state]} :data, :as event}]
  (when (<= (:game-no game-account-state) game-no)
    (misc/state-already-merged! state event))

  (log/infof "✨New game account state, game-no: %s -> %s"
             game-no
             (:game-no game-account-state))

  (-> state
      (misc/merge-sync-state game-account-state)
      (misc/reserve-dispatch)))

;; system/force-sync-state
;; receiving this event when something goes wrong.
(defmethod handle-event :system/recover-state
  [{:keys [mint-info game-id game-no]}
   {{:keys [game-account-state]} :data, :as event}]

  (log/infof "🏥Recover game account state, game-no: %s -> %s, players: %s"
             game-no
             (:game-no game-account-state)
             (:players game-account-state))

  (-> (m/make-game-state game-account-state mint-info {:game-id game-id})
      (misc/dispatch-reset)))

;; system/reset
;; receiving this event for reset states
(defmethod handle-event :system/reset
  [{:keys [player-map], :as state} event]
  ;; (log/infof "Reset, player-map: %s" player-map)
  (-> state
      (misc/remove-eliminated-players)
      (misc/reset-sng-state)
      (misc/submit-non-alive-players)
      (misc/remove-non-alive-players)
      (misc/add-joined-player)
      (misc/reset-player-map-status)
      (misc/increase-blinds)            ; For SNG only
      (misc/reset-game-state)
      (misc/dispatch-start-game)))

;; system/start-game
;; Receiving this event to trigger game start.
;; Unless all players are ready, kick non-ready players
;; When there's enough players to start:
;; - set game status to shuffle
;; - generate a default deck of cards
;; - ask the first player (BTN) to shuffle the cards.
(defmethod handle-event :system/start-game
  [{:keys [status player-map game-type size start-time game-account-state], :as state}
   event]

  (when-not (= :game-status/init status)
    (misc/invalid-game-status! state event))

  (log/infof "🎰Start game[%s / %s], number of players: %s"
             (name game-type)
             size
             (count player-map))
  (doseq [[id p] player-map]
    (log/infof "🎰-%s %s" id (:online-status p)))

  (cond

    ;; ------------------------------------------------
    ;; Common fail case
    ;; ------------------------------------------------

    ;; If the number of players is not enough for starting
    ;; Require further alive event from the only client
    (= (count player-map) 1)
    (do
      (log/infof "🛑No enough players to start")
      (-> state
          (misc/dispatch-reset)))

    ;; No players
    ;; Reset game state, and do nothing
    ;; Waiting a player to join
    (zero? (count player-map))
    (do
      (log/infof "🛑No players to start")
      (-> state
          (misc/dispatch-reset)))

    ;; ------------------------------------------------
    ;; SNG fail case
    ;; ------------------------------------------------

    ;; SNG type without full table
    ;; Kick the non-ready players
    (and (#{:bonus :sng :tournament} game-type)
         (nil? start-time)              ; Nil start-time means the first start
         (or (not (every? #(= :normal (:online-status %)) (vals player-map)))
             (< (count player-map) size)))
    (do
      (log/infof "🚧No enough ready players for SNG/Bonus game.")
      (-> state
          (misc/dispatch-reset)))

    ;; At least one player is ready.
    ;; Otherwise the SNG game can not start.
    (and (#{:sng :bonus} game-type)
         (every? #(not= :normal (:online-status %)) (vals player-map))
         (= :in-progress (:status game-account-state)))
    (do
      (log/infof "🛑All players are not ready (SNG)")
      (-> state
          (misc/dispatch-reset)))

    ;; ------------------------------------------------
    ;; Cash fail case
    ;; ------------------------------------------------

    ;; In cash game, if any client is not ready, kick it
    (and (= :cash game-type)
         (not (every? #(= :normal (:online-status %)) (vals player-map))))
    (do
      (log/infof "🛑Not all players are ready")
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
                        (.getTime (js/Date.))
                        start-time)]
       (-> state
           (assoc :start-time start-time)
           (assoc :btn next-btn)
           (misc/set-operation-player-ids)
           (misc/with-next-op-player-id-as :shuffle-player-id)
           (assoc :prepare-cards [{:data      data,
                                   :op        :init,
                                   :player-id nil}]
                  :status        :game-status/shuffle)
           (misc/dispatch-shuffle-timeout)
           (doto prn-str))))))

;; client/shuffle-cards
;; receiving this event when player submit shuffled cards
;; each player will shuffle cards and encrypt with their shuffle key.
(defmethod handle-event :client/shuffle-cards
  [{:keys [op-player-ids shuffle-player-id btn prepare-cards status], :as state}
   {{:keys [data]} :data, player-id :player-id, :as event}]

  (when-not (= :game-status/shuffle status)
    (misc/invalid-game-status! state event))

  (when-not (= shuffle-player-id player-id)
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

  (when-not (= encrypt-player-id player-id)
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
  [{:keys [status after-key-share], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when-not (= :game-status/key-share status)
    (misc/invalid-game-status! state event))

  (doseq [[key-ident _] share-keys]
    (when-not (misc/valid-key-ident? state key-ident player-id)
      (misc/invalid-share-key! state event)))

  (when-not (seq share-keys)
    (misc/empty-share-keys! state event))

  (let [new-state (update state :share-key-map merge share-keys)]
    (cond
      (seq (misc/list-missing-key-idents new-state))
      (do
        (log/infof "🔑Wait more keys")
        (-> new-state
            (misc/dispatch-key-share-timeout)))

      (= :settle after-key-share)
      (do
        (log/infof "🔑Settle showdown")
        (misc/settle new-state :showdown))

      (= :init-street after-key-share)
      (do
        (log/infof "🔑Next street")
        (-> new-state
            (assoc :status  :game-status/play)
            (misc/next-state)))

      (= :runner after-key-share)
      (do
        (log/infof "🔑Settle runner")
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
  [{:keys [status street after-key-share], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when-not (= :game-status/key-share status)
    (misc/invalid-game-status! state event))

  (let [missing-key-idents (misc/list-missing-key-idents state)]
    (log/debugf "🔒️Missing key idents: %s" missing-key-idents)
    (let [timeout-player-ids (map first missing-key-idents)]
      (-> state
          (misc/mark-dropout-players timeout-player-ids)
          (misc/next-state)))))

;; system/shuffle-timeout
;; Receiving this event when shuffling is timeout
;; Players who did not complete their task, will be marked as dropout.
;; The game will turn back to init state, since it failed to start in case.
(defmethod handle-event :system/shuffle-timeout
  [{:keys [status shuffle-player-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (= :game-status/shuffle status)
    (misc/invalid-game-status! state event))

  (-> state
      (misc/mark-dropout-players [shuffle-player-id])
      (misc/dispatch-reset)))

;; system/encrypt-timeout
;; Receiving this event when encrypting is timeout
;; Players who did not complete their task, will be marked as dropout.
;; The game will turn back to init state, since it failed to start in case.
(defmethod handle-event :system/encrypt-timeout
  [{:keys [status encrypt-player-id], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (= :game-status/encrypt status)
    (misc/invalid-game-status! state event))

  (-> state
      (misc/mark-dropout-players [encrypt-player-id])
      (misc/dispatch-reset)))

;; system/player-action-timeout
(defmethod handle-event :system/player-action-timeout
  [{:keys [status action-player-id], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (-> state
      (assoc-in [:player-map action-player-id :status] :player-status/fold)
      (misc/next-state)))

;; client/ready
;; A event received when a client is ready to start
(defmethod handle-event :client/ready
  [{:keys [status player-map winner-id size], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (get player-map player-id)
    (misc/invalid-player-id! state event))

  (when-not (#{:game-status/init} status)
    (misc/invalid-game-status! state event))

  (when winner-id (misc/sng-finished! state event))

  (log/infof "✅Player ready: %s" player-id)

  (-> state
      (assoc-in [:player-map player-id :online-status] :normal)
      (misc/dispatch-start-game)))

;; system/dropout
;; A event received when a client dropout its connection
;; All client whiout this event will be kicked when game start
(defmethod handle-event :system/dropout
  [{:keys [status player-map game-type size], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (get player-map player-id)
    (misc/invalid-player-id! state event))

  (log/infof "💔️Player dropout: %s" player-id)

  (let [player-map (assoc-in player-map [player-id :online-status] :dropout)]
    (-> state
        (assoc :player-map player-map)
        (misc/reserve-dispatch))))

;; system/alive
;; A event received when a client established
;; This command only work during the game.
(defmethod handle-event :system/alive
  [{:keys [status player-map game-type size], :as state}
   {player-id :player-id,
    :as       event}]

  (when-not (get player-map player-id)
    (misc/invalid-player-id! state event))

  (when (#{:game-status/init} status)
    (misc/invalid-game-status! state event))

  (log/infof "❤️Player alive: %s" player-id)

  (let [player-map (assoc-in player-map [player-id :online-status] :normal)]
    (-> state
        (assoc :player-map player-map)
        (misc/reserve-dispatch))))

;; client/leave
;; A event received when a player leave game
;; 1. Current game is running
;; The player must share all the necessary keys for current game
;; Otherwise this event will not success.
;; 2. Game is not running
;; Send a claim transaction for player
(defmethod handle-event :client/leave
  [{:keys [status action-player-id game-type start-time state-id], :as state}
   {{:keys [released-keys]} :data,
    player-id :player-id,
    :as       event}]
  (let [new-state      (-> state
                           (update-in [:player-map player-id]
                                      assoc
                                      :online-status :leave
                                      :status        :player-status/fold)
                           (update :player-actions
                                   conj
                                   {:action :fold, :player-id player-id, :state-id state-id}))

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
      (do
        (log/infof "⏪️Player leave: %s" player-id)
        (-> new-state
            (misc/dispatch-reset)))

      ;; The last player will win immediately
      (= 1 (count remain-players))
      (do
        (log/infof "⏪️Player leave: %s. The left player win: %s"
                   player-id
                   (:player-id (first remain-players)))
        (misc/single-player-win new-state (:player-id (first remain-players))))

      ;; Game is running, calculate next state
      :else
      (do
        (log/infof "⏪️player leave: %s. Game continue." player-id)
        (cond-> (-> new-state
                    (update :released-keys-map assoc player-id released-keys))

          (= player-id action-player-id)
          (misc/next-state)

          (not (= player-id action-player-id))
          (misc/reserve-dispatch))))))

(defmethod handle-event :player/fold
  [{:keys [status action-player-id state-id], :as state}
   ;; use released keys
   {{:keys [released-keys]} :data,
    player-id :player-id,
    :as       event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/fold)
      (update :player-actions conj {:action :fold, :player-id player-id, :state-id state-id})
      (misc/next-state)))

(defmethod handle-event :player/call
  [{:keys [bet-map player-map status action-player-id street-bet state-id], :as state}
   {player-id :player-id, :as event}]

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
        (update :player-actions
                conj
                {:action :call, :amount bet, :player-id player-id, :state-id state-id})
        (misc/next-state))))

(defmethod handle-event :player/check
  [{:keys [bet-map status action-player-id street-bet state-id], :as state}
   {player-id :player-id, :as event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (when-not (= street-bet (get bet-map player-id))
    (misc/player-cant-check! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/acted)
      (update :player-actions conj {:action :check, :player-id player-id, :state-id state-id})
      (misc/next-state)))

(defmethod handle-event :player/bet
  [{:keys [bet-map player-map bb status min-raise action-player-id street-bet state-id], :as state}
   {{:keys [amount]} :data,
    player-id        :player-id,
    :as              event}]

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
        (update :player-actions
                conj
                {:action :bet, :amount amount, :player-id player-id, :state-id state-id})
        (assoc :overwrite-this-event (if allin? :player/allin :player/bet))
        (misc/next-state))))

(defmethod handle-event :player/raise
  [{:keys [bet-map player-map min-raise status action-player-id street-bet state-id], :as state}
   {{:keys [amount]} :data,
    player-id        :player-id,
    :as              event}]

  (let [curr-bet (get bet-map player-id (js/BigInt 0))]

    (when-not (= :game-status/play status)
      (misc/invalid-game-status! state event))

    (when-not (= action-player-id player-id)
      (misc/player-not-in-action! state event))

    (when (= (js/BigInt 0) street-bet)
      (misc/player-cant-raise! state event))

    (when-not (and (= js/BigInt (type amount))
                   (> amount (js/BigInt 0)))
      (misc/invalid-amount! state event))

    (when-not (or (>= (+ curr-bet amount) (+ street-bet min-raise))
                  (= amount (get-in player-map [player-id :chips])))
      (misc/player-raise-too-small! state event))

    (let [player         (get player-map player-id)
          [bet player allin?] (misc/take-bet-from-player player amount)
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
          (update :player-actions
                  conj
                  {:action :raise, :amount amount, :player-id player-id, :state-id state-id})
          (assoc :overwrite-this-event (if allin? :player/allin :player/raise))
          (misc/next-state)))))
