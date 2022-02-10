(ns depk.transactor.game.event-handler
  (:require
   [depk.transactor.game.models :as m]
   [depk.transactor.game.encrypt :as encrypt]
   [depk.transactor.game.event-handler.misc :as misc]
   [cljs.core.async :refer [go <!]]))

(defmulti handle-event
  (fn [_state event]
    (:type event)))

(defmethod handle-event :default
  [state event]
  (println "event ignored:" event)
  state)

;; implementations

;; system/sync-state
;; receiving this event when game account reflect there's a new player
;; will only success when game is in prepare status.
;; will start game after a few seconds.
(defmethod handle-event :system/sync-state
  [state {{:keys [players game-account-state]} :data, :as event}]

  (when-not (#{:game-status/init :game-status/settle :game-status/showdown} (:status state))
    (misc/invalid-game-status! state event))

  (-> state
      (misc/reset-game-state)
      (misc/merge-sync-state players game-account-state)
      (misc/try-start-game)))

;; system/start-game
;; receiving this event when game start.
;; set game status to shuffle
;; generate a default deck of cards
;; ask the first player (BTN) to shuffle the cards.
(defmethod handle-event :system/start-game
  [{:keys [status player-map], :as state}
   {{:keys [btn]} :data, :as event}]

  (when-not (= :game-status/init status)
    (misc/invalid-game-status! state event))

  ;; Start when all players ready
  ;; otherwise kick all non-ready players
  (if (every? #(= :normal (:online-status %)) (vals player-map))
    (go
     (let [ciphers (encrypt/cards->card-strs misc/default-deck-of-cards)
           data    (-> (<! (encrypt/encrypt-ciphers-with-default-shuffle-key ciphers))
                       (encrypt/ciphers->hex))]
       (-> state
           (assoc :prepare-cards [{:data      data,
                                   :op        :init,
                                   :player-id nil}]
                  :btn           btn
                  :shuffle-player-id (:player-id (misc/get-player-by-position state btn))
                  :status        :game-status/shuffle))))
    (-> state
        (misc/kick-dropout-players))))

;; client/shuffle-cards
;; receiving this event when player submit shuffled cards
;; each player will shuffle cards and encrypt with their shuffle key.
(defmethod handle-event :client/shuffle-cards
  [{:keys [shuffle-player-id btn prepare-cards status], :as state}
   {{:keys [data]} :data, player-id :player-id, :as event}]

  (when-not (= :game-status/shuffle status)
    (misc/invalid-game-status! state event))

  (when-not (= shuffle-player-id player-id)
    (misc/invalid-player-id! state event))

  (let [new-shuffle-player-id (misc/next-player-id state shuffle-player-id)
        new-prepare-cards     (conj prepare-cards
                                    {:data      data,
                                     :op        :shuffle,
                                     :player-id player-id})]
    (cond
      ;; shuffle finished
      (misc/btn-player-id? state new-shuffle-player-id)
      (-> state
          (assoc :prepare-cards new-prepare-cards
                 :shuffle-player-id nil
                 :encrypt-player-id (:player-id (misc/get-player-by-position state btn))
                 :status        :game-status/encrypt))

      :else
      (-> state
          (assoc :prepare-cards     new-prepare-cards
                 :shuffle-player-id new-shuffle-player-id)))))

;; client/encrypt-cards
;; receiving this event when player submit encrypted cards
;; each player will remove its shuffle key, and encrypt each cards with encrypt keys respectly.
(defmethod handle-event :client/encrypt-cards
  [{:keys [encrypt-player-id prepare-cards status], :as state}
   {{:keys [data]} :data, player-id :player-id, :as event}]

  (when-not (= :game-status/encrypt status)
    (misc/invalid-game-status! state event))

  (when-not (= encrypt-player-id player-id)
    (misc/invalid-player-id! state event))

  (let [new-encrypt-player-id (misc/next-player-id state encrypt-player-id)
        new-prepare-cards     (conj prepare-cards
                                    {:data      data,
                                     :op        :encrypt,
                                     :player-id player-id})]
    (go

     (cond
       ;; encrypt finished
       (misc/btn-player-id? state new-encrypt-player-id)
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
             (misc/update-require-key-idents)))

       :else
       (-> state
           (assoc :prepare-cards     new-prepare-cards
                  :encrypt-player-id new-encrypt-player-id))))))

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

  (let [new-state (update state :share-key-map merge share-keys)]
    (cond
      (seq (misc/list-missing-key-idents new-state))
      new-state

      (= :settle after-key-share)
      (misc/settle new-state :showdown)

      (= :init-street after-key-share)
      (-> new-state
          (assoc :status  :game-status/play
                 :bet-map nil)
          (misc/next-state))

      (= :runner after-key-share)
      (-> new-state
          (assoc :street :street/showdown)
          (misc/settle :runner))

      :else
      (throw (ex-info "Invalid after-key-share" {:after-key-share after-key-share})))))

;; Alive
;; A event received when a client is ready for next game
;; All client whiout this event will be kicked when game start
(defmethod handle-event :client/alive
  [{:keys [status player-map], :as state}
   {player-id :player-id,
    :as       event}]
  (when-not (= status :game-status/init)
    (misc/invalid-game-status! state event))

  (-> state
      (assoc-in [:player-map player-id :online-status] :normal)
      (misc/try-start-game)))

;; Leave
;; A event received when a player leave game
;; 1. Current game is running
;; The player must share all the necessary keys for current game
;; Otherwise this event will not success.
;; 2. Game is not running
;; Send a claim transaction for player
(defmethod handle-event :client/leave
  [{:keys [status after-key-share player-map], :as state}
   {{:keys [share-keys]} :data,
    player-id :player-id,
    :as       event}]
  (let [state (update-in state [:player-map player-id] assoc
                         :online-status :leave
                         :status :player-status/fold)]
    (cond
      ;; Game is not running, can leave immetdiately
      (#{:game-status/init :game-status/settle :game-status/showdown} status)
      (-> state
          (misc/kick-dropout-players))
      ;; Game is running, calculate next state
      true
      (misc/next-state state))))

(defmethod handle-event :player/fold
  [{:keys [status action-player-id], :as state}
   {player-id :player-id, :as event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/fold)
      (update :player-actions conj {:action :fold, :player-id player-id})
      (misc/next-state)))

(defmethod handle-event :player/call
  [{:keys [bet-map player-map status action-player-id street-bet], :as state}
   {player-id :player-id, :as event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (let [bet            (get bet-map player-id 0)
        player         (get player-map player-id)
        amount-to-call (- street-bet bet)
        [bet player allin?] (misc/take-bet-from-player player amount-to-call)
        status         (if allin? :player-status/allin :player-status/acted)]
    (-> state
        (assoc-in [:player-map player-id] player)
        (update-in [:bet-map player-id] (fnil + 0) bet)
        (assoc-in [:player-map player-id :status] status)
        (update :player-actions conj {:action :call, :amount bet, :player-id player-id})
        (misc/next-state))))

(defmethod handle-event :player/check
  [{:keys [bet-map status action-player-id street-bet], :as state}
   {player-id :player-id, :as event}]

  (when-not (= :game-status/play status)
    (misc/invalid-game-status! state event))

  (when-not (= action-player-id player-id)
    (misc/player-not-in-action! state event))

  (when-not (= street-bet (get bet-map player-id))
    (misc/player-cant-check! state event))

  (-> state
      (assoc-in [:player-map player-id :status] :player-status/acted)
      (update :player-actions conj {:action :check, :player-id player-id})
      (misc/next-state)))

(defmethod handle-event :player/bet
  [{:keys [bet-map player-map status min-raise action-player-id street-bet], :as state}
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
             (zero? street-bet))
    (misc/player-cant-bet! state event))

  (when-not (pos-int? amount)
    (misc/invalid-amount! state event))

  (when (< amount min-raise)
    (misc/player-bet-too-small! state event))

  (let [player (get player-map player-id)
        [bet player allin?] (misc/take-bet-from-player player amount)]
    (-> state
        (assoc-in [:player-map player-id] player)
        (assoc-in [:bet-map player-id] bet)
        (assoc-in [:player-map player-id :status]
                  (if allin? :player-status/allin :player-status/acted))
        (assoc :min-raise  bet
               :street-bet bet)
        (update :player-actions conj {:action :bet, :amount amount, :player-id player-id})
        (misc/next-state))))

(defmethod handle-event :player/raise
  [{:keys [bet-map player-map min-raise status action-player-id street-bet], :as state}
   {{:keys [amount]} :data,
    player-id        :player-id,
    :as              event}]

  (let [curr-bet (get bet-map player-id 0)]

    (when-not (= :game-status/play status)
      (misc/invalid-game-status! state event))

    (when-not (= action-player-id player-id)
      (misc/player-not-in-action! state event))

    (when (zero? street-bet)
      (misc/player-cant-raise! state event))

    (when-not (> amount 0)
      (misc/invalid-amount! state event))

    (when (< (+ curr-bet amount) (+ street-bet min-raise))
      (misc/player-raise-too-small! state event))

    (let [player         (get player-map player-id)
          [bet player allin?] (misc/take-bet-from-player player amount)
          new-street-bet (+ curr-bet bet)
          new-min-raise  (- new-street-bet street-bet)]
      (-> state
          (assoc-in [:player-map player-id] player)
          (update-in [:bet-map player-id] (fnil + 0) bet)
          (assoc-in [:player-map player-id :status]
                    (if allin? :player-status/allin :player-status/acted))
          (assoc :min-raise  new-min-raise
                 :street-bet new-street-bet)
          (update :player-actions conj {:action :raise, :amount amount, :player-id player-id})
          (misc/next-state)))))
