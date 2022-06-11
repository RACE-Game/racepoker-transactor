(ns depk.transactor.game
  "Commit game events to Arweave and Solana."
  (:require
   [depk.transactor.util :refer [go-try <!?]]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.worker :as worker]
   [depk.transactor.game.event-loop :as eloop]
   [depk.transactor.game.manager :as manager]
   [depk.transactor.log :as log]
   [depk.transactor.event.protocol :as ep]
   ["buffer" :as buffer]))

(defn error-game-not-exist!
  [game-id]
  (throw (ex-info "game not exist" {:game-id game-id})))

(defn attach-game
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (log/infof "👔player[%s] attach to game [%s]" player-id game-id)
   (manager/try-start-game game-manager game-id)))

(defn state
  [game-manager game-id]
  {:pre [(string? game-id)]}
  (when-let [game-worker (manager/find-game-unchecked game-manager game-id)]
    (m/game-state->resp (worker/get-snapshot game-worker))))

;; Leaving game
;; Must provides all keys
(defn leave
  [game-manager game-id player-id released-keys]
  {:pre [(string? game-id)
         (or (nil? released-keys)
             (vector? released-keys))]}
  (go-try
   ;; (log/infof "player[%s] leave game [%s]" player-id game-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/leave
                                      (worker/get-snapshot game-worker)
                                      {:released-keys released-keys}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn ready
  [game-manager game-id player-id rsa-pub sig]
  {:pre [(string? player-id)
         (string? game-id)
         (string? rsa-pub)
         (string? sig)]}
  (go-try
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/ready
                                      (worker/get-snapshot game-worker)
                                      {:rsa-pub rsa-pub,
                                       :sig     sig}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Alive, reconnect
(defn alive
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :system/alive
                                      (worker/get-snapshot game-worker)
                                      {}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Dropout, disconnect
(defn dropout
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :system/dropout
                                      (worker/get-snapshot game-worker)
                                      {}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Shuffle Cards

(defn shuffle-cards
  [game-manager game-id player-id data]
  {:pre [(string? game-id)
         (string? player-id)
         (some? data)]}
  (go-try
   ;; (log/infof "player[%s] shuffle cards" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/shuffle-cards
                                      (worker/get-snapshot game-worker)
                                      {:data data}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn encrypt-cards
  [game-manager game-id player-id data]
  {:pre [(string? game-id)
         (string? player-id)
         (some? data)]}
  (go-try
   ;; (log/infof "player[%s] encrypt cards" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/encrypt-cards
                                      (worker/get-snapshot game-worker)
                                      {:data data}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn share-keys
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)
         (string? player-id)
         (map? share-keys)]}
  (go-try
   ;; (log/infof "player[%s] share keys" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/share-keys
                                      (worker/get-snapshot game-worker)
                                      {:share-keys share-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn release
  [game-manager game-id player-id released-keys]
  {:pre [(string? game-id)
         (string? player-id)
         (vector? released-keys)]}
  (go-try
   ;; (log/infof "player[%s] release keys" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/release
                                      (worker/get-snapshot game-worker)
                                      {:released-keys released-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-bet
  [game-manager game-id player-id amount]
  {:pre [(string? game-id)
         (string? player-id)
         (> amount 0)]}
  (go-try
   ;; (log/infof "player[%s] bet" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :player/bet
                                      (worker/get-snapshot game-worker)
                                      {:amount amount}
                                      player-id))
     (error-game-not-exist! game-id))))


(defn player-raise
  [game-manager game-id player-id amount]
  {:pre [(string? game-id)
         (string? player-id)
         (> amount 0)]}
  (go-try
   ;; (log/infof "player[%s] raise" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :player/raise
                                      (worker/get-snapshot game-worker)
                                      {:amount amount}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-call
  [game-manager game-id player-id]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   ;; (log/infof "player[%s] call" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :player/call
                                      (worker/get-snapshot game-worker)
                                      {}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-fold
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   ;; (log/infof "player[%s] fold" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :player/fold
                                      (worker/get-snapshot game-worker)
                                      {:share-keys share-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-check
  [game-manager game-id player-id]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   ;; (log/infof "player[%s] check" player-id)
   (if-let [game-worker (manager/find-game game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :player/check
                                      (worker/get-snapshot game-worker)
                                      {}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-reveal [])

(defn player-musk [])

(defn fetch-histories
  [_game-manager _game-id])

(defn list-running-games
  [game-manager]
  (manager/list-running-games game-manager))

(defn list-players
  [game-manager]
  (manager/list-players game-manager))
