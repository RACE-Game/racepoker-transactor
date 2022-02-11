(ns depk.transactor.game
  "Commit game events to Arweave and Solana."
  (:require
   [depk.transactor.util :refer [go-try <!?]]
   [depk.transactor.game.event-loop :as event-loop]
   [depk.transactor.game.event-handler :as event-handler]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.game.encrypt :as encrypt]
   [depk.transactor.game.manager :as manager]
   [depk.transactor.game.api :as api]
   [depk.transactor.log :as log]))

(defn error-game-not-exist!
  [game-id]
  (throw (ex-info "game not exist" {:game-id game-id})))

(defn attach-game
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (log/infof "player[%s] attach to game [%s]" player-id game-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     game-handle
     (<!? (manager/load-game game-manager game-id)))))

(defn state
  [game-manager game-id]
  {:pre [(string? game-id)]}
  (go-try
   (when-let [game-handle (manager/find-game game-manager game-id)]
     (select-keys
      (handle/get-snapshot game-handle)
      [:game-no :status :street :player-map :pots :min-raise
       :street-bet :bet-map :action-player-id
       :showdown-map :prize-map :state-id :prepare-cards
       :shuffle-player-id :encrypt-player-id
       :btn :sb :bb :require-key-idents :share-key-map
       :card-ciphers :player-actions :winning-type]))))

;; Leaving game
;; Must provides all keys
(defn leave
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)]}
  (go-try
   (log/infof "player[%s] leave game [%s]" player-id game-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/leave
                                      (handle/get-snapshot game-handle)
                                      {:share-keys share-keys}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Alive, confirmation before start
(defn alive
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (log/infof "player[%s] confirm alive, game [%s]" player-id game-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/alive
                                      (handle/get-snapshot game-handle)
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
   (log/infof "player[%s] shuffle cards" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/shuffle-cards
                                      (handle/get-snapshot game-handle)
                                      {:data data}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn encrypt-cards
  [game-manager game-id player-id data]
  {:pre [(string? game-id)
         (string? player-id)
         (some? data)]}
  (go-try
   (log/infof "player[%s] encrypt cards" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/encrypt-cards
                                      (handle/get-snapshot game-handle)
                                      {:data data}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn share-keys
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)
         (string? player-id)
         (map? share-keys)]}
  (go-try
   (log/infof "player[%s] share keys" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/share-keys
                                      (handle/get-snapshot game-handle)
                                      {:share-keys share-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn release
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)
         (string? player-id)
         (map? share-keys)]}
  (go-try
   (log/infof "player[%s] release keys" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/release
                                      (handle/get-snapshot game-handle)
                                      {:share-keys share-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-bet
  [game-manager game-id player-id amount]
  {:pre [(string? game-id)
         (string? player-id)
         (> amount 0)]}
  (go-try
   (log/infof "player[%s] bet" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :player/bet
                                      (handle/get-snapshot game-handle)
                                      {:amount amount}
                                      player-id))
     (error-game-not-exist! game-id))))


(defn player-raise
  [game-manager game-id player-id amount]
  {:pre [(string? game-id)
         (string? player-id)
         (> amount 0)]}
  (go-try
   (log/infof "player[%s] raise" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :player/raise
                                      (handle/get-snapshot game-handle)
                                      {:amount amount}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-call
  [game-manager game-id player-id]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   (log/infof "player[%s] call" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :player/call
                                      (handle/get-snapshot game-handle)
                                      {}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-fold
  [game-manager game-id player-id share-keys]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   (log/infof "player[%s] fold" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :player/fold
                                      (handle/get-snapshot game-handle)
                                      {:share-keys share-keys}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-check
  [game-manager game-id player-id]
  {:pre [(string? game-id)
         (string? player-id)]}
  (go-try
   (log/infof "player[%s] check" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :player/check
                                      (handle/get-snapshot game-handle)
                                      {}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn player-reveal [])

(defn player-musk [])

(defn fetch-histories
  [game-manager game-id]
  (manager/fetch-game-histories game-manager game-id))
