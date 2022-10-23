(ns depk.transactor.game
  "Commit game events to Arweave and Solana."
  (:require
   [depk.transactor.util           :refer [go-try tournament-game-id?]]
   [depk.transactor.game.models    :as m]
   [depk.transactor.game.worker    :as worker]
   [depk.transactor.worker-manager :as manager]
   [depk.transactor.log            :as log]))

(defn error-game-not-exist!
  [game-id]
  (throw (ex-info "game not exist" {:game-id game-id})))

(defn attach-game
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] attach to game" player-id)
   ;; We can only start normal game through this API
   (if-not (tournament-game-id? game-id)
     (if-let [game-worker (manager/find-worker-unchecked game-manager game-id)]
       (do
         (worker/send-event game-worker {:type :system/start-synchronizer})
         :ok)
       (do
         (manager/try-start game-manager game-id worker/make-worker)
         :ok))
     :ok)))

(defn state
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (when-let [game-worker (manager/find-worker-unchecked game-manager game-id)]
    (log/log "➡️" game-id "Player[%s] fetch game state" player-id)
    (:serialized-state (worker/get-snapshot game-worker))))

;; Leaving game
;; Must provides all keys
(defn leave
  [game-manager game-id player-id released-keys]
  {:pre [(string? game-id)
         (or (nil? released-keys)
             (vector? released-keys))]}
  (go-try
   (log/log "➡️" game-id "Player[%s] leave game" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/leave
                                      (worker/get-snapshot game-worker)
                                      {:released-keys released-keys}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn ready
  [game-manager game-id player-id rsa-pub ed-pub sig]
  {:pre [(string? player-id)
         (string? game-id)
         (string? rsa-pub)
         (string? ed-pub)
         (string? sig)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] ready" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/ready
                                      (worker/get-snapshot game-worker)
                                      {:rsa-pub rsa-pub,
                                       :sig     sig,
                                       :ed-pub  ed-pub}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn fix-keys
  [game-manager game-id player-id rsa-pub ed-pub sig]
  {:pre [(string? player-id)
         (string? game-id)
         (string? rsa-pub)
         (string? ed-pub)
         (string? sig)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] fix keys" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/fix-keys
                                      (worker/get-snapshot game-worker)
                                      {:rsa-pub rsa-pub,
                                       :sig     sig,
                                       :ed-pub  ed-pub}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Alive, reconnect
(defn alive
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] reconnect" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] dropout" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :system/dropout
                                      (worker/get-snapshot game-worker)
                                      {}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Shuffle Cards

(defn shuffle-cards
  [game-manager game-id player-id secret-id sig data]
  {:pre [(string? game-id)
         (string? player-id)
         (string? secret-id)
         (string? sig)
         (some? data)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] shuffle cards" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/shuffle-cards
                                      (worker/get-snapshot game-worker)
                                      {:data      data,
                                       :sig       sig,
                                       :secret-id secret-id}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn encrypt-cards
  [game-manager game-id player-id secret-id sig data]
  {:pre [(string? game-id)
         (string? player-id)
         (string? secret-id)
         (string? sig)
         (some? data)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] encrypt cards" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/encrypt-cards
                                      (worker/get-snapshot game-worker)
                                      {:data      data,
                                       :sig       sig,
                                       :secret-id secret-id}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn share-keys
  [game-manager game-id player-id share-keys secret-id sig]
  {:pre [(string? game-id)
         (string? player-id)
         (map? share-keys)
         (string? secret-id)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] share keys" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
     (worker/send-event game-worker
                        (m/make-event :client/share-keys
                                      (worker/get-snapshot game-worker)
                                      {:share-keys share-keys,
                                       :secret-id  secret-id,
                                       :sig        sig}
                                      player-id))
     (error-game-not-exist! game-id))))

(defn release
  [game-manager game-id player-id released-keys]
  {:pre [(string? game-id)
         (string? player-id)
         (vector? released-keys)]}
  (go-try
   (log/log "➡️" game-id "Player[%s] release keys" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] bet: %s" player-id amount)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] raise: %s" player-id amount)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] call" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] fold" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
   (log/log "➡️" game-id "Player[%s] check" player-id)
   (if-let [game-worker (manager/find-worker game-manager game-id)]
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
  [manager]
  (when-let [worker-map @(:worker-map manager)]
    (->> worker-map
         (keep (fn [[game-id w]]
                 (let [{:keys [player-ids start-time]} (worker/get-snapshot w)]
                   [game-id
                    {:start-time start-time,
                     :player-ids player-ids}])))
         (into {}))))

(defn list-players
  [manager]
  (when-let [worker-map @(:worker-map manager)]
    (->> worker-map
         (mapcat (fn [[_ w]]
                   (let [{:keys [player-ids]} (worker/get-snapshot w)]
                     player-ids)))
         (distinct))))
