(ns depk.transactor.game
  "Commit game events to Arweave and Solana."
  (:require
   [depk.transactor.util :refer [go-try <!?]]
   [depk.transactor.game.models :as m]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.game.event-loop :as eloop]
   [depk.transactor.game.manager :as manager]
   [depk.transactor.log :as log]
   [depk.transactor.event.protocol :as ep]
   [solana-clj.publickey :as pubkey]
   ["tweetnacl" :as nacl]
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
  (when-let [game-handle (manager/find-game game-manager game-id)]
    (m/game-state->resp (handle/get-snapshot game-handle))))

;; Leaving game
;; Must provides all keys
(defn leave
  [game-manager game-id player-id released-keys]
  {:pre [(string? game-id)
         (vector? released-keys)]}
  (go-try
   ;; (log/infof "player[%s] leave game [%s]" player-id game-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/leave
                                      (handle/get-snapshot game-handle)
                                      {:released-keys released-keys}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

(defn ready
  [game-manager game-id player-id uuid sig]
  {:pre [(string? game-id)]}
  (go-try
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (let [game-account-snapshot (handle/get-game-account-snapshot game-handle)
           player   (first (filter #(= player-id (str (:pubkey %)))
                                   (:players game-account-snapshot)))
           ident-pk (:ident player)
           k        (pubkey/to-buffer ident-pk)]

       (if (nacl/sign.detached.verify
            (buffer/Buffer.from uuid)
            (buffer/Buffer.from sig "hex")
            k)
         (handle/send-event game-handle
                            (m/make-event :client/ready
                                          (handle/get-snapshot game-handle)
                                          {}
                                          player-id))
         (do (log/infof "⭕Signature check failed.")
             (throw (ex-info "Reject ready" {:reason "Signature check failed"})))))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Alive, reconnect
(defn alive
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :system/alive
                                      (handle/get-snapshot game-handle)
                                      {}
                                      player-id))
     (throw (ex-info "game not exist" {:game-id game-id})))))

;; Dropout, disconnect
(defn dropout
  [game-manager game-id player-id]
  {:pre [(string? game-id)]}
  (go-try
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :system/dropout
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
   ;; (log/infof "player[%s] shuffle cards" player-id)
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
   ;; (log/infof "player[%s] encrypt cards" player-id)
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
   ;; (log/infof "player[%s] share keys" player-id)
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/share-keys
                                      (handle/get-snapshot game-handle)
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
   (if-let [game-handle (manager/find-game game-manager game-id)]
     (handle/send-event game-handle
                        (m/make-event :client/release
                                      (handle/get-snapshot game-handle)
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
   ;; (log/infof "player[%s] raise" player-id)
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
   ;; (log/infof "player[%s] call" player-id)
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
   ;; (log/infof "player[%s] fold" player-id)
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
   ;; (log/infof "player[%s] check" player-id)
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
  [_game-manager _game-id])

(defn list-game-ids-by-player-id
  [game-manager player-id]
  (manager/list-game-ids-by-player-id game-manager player-id))
