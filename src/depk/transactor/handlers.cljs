(ns depk.transactor.handlers
  (:require
   [depk.transactor.log :as log]
   [clojure.string      :as str]
   [cljs.core.async     :as a]
   [depk.transactor.game :as game]
   [depk.transactor.state.game-manager :refer [game-manager]]
   [cognitect.transit   :as t]
   ["uuid"              :as uuid]))

;; Websocket Event Handler

(defmulti event-msg-handler :id)

(defmethod event-msg-handler :default
  [{:as ev-msg, :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [[ev-id ev-data] event]
    (case ev-id
      :chsk/uidport-close
      (game/dropout @game-manager (first ev-data) (last ev-data))

      :chsk/uidport-open
      (game/alive @game-manager (first ev-data) (last ev-data))

      :noop)))

(defmethod event-msg-handler :game/attach
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/infof "🚩Attach game: %s" uid)
  (let [[game-id player-id] uid]
    (a/go
     (if (a/<! (game/attach-game @game-manager game-id player-id))
       (?reply-fn {:result :ok})
       (?reply-fn {:result :err})))))

(defmethod event-msg-handler :game/state
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Sync game state: %s" uid)
  (let [[game-id player-id] uid
        state (game/state @game-manager game-id)]
    (?reply-fn {:result :ok,
                :state  state})))

(defmethod event-msg-handler :client/leave
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/infof "🚩Leave game: %s" uid)
  (a/go
   (let [[game-id player-id]     uid
         {:keys [released-keys]} ?data]
     (a/<! (game/leave @game-manager game-id player-id released-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/ready
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Keep alive: %s" uid)

  (a/go
   (let [[game-id player-id] uid]

     (a/<! (game/ready @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/shuffle-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Client shuffle cards: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/shuffle-cards @game-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/encrypt-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Client encrypt cards: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/encrypt-cards @game-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/share-keys
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Client share keys: %s" uid)
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/share-keys @game-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/call
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/infof "⚽Call: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-call @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/raise
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (log/infof "⚽Raise: %s %s" uid amount)
     (a/<! (game/player-raise @game-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/check
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/infof "⚽Check: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-check @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/bet
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (log/infof "⚽Bet: %s %s" uid amount)
     (a/<! (game/player-bet @game-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/fold
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/infof "⚽Fold: %s" uid)
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/player-fold @game-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :message/text
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Player send text message: %s" ?data)
  (let [[game-id player-id] uid
        msg {:game-id    (:game-id ?data),
             :sender     player-id,
             :text       (:text ?data),
             :message/id (uuid/v4)}]
    (doseq [u (:any @connected-uids)]
      (when (= game-id (first u))
        (send-fn u [:message/text msg])))))

(defmethod event-msg-handler :message/sticker
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  ;; (log/infof "Player send sticker message: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         msg {:game-id    (:game-id ?data),
              :sender     player-id,
              :sticker-id (:sticker-id ?data),
              :message/id (uuid/v4)}]
     (doseq [u (:any @connected-uids)]
       (when (= game-id (first u))
         (send-fn u [:message/sticker msg]))))))

(defn attach-event-handler
  "Attach game event handler to websocket channel."
  [ws-conn]
  (let [{:keys [ch-chsk]} ws-conn]
    (a/go-loop [evt (a/<! ch-chsk)]
      (try
        (event-msg-handler evt)
        (catch js/Error e
          (log/errorf "💥Error in event message handler: %s" (ex-message e))))
      (recur (a/<! ch-chsk)))))

;; HTTP handlers

(defn joined-games-list
  [^js req ^js res]
  (let [player-id (aget (.-query req) "player-id")
        w         (t/writer :json)]
    (doto res
     (.contentType "application/transit+json")
     (.send
      (t/write w
               (vec (game/list-game-ids-by-player-id
                     @game-manager
                     player-id)))))))

(defn status
  "Return current running status.

  Including how many games are running."
  [^js req ^js res])
