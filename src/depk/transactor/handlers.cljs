(ns depk.transactor.handlers
  (:require
   [depk.transactor.log :as log]
   [clojure.string :as str]
   [cljs.core.async :as a]
   [depk.transactor.util :as u]
   [depk.transactor.game :as game]
   [depk.transactor.tournament :as tournament]
   [depk.transactor.state.worker-manager :refer [worker-manager]]
   [cognitect.transit :as t]
   [depk.transactor.constant :as c]
   ["uuid" :as uuid]
   [goog.string :as gstr]))

;;; Websocket Event Handler

(defmulti event-msg-handler :id)

(defn attach-event-handler
  "Attach game event handler to websocket channel."
  [ws-conn]
  (let [{:keys [ch-chsk]} ws-conn]
    (a/go-loop [evt (a/<! ch-chsk)]
      (try
        (event-msg-handler evt)
        (catch js/Error e
          (log/log "💥" nil "Error in event message handler: %s" (ex-message e))))
      (recur (a/<! ch-chsk)))))

(defmethod event-msg-handler :default
  [{:as ev-msg, :keys [event id ?data ring-req ?reply-fn send-fn connected-uids]}]
  (let [[ev-id ev-data] event]
    (case ev-id
      :chsk/uidport-close
      (let [[game-id player-id] ev-data]
        (game/dropout @worker-manager game-id player-id))

      :chsk/uidport-open
      (let [[game-id player-id] ev-data]
        (game/alive @worker-manager game-id player-id))

      :noop)))

(defmethod event-msg-handler :game/attach
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (let [[game-id player-id] uid]
    (a/go
     (if (a/<! (game/attach-game @worker-manager game-id player-id))
       (?reply-fn {:result :ok})
       (?reply-fn {:result :err})))))

(defmethod event-msg-handler :game/state
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (let [[game-id player-id] uid
        state (game/state @worker-manager game-id player-id)]
    (if (seq state)
      (?reply-fn {:result :ok,
                  :state  state})
      (?reply-fn {:result :err}))))

(defmethod event-msg-handler :client/leave
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id]     uid
         {:keys [released-keys]} ?data]
     (a/<! (game/leave @worker-manager game-id player-id released-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/ready
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id rsa-pub sig] uid]
     (a/<! (game/ready @worker-manager game-id player-id rsa-pub sig))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/shuffle-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/shuffle-cards @worker-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/encrypt-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/encrypt-cards @worker-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/share-keys
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/share-keys @worker-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/call
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]

  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-call @worker-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/raise
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (a/<! (game/player-raise @worker-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/check
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-check @worker-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/bet
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (a/<! (game/player-bet @worker-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/fold
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/player-fold @worker-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :message/text
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  (let [[game-id player-id] uid
        msg {:game-id    (:game-id ?data),
             :sender     player-id,
             :text       (:text ?data),
             :message/id (uuid/v4),
             :timestamp  (js/Date.)}]
    (doseq [u (:any @connected-uids)]
      (when (= game-id (first u))
        (send-fn u [:message/text msg])))))

(defmethod event-msg-handler :message/sticker
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
   (let [[game-id player-id] uid
         msg {:game-id    (:game-id ?data),
              :sender     player-id,
              :sticker-id (:sticker-id ?data),
              :message/id (uuid/v4),
              :timestamp  (js/Date.)}]
     (doseq [u (:any @connected-uids)]
       (when (= game-id (first u))
         (send-fn u [:message/sticker msg]))))))

(defmethod event-msg-handler :tournament/join
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  (a/go
  ))

;;; HTTP handlers

(defn get-tournament
  "Get the tournament information."
  [^js req ^js res]
  (let [tournament-id (aget req "params" "tournamentId")
        state         (tournament/state @worker-manager tournament-id)]
    (doto res
     (.contentType "application/transit+json")
     (.send state))))

(defn load-tournament
  "Load the tournament."
  [^js req ^js res]
  (let [tournament-id (aget req "params" "tournamentId")
        signed-message (aget req "body" "signed-message")
        w (t/writer :json {:handlers {js/BigInt u/bigint-writer}})]
    (tournament/launch-tournament @worker-manager tournament-id)
    (doto res
     (.contentType "application/transit+json")
     (.send (t/write w {:result :ok})))))

(defn stats
  "Return current running status.

  Including how many games are running."
  [^js req ^js res]
  (let [w (t/writer :json {:handlers {js/BigInt u/bigint-writer}})]
    (doto res
     (.contentType "application/transit+json")
     (.send
      (t/write w
               {:version c/version,
                :games   (game/list-running-games @worker-manager),
                :players (game/list-players @worker-manager)})))))
