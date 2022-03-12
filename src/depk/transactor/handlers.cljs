(ns depk.transactor.handlers
  (:require
   [depk.transactor.log :as log]
   [clojure.string :as str]
   [cljs.core.async :as a]
   [depk.transactor.util :refer [<!? def-async-handler]]
   [depk.transactor.game :as game]
   [solana-clj.publickey :as pubkey]
   [depk.transactor.state.game-manager :refer [game-manager]]
   [depk.transactor.game.api :as api]
   [depk.transactor.state.api :refer [chain-api]]
   ["buffer" :as buffer]
   ["tweetnacl" :as nacl]
   ["uuid" :as uuid]))

;; Websocket Event Handler

(defmulti event-msg-handler :id)

(defmethod event-msg-handler :default
  [{:as ev-msg, :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Unhandled event: %s" ev-msg)
  (when ?reply-fn
    (?reply-fn {:unmatched-event-as-echoed-from-server ev-msg})))

(defmethod event-msg-handler :game/attach
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Attach game: %s" uid)
  (let [[game-id player-id] uid]
    (a/go
     (if (a/<! (game/attach-game @game-manager game-id player-id))
       (?reply-fn {:result :ok})
       (?reply-fn {:result :err})))))

(defmethod event-msg-handler :game/state
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Sync game state: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         state (a/<! (game/state @game-manager game-id))]
     (?reply-fn {:result :ok,
                 :state  state}))))

(defmethod event-msg-handler :client/leave
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Leave game: %s" uid)
  (a/go
   (let [[game-id player-id]     uid
         {:keys [released-keys]} ?data]
     (a/<! (game/leave @game-manager game-id player-id released-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/alive
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Keep alive: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/alive @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/shuffle-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Client shuffle cards: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/shuffle-cards @game-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/encrypt-cards
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Client encrypt cards: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [data]}      ?data]
     (a/<! (game/encrypt-cards @game-manager game-id player-id data))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/share-keys
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Client share keys: %s" uid)
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/share-keys @game-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :client/release
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Client release keys: %s" uid)
  (a/go
   (let [[game-id player-id]     uid
         {:keys [released-keys]} ?data]
     (a/<! (game/release @game-manager game-id player-id released-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/call
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Call: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-call @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/raise
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Raise: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (a/<! (game/player-raise @game-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/check
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Check: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (a/<! (game/player-check @game-manager game-id player-id))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/bet
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Bet: %s" uid)
  (a/go
   (let [[game-id player-id] uid
         {:keys [amount]}    ?data]
     (a/<! (game/player-bet @game-manager game-id player-id amount))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :player/fold
  [{:as ev-msg, :keys [event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Fold: %s" uid)
  (a/go
   (let [[game-id player-id]  uid
         {:keys [share-keys]} ?data]
     (a/<! (game/player-fold @game-manager game-id player-id share-keys))
     (?reply-fn {:result :ok}))))

(defmethod event-msg-handler :message/text
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Player send text message: %s" uid)
  (let [[game-id player-id] uid
        msg {:game-id    game-id,
             :sender     player-id,
             :text       (:text ?data),
             :message/id (uuid/v4)}]
    (doseq [u (:any @connected-uids)]
      (when (= game-id (first u))
        (send-fn u [:message/text msg])))))

(defmethod event-msg-handler :message/sticker
  [{:as ev-msg, :keys [connected-uids event id uid ?data ring-req ?reply-fn send-fn]}]
  (log/debugf "Player send sticker message: %s" uid)
  (a/go
   (let [[game-id player-id] uid]
     (println @connected-uids)
     (when ?reply-fn
       (?reply-fn {:result :ok})))))

(defn attach-event-handler
  [ch-recv]
  (a/go-loop [evt (a/<! ch-recv)]
    (try
      (event-msg-handler evt)
      (catch js/Error e (log/error e)))
    (recur (a/<! ch-recv))))

(defn user-id-fn
  [{:keys [params]}]
  (println "Receive WS connection with params: " (prn-str params))
  (let [{:keys [pubkey sig game-id]} params
        k   (pubkey/to-buffer (pubkey/make-public-key pubkey))
        msg (buffer/Buffer.from
             (str pubkey
                  " sign with game "
                  game-id
                  " for RACE Poker."))]
    (if (nacl/sign.detached.verify msg (buffer/Buffer.from sig "hex") k)
      (do (println "Signature check succeed.")
          [game-id pubkey])
      (println "Signature check failed."))))
