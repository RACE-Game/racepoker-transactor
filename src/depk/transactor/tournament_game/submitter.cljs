(ns depk.transactor.tournament-game.submitter
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]
   [clojure.string      :as str]))

(defn start
  [game-id input output post-msg-fn]
  (a/go-loop []
    (let [{:keys [data type]}  (a/<! input)
          ;; there's no rake in tournament
          {:keys [settle-map]} data]
      (case type
        :system/settle
        (do
          (log/log "☎️" game-id
                   "Submit settlement to tournament: %s"
                   (prn-str settle-map))
          (post-msg-fn {:broadcast     :broadcast/tournament-game-settle,
                        :game-id       game-id,
                        :tournament-id (first (str/split game-id #"#")),
                        :settle-map    settle-map})
          (recur))

        (do
          (a/close! output)
          (log/log "💤" game-id "Submitter quit"))))))

(defrecord TournamentGameSubmitter [input output post-msg-fn])

(extend-type TournamentGameSubmitter
 ep/IAttachable
 (-input [this]
   (:input this))
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   [:system/settle
    :system/shutdown])

 ep/IWaitable
 (-wait [this]
   (a/go (a/<! (:output this))))

 ep/IComponent
 (-start [this opts]
   (let [{:keys [input output post-msg-fn]} this
         {:keys [game-id]} opts]
     (log/log "🎉" game-id "Start submitter")
     (start game-id input output post-msg-fn))))

(defn make-tournament-game-submitter
  [post-msg-fn]
  (let [input (a/chan)
        output (a/promise-chan)]
    (->TournamentGameSubmitter input output post-msg-fn)))
