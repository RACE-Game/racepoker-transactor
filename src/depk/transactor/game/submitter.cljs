(ns depk.transactor.game.submitter
  (:require
   [cljs.core.async      :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log  :as log]
   [depk.transactor.util :as u]))

(def settle-batch-size
  "A batch size for settlement."
  (if goog.DEBUG 2 5))

(defn merge-settle-item
  [s1 s2]
  (let [settle-status (cond
                        (or (= :leave (:settle-status s1))
                            (= :leave (:settle-status s2)))
                        :leave

                        :else
                        :no-update)

        amount        (+ (* (if (= :chips-sub (:settle-type s1)) (js/BigInt -1) (js/BigInt 1))
                            (:amount s1))
                         (* (if (= :chips-sub (:settle-type s2)) (js/BigInt -1) (js/BigInt 1))
                            (:amount s2)))

        rake          (+ (get s1 :rake (js/BigInt 0))
                         (get s2 :rake (js/BigInt 0)))

        settle-type   (cond
                        (> amount (js/BigInt 0))
                        :chips-add

                        (< amount (js/BigInt 0))
                        :chips-sub

                        :else
                        :no-update)]
    {:settle-status settle-status,
     :settle-type   settle-type,
     :amount        (u/abs amount),
     :rake          rake}))

(defn merge-settle-map
  [m1 m2]
  (merge-with merge-settle-item m1 m2))

(defn start
  [chain-api game-id input output init-state]
  (a/go-loop [settle-serial  (-> init-state
                                 :game-account-state
                                 :settle-serial)
              acc-settle-map nil
              acc-count      0]
    (let [{:keys [type data]} (a/<! input)]
      (case type
        :system/settle
        (let [{:keys [rake settle-map]} data
              any-leave?     (some #(= :leave (:settle-status %)) (vals settle-map))
              new-count      (inc acc-count)
              new-settle-map (merge-settle-map acc-settle-map settle-map)

              last-state     (a/<! (p/-fetch-game-account
                                    chain-api
                                    game-id
                                    {:settle-serial settle-serial}))]

          (log/log "📥" game-id "New settle, rake: %s" rake)
          (doseq [[pid {:keys [settle-status settle-type amount]}] settle-map]
            (log/log "📥" game-id "-%s %s %s %s" pid settle-status settle-type amount))

          (if (or any-leave? (>= new-count settle-batch-size))
            (let [settle-serial (a/<! (p/-settle chain-api
                                                 game-id
                                                 last-state
                                                 settle-serial
                                                 new-settle-map))]
              (recur settle-serial nil 0))
            (recur settle-serial new-settle-map new-count)))


        :system/set-winner
        (let [{:keys [settle-serial ranking]} data
              last-state    (a/<! (p/-fetch-game-account
                                   chain-api
                                   game-id
                                   {:settle-serial settle-serial}))
              settle-serial (a/<! (p/-set-winner chain-api
                                                 game-id
                                                 last-state
                                                 settle-serial
                                                 ranking))]
          (recur settle-serial acc-settle-map acc-count))

        ;; EXIT
        nil
        (do
          (log/log "💤️" game-id "Sync loop quit")
          (a/close! output))))))

(defrecord Submitter [chain-api input output])

(extend-type Submitter
 ep/IAttachable
 (-input [this]
   (:input this))
 (-output [_this]
   nil)
 (-interest-event-types [_this]
   [:system/settle
    :system/set-winner])

 ep/IWaitable
 (-wait [this]
   (a/go (a/<! (:output this))))

 ep/IComponent
 (-start [this opts]
   (let [{:keys [chain-api input output]} this
         {:keys [game-id init-state]}     opts]
     (log/log "🎉" game-id "Start submitter")
     (start chain-api game-id input output init-state))))

(defn make-submitter
  [chain-api]
  (let [input  (a/chan 4)
        output (a/promise-chan)]
    (->Submitter chain-api input output)))
