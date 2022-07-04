(ns depk.transactor.chain.submitter
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
  [chain-api game-id input init-state]
  (a/go-loop [settle-serial  (:settle-serial init-state)
              acc-settle-map nil
              acc-count      0]
    (let [{:keys [type data], :as event} (a/<! input)]
      (if event
        (condp = type
          :system/settle
          (let [{:keys [rake settle-map]} data
                any-leave?     (some #(= :leave (:settle-status %)) (vals settle-map))
                new-count      (inc acc-count)
                new-settle-map (merge-settle-map acc-settle-map settle-map)

                last-state     (a/<! (p/-fetch-game-account
                                      chain-api
                                      game-id
                                      {:settle-serial settle-serial}))]

            (log/infof "📥New settle, rake: %s" rake)
            (doseq [[pid {:keys [settle-status settle-type amount]}] settle-map]
              (log/infof "📥- %s %s %s %s" pid settle-status settle-type amount))

            (if (or any-leave? (>= new-count settle-batch-size))
              (let [_ (a/<! (p/-settle chain-api
                                       game-id
                                       last-state
                                       settle-serial
                                       new-settle-map))]
                (recur (inc settle-serial) nil 0))
              (recur settle-serial new-settle-map new-count)))

          :system/set-winner
          (let [{:keys [settle-serial ranking]} data
                last-state (a/<! (p/-fetch-game-account
                                  chain-api
                                  game-id
                                  {:settle-serial settle-serial}))
                _ (a/<! (p/-set-winner chain-api
                                       game-id
                                       last-state
                                       settle-serial
                                       ranking))]
            (recur (inc settle-serial) acc-settle-map acc-count)))
        ;; EXIT
        (log/infof "💤️Sync loop quit for game[%s]" game-id)))))

(defrecord Submitter [chain-api input])

(extend-type Submitter
 ep/IAttachable
 (-input [_this]
   nil)
 (-output [this]
   (:output this))
 (-interest-event-types [_this]
   [:system/settle
    :system/set-winner])

 ep/IComponent
 (-start [this opts]
   (let [{:keys [chain-api input]}    this
         {:keys [game-id init-state]} opts]
     (log/infof "🏁Start submitter for game[%s]" game-id)
     (start chain-api game-id input init-state))))

(defn make-submitter
  [chain-api]
  (let [input (a/chan)]
    (->Submitter chain-api input)))
