(ns depk.transactor.chain.sync-loop
  "Sychronization for blockchain states."
  (:require
   [cljs.core.async          :as a]
   [depk.transactor.log      :as log]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.util     :as u]
   [clojure.set              :as set]
   [depk.transactor.constant :as c]))

(def sync-loop-event-types
  [:system/settle
   :system/set-winner])

(def settle-batch-size
  "A batch size for settlement."
  5)

(defn merge-settle-item
  [s1 s2]
  (let [settle-status (cond
                        (or (= :leave (:settle-status s1))
                            (= :leave (:settle-status s2)))
                        :leave

                        (or (= :no-update (:settle-status s1))
                            (= :no-update (:settle-status s2)))
                        :no-update

                        :else
                        :empty-seat)

        amount        (+ (* (if (= :chips-sub (:settle-type s1)) (js/BigInt -1) (js/BigInt 1))
                            (:amount s1))
                         (* (if (= :chips-sub (:settle-type s2)) (js/BigInt -1) (js/BigInt 1))
                            (:amount s2)))

        settle-type   (cond
                        (> amount (js/BigInt 0))
                        :chips-add

                        (< amount (js/BigInt 0))
                        :chips-sub

                        :else
                        :no-update)]
    {:settle-status settle-status,
     :settle-type   settle-type,
     :amount        (u/abs amount)}))

(defn merge-settle-map
  [m1 m2]
  (merge-with merge-settle-item m1 m2))

(defn start-sync-loop
  "Fetch game state through chain API, emit :system/sync-state event to game handle."
  [chain-api game-id input output init-game-account-state]
  (log/infof "🏁Start state sync loop for game[%s]" game-id)
  (a/go-loop [last-state     nil
              acc-settle-map nil
              acc-rake       (js/BigInt 0)
              acc-count      0]
    (let [{:keys [buyin-serial]} last-state
          to         (a/timeout 2000)
          [val port] (a/alts! [to input])]
      (condp = port
        ;; No input, fetch new state
        to
        (let [state (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))]
          (log/infof "🥡game[%s] Fetching new state, buyin: %s settle: %s"
                     game-id
                     (:buyin-serial last-state)
                     (:settle-serial last-state))
          (when (not= buyin-serial (:buyin-serial state))
            (log/infof "🥡game[%s] New state, buyin: %s settle: %s"
                       game-id
                       (:buyin-serial state)
                       (:settle-serial state))
            (a/>! output
                  {:type    :system/sync-state,
                   :game-id game-id,
                   :data    {:game-account-state state}}))
          (recur state acc-settle-map acc-rake acc-count))

        ;; Has input, send transaction
        input
        (let [{:keys [type data]} val]
          (condp = type
            :system/settle
            (let [{:keys [settle-map settle-serial rake]} data
                  any-leave?     (some #(= :leave (:settle-status %)) (vals settle-map))
                  acc-count      (inc acc-count)
                  new-settle-map (merge-settle-map acc-settle-map settle-map)
                  acc-rake       (+ acc-rake rake)]
              (log/infof "🔨game[%s] Received settle event: #%s" game-id settle-serial)
              (log/infof "🔨Current settle map: %s" game-id acc-settle-map)
              (log/infof "🔨New settle map: %s" new-settle-map)
              (log/infof "🔨New rake: %s" acc-rake)
              (if (or any-leave? (<= settle-batch-size acc-count))
                (do (log/infof "🔨Sending Settle transaction, serial: %s"
                               (:settle-serial data))
                    (let [state (a/<! (p/-settle chain-api
                                                 game-id
                                                 (:settle-serial data)
                                                 acc-rake
                                                 new-settle-map))]
                      (a/>! output
                            {:type    :system/settle-succeed,
                             :game-id game-id,
                             :data    {:game-account-state state}}))
                    (log/infof "🔨Clear settle accumulator")
                    (recur last-state nil (js/BigInt 0) 0))
                (recur last-state new-settle-map acc-rake acc-count)))

            :system/set-winner
            (do
              (let [state (a/<! (p/-set-winner chain-api
                                               game-id
                                               (:settle-serial data)
                                               (:winner-id data)))]
                (a/>! output
                      {:type    :system/settle-succeed,
                       :game-id game-id,
                       :data    {:game-account-state state}}))
              (recur last-state acc-settle-map acc-rake acc-count))))))))
