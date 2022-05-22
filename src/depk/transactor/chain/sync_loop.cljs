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

(defn outdated-state?
  [ctx state]
  (let [{:keys [curr-settle-serial curr-buyin-serial]} ctx]
    (or (< (:settle-serial state) curr-settle-serial)
        (< (:buyin-serial state) curr-buyin-serial))))

(defn start-sync-loop
  "Sync transactor state with on-chain state."
  [chain-api game-id input output init-state]
  (log/infof "🏁Start state sync loop for game[%s]" game-id)

  ;; Sync joined players
  (a/go-loop [buyin-serial (:buyin-serial init-state)]
    (let [state (a/<! (p/-fetch-game-account chain-api game-id {:commitment "finalized"}))]

      (when (and state (< buyin-serial (:buyin-serial state)))
        (log/infof "👀️Read game[%s] state, %s -> %s" game-id buyin-serial (:buyin-serial state))
        (a/>! output
              {:type :system/sync-state, :game-id game-id, :data {:game-account-state state}}))
      (a/<! (a/timeout 3000))
      (recur (max buyin-serial (:buyin-serial state)))))

  ;; Sync player chips, status
  (a/go-loop [settle-serial  (:settle-serial init-state)
              acc-rake       (js/BigInt 0)
              acc-settle-map nil
              acc-count      0]
    (let [{:keys [type data]} (a/<! input)]
      (condp = type
        :system/settle
        (let [{:keys [rake settle-map]} data
              any-leave?     (some #(= :leave (:settle-status %)) (vals settle-map))
              new-count      (inc acc-count)
              new-settle-map (merge-settle-map acc-settle-map settle-map)
              new-rake       (+ acc-rake rake)
              last-state     (a/<! (p/-fetch-game-account
                                    chain-api
                                    game-id
                                    {:settle-serial settle-serial}))]

          (log/infof "⚖️New settle, rake: %s" rake)
          (doseq [[pid {:keys [settle-status settle-type amount]}] settle-map]
            (log/infof "⚖️- %s %s %s %s" pid settle-status settle-type amount))

          (if (or any-leave? (>= new-count settle-batch-size))
            (let [_ (a/<! (p/-settle chain-api
                                     game-id
                                     last-state
                                     settle-serial
                                     new-rake
                                     new-settle-map))]
              (recur (inc settle-serial) (js/BigInt 0) nil 0))
            (recur settle-serial new-rake new-settle-map new-count)))

        :system/set-winner
        (let [{:keys [settle-serial winner-id]} data
              last-state (a/<! (p/-fetch-game-account
                                chain-api
                                game-id
                                {:settle-serial settle-serial}))
              _ (a/<! (p/-set-winner chain-api
                                     game-id
                                     last-state
                                     settle-serial
                                     winner-id))]
          (recur (inc settle-serial) acc-rake acc-settle-map acc-count))))))



(comment

  (-> (merge-settle-map {"1" {:settle-status :no-update,
                              :settle-type   :chips-add,
                              :amount        (js/BigInt 400)},
                         "2" {:settle-status :no-update,
                              :settle-type   :chips-sub,
                              :amount        (js/BigInt 400)}}
                        {"1" {:settle-status :leave,
                              :settle-type   :chips-add,
                              :amount        (js/BigInt 400)},
                         "2" {:settle-status :no-update,
                              :settle-type   :chips-sub,
                              :amount        (js/BigInt 400)}})
      (merge-settle-map {"2" {:settle-status :leave,
                              :settle-type   :no-update,
                              :amount        (js/BigInt 400)}})))
