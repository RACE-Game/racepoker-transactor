(ns depk.transactor.chain.misc)

(defn merge-events
  "Merge multiple settlement events into one."
  [events]
  (->> events
       (reduce (fn [req1 req2]
                 (let [{chips-change-map-1  :chips-change-map,
                        player-status-map-1 :player-status-map}
                       req1

                       {chips-change-map-2  :chips-change-map,
                        player-status-map-2 :player-status-map}
                       req2

                       chips-change-map (merge-with (fnil + (js/BigInt 0) (js/BigInt 0))
                                                    chips-change-map-1
                                                    chips-change-map-2)

                       player-status-map (merge-with (fn [s1 s2]
                                                       (cond
                                                         (#{s1 s2} :leave)   :leave
                                                         (#{s1 s2} :dropout) :dropout
                                                         :else               :normal))
                                                     player-status-map-1
                                                     player-status-map-2)]
                   {:chips-change-map  chips-change-map,
                    :player-status-map player-status-map})))))
