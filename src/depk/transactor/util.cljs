(ns depk.transactor.util
  (:require-macros depk.transactor.util)
  (:require [depk.transactor.log :as log]
            [cljs.core.async :refer [<! >! go-loop chan close!]]
            [depk.transactor.state.config :refer [config]]
            [clojure.walk :as walk]))

(def request-log-ignores
  #{"/api/v1/game/state"})

(defn merge-orderly
  [chs]
  (let [out (chan (count chs))]
    (go-loop [[ch & chs] chs
              v []]
      (if ch
        (recur chs (conj v (<! ch)))
        (>! out v)))
    out))

(defn merge-chs-orderly
  [chs]
  (let [out (chan (count chs))]
    (go-loop [[ch & chs] chs]
      (if-let [it (when ch (<! ch))]
        (do
          (>! out it)
          (recur chs))
        (close! out)))
    out))
