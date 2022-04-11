(ns depk.transactor.util
  (:require-macros depk.transactor.util)
  (:require
   ;; [depk.transactor.log :as log]
   [cljs.core.async   :refer [<! >! go-loop chan close!]]
   [clojure.walk      :as walk]
   [cognitect.transit :as transit]
   [taoensso.sente.packers.transit :as sente-transit]))

(def request-log-ignores
  #{"/api/v1/game/state"})

(def bigint-writer
  (transit/write-handler
   (constantly "n")
   (fn [v] (str v))))

(def bigint-reader
  (transit/read-handler
   (fn [t] (js/BigInt t))))

(def sente-packer
  (sente-transit/->TransitPacker
   :json
   {:handlers {js/BigInt bigint-writer}}
   {:handlers {"n" bigint-reader}}))

(defn abs
  "Abs for js/BigInt."
  [x]
  (if (> x (js/BigInt 0))
    x
    (- x)))
