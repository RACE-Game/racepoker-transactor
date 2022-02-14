(ns depk.transactor.util
  (:require-macros depk.transactor.util)
  (:require [depk.transactor.log :as log]
            [cljs.core.async :refer [<! >! go-loop chan close!]]
            [depk.transactor.state.config :refer [config]]
            [clojure.walk :as walk]))

(def request-log-ignores
  #{"/api/v1/game/state"})
