(ns depk.transactor.event.specs
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::type
  #{
    ;; broadcaster
    :system/broadcast-state
    ;; store api
    :system/save-game-history
    ;; chain api
    :system/settle-finished
    :system/settle-failed
    ;; chain api: from sync loop
    :system/sync-state
    })
