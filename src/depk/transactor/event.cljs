(ns depk.transactor.event
  (:require [cljs.core.async :as a]
            [depk.transactor.event.protocol :as p]
            [depk.transactor.event.event-bus :as ebus]
            [depk.transactor.event.specs]))

(defn attach
  "Attach to event bus.

  Will create the connection with bus for both consuming and producing."
  [attachable ebus]
  (let [input (p/-input attachable)
        output (p/-output attachable)]
    (when input
      (a/tap (:output-mult ebus) input))
    (when output
      (a/pipe output (:input ebus)))))

(defn start-component
  [component opts]
  (p/-start component opts))

(defn shutdown
  [ebus]
  (p/-shutdown ebus))

(defn send
  [ebus event]
  (p/-send ebus event))

(defn make-mem-event-bus []
  (ebus/make-mem-event-bus))