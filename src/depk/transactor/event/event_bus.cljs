(ns depk.transactor.event.event-bus
  "A event bus is a channel with multiple sources and destinations.

  An IAttachable struct can attach to event bus, for both consuming and producing."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.event.protocol :as p]
   [depk.transactor.log :as log]))

(defrecord MemEventBus [input output output-mult])

(extend-type MemEventBus
 p/IComponent
 (p/-start [this _opts]
   (let [{:keys [input output]} this]
     (a/go-loop [it (a/<! input)]
       (cond
         (not (:type it))
         (log/errorf "☠️Invalid Event: %s" it)

         (:player-id it)
         (log/infof "🤡Event: %s Player: %s" (:type it) (:player-id it))

         :else
         (log/infof "👽️️Event: %s" (:type it)))
       (a/>! output it)
       (recur (a/<! input))))
   this)

 p/IEventBus
 (p/-shutdown [this]
   (a/close! (:input this)))

 (p/-send [this event]
   (a/put! (:input this) event)))

(defn make-mem-event-bus
  []
  (let [input       (a/chan 10)
        output      (a/chan 10)
        output-mult (a/mult output)]
    (->MemEventBus input output output-mult)))
