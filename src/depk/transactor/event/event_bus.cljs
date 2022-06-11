(ns depk.transactor.event.event-bus
  "A event bus is a channel with multiple sources and destinations.

  An IAttachable struct can attach to event bus, for both consuming and producing."
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.event.protocol :as p]
   [depk.transactor.log :as log]))

(defrecord MemEventBus [input output output-pub])

(extend-type MemEventBus
 p/IComponent
 (p/-start [this _opts]
   (let [{:keys [input output]} this]
     (a/go-loop [it (a/<! input)]
       ;; (log/infof "-->Event: %s" (:type it))
       (if it
         (do
           (a/>! output it)
           (recur (a/<! input)))
         (a/close! output))))
   this)

 p/IEventBus
 (p/-shutdown [this]
   (log/info "💤Shutdown event bus.")
   (a/close! (:input this)))

 (p/-send [this event]
   (a/put! (:input this) event)))

(defn make-mem-event-bus
  []
  (log/infof "🏁Use in-memory event bus")
  (let [input      (a/chan 10)
        output     (a/chan 10)
        output-pub (a/pub output :type)]
    (->MemEventBus input output output-pub)))
