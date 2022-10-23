(ns depk.transactor.event.protocol)

(defprotocol IAttachable
  (-input [this])
  (-output [this])
  (-interest-event-types [this]))

(defprotocol IEventBus
  (-shutdown [this])
  (-send [this event]))

(defprotocol IComponent
  (-start [this opts]))

(defprotocol IWaitable
  (-wait [this]))
