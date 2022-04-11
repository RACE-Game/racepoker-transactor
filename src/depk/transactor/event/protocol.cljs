(ns depk.transactor.event.protocol)

(defprotocol IAttachable
  (-input [this])
  (-output [this]))

(defprotocol IEventBus
  (-shutdown [this])
  (-send [this event]))

(defprotocol IComponent
  (-start [this opts]))
