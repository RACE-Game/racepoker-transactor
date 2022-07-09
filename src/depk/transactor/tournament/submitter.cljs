(ns depk.transactor.tournament.submitter
  (:require
   [cljs.core.async     :as a]
   [depk.transactor.chain.protocol :as p]
   [depk.transactor.event.protocol :as ep]
   [depk.transactor.log :as log]))

(defn start
  [chain-api tournament-id input init-state]
  (log/infof "🏁Start submitter for tournament[%s]" tournament-id)
  (a/go-loop [settle-serial (:settle-serial init-state)]
    (let [event (a/<! input)]
      (log/infof "⬆️Submitter receives event: %s" event)
      (if event
        (let [{:keys [type data]} event]
          (case type
            :system/submit-start-tournament
            (let [last-state    (a/<! (p/-fetch-tournament-account
                                       chain-api
                                       tournament-id
                                       {:settle-serial  settle-serial,
                                        :without-ranks? true}))

                  settle-serial (a/<! (p/-start-tournament chain-api
                                                           tournament-id
                                                           last-state
                                                           settle-serial))]

              (recur settle-serial))

            :system/settle-tournament
            (let [{:keys [ranks]} data
                  last-state      (a/<! (p/-fetch-tournament-account
                                         chain-api
                                         tournament-id
                                         {:settle-serial  settle-serial,
                                          :without-ranks? true}))]
              (a/<! (p/-settle-tournament chain-api
                                          tournament-id
                                          last-state
                                          settle-serial
                                          ranks)))))
        (log/infof "💤️Submitter quit for tournament[%s]" tournament-id)))))

(defrecord TournamentSubmitter [chain-api input])

(extend-type TournamentSubmitter
 ep/IAttachable
 (-input [this]
   (:input this))
 (-output [_this]
   nil)
 (-interest-event-types [_this]
   [:system/settle-tournament
    :system/submit-start-tournament])

 ep/IComponent
 (-start [this opts]
   (let [{:keys [chain-api input]} this
         {:keys [tournament-id init-state]} opts]
     (start chain-api tournament-id input init-state))))

(defn make-tournament-submitter
  [chain-api]
  (let [input (a/chan)]
    (->TournamentSubmitter chain-api input)))
