(ns depk.transactor.tournament-game.handle
  "Game handle is used to control a set of components of a game."
  (:require
   [cljs.core.async :as a]
   [depk.transactor.tournament-game.submitter :as submitter]
   [depk.transactor.game.models :as m]
   [depk.transactor.event :as event]
   [depk.transactor.game.broadcaster :as broadcaster]
   [depk.transactor.game.event-loop :as eloop]
   [depk.transactor.log :as log]))

(defrecord TournamentGameHandle
  [event-bus
   submitter
   broadcaster
   event-loop])

(defn make-game-handle
  [game-id players size blinds-mode post-msg-fn]
  (log/log "🎉" game-id "Create game handle")
  (a/go
   (let [game-account-state {:players         players,
                             ;; Wait 1 minute before the real start.
                             :start-time      nil,
                             :game-type       :tournament,
                             :buyin-serial    1,
                             :settle-serial   1,
                             :size            size,
                             :sb              (js/BigInt 50),
                             :bb              (js/BigInt 100),
                             :ante            (js/BigInt 0),
                             :base-sb         (js/BigInt 50),
                             :base-bb         (js/BigInt 100),
                             :base-ante       (js/BigInt 0),
                             :transactor-rake (js/BigInt 0),
                             :owner-rake      (js/BigInt 0)}

         mint-info          {:decimals 0}
         init-state         (m/make-game-state game-account-state
                                               mint-info
                                               {:game-id     game-id,
                                                :blinds-mode blinds-mode,
                                                :halt?       true})
         opts               {:game-id game-id, :init-state init-state}
         event-bus          (event/make-mem-event-bus)
         submitter          (submitter/make-tournament-game-submitter post-msg-fn)
         broadcaster        (broadcaster/make-game-broadcaster post-msg-fn)
         event-loop         (eloop/make-event-loop)]
     ;; Attach components to event bus
     (event/attach event-loop event-bus)
     (event/attach submitter event-bus)
     (event/attach broadcaster event-bus)
     ;; Start components
     (event/start-component event-bus opts)
     (event/start-component submitter opts)
     (event/start-component event-loop opts)
     (event/start-component broadcaster opts)

     (log/log "🎉" game-id "Game handle started")
     (->TournamentGameHandle event-bus
                             submitter
                             broadcaster
                             event-loop))))

(defn send-event
  [game-handle event]
  (event/send (:event-bus game-handle) event))

(defn shutdown
  [game-handle]
  (event/shutdown (:event-bus game-handle)))

(defn wait
  [game-handle]
  (event/wait (:submitter game-handle)))
