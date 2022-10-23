(ns depk.transactor.state.reg-center
  "Registration center synchronizer."
  (:require
   [cljs.core.async :as a]
   [depk.transactor.tournament :as tournament]
   [depk.transactor.state.worker-manager :as worker-manager]
   [depk.transactor.state.config :as config]
   [depk.transactor.chain :as chain]
   [depk.transactor.util :as u]
   [depk.transactor.log :as log]
   [mount.core :as mount]))

(defn start-reg-center
  []
  (let [chain-api (chain/make-solana-api)
        reg-center-address (get @config/config :reg-center-address)]
    (log/log "🌐" nil "Reg center address: %s" reg-center-address)
    (a/go-loop []
      (let [tournaments (a/<! (chain/fetch-tournament-list chain-api reg-center-address))
            now         (u/current-unix-timestamp)]

        (doseq [{:keys [pubkey start-time]} tournaments
                :when (and (>= now (- start-time 600))
                           (<= now (+ start-time 600)))]

          (tournament/launch-tournament @worker-manager/worker-manager (str pubkey))))
      (a/<! (a/timeout 10000))
      (recur))))

(mount/defstate reg-center
  :start
  (start-reg-center))
