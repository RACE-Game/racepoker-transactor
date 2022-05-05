(ns depk.transactor.auth
  "NaCL based auth."
  (:require
   ["buffer" :as buffer]
   ["tweetnacl" :as nacl]
   [solana-clj.publickey :as pubkey]
   [solana-clj.keypair :as keypair]
   [depk.transactor.log :as log]))

;; (defn user-id-fn
;;   [{:keys [params] :as req}]
;;   (log/infof "💫Receive WS connection with params: " (prn-str params))
;;   (let [{:keys [pubkey sig game-id]} params
;;         k   (pubkey/to-buffer (pubkey/make-public-key pubkey))
;;         msg (buffer/Buffer.from
;;              (str pubkey
;;                   " sign with game "
;;                   game-id
;;                   " for RACE Poker."))]
;;     (if (nacl/sign.detached.verify msg (buffer/Buffer.from sig "hex") k)
;;       (do (log/infof "✅Signature check succeed.")
;;           [game-id pubkey])
;;       (do (log/infof "⭕Signature check failed.")
;;           (throw (ex-info "Reject connection" {:reason "Signature check failed"}))))))

(defn user-id-fn
  [{:keys [params]}]
  (log/infof "💫Receive WS connection with params: " (prn-str params))
  (let [{:keys [pubkey sig uuid game-id]} params]
    [game-id pubkey uuid sig]))
