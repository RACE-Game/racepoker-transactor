(ns depk.transactor.auth
  "NaCL based auth."
  (:require
   ["buffer"    :as buffer]
   ["tweetnacl" :as nacl]
   [solana-clj.publickey :as pubkey]))

(defn user-id-fn
  [{:keys [params]}]
  (println "Receive WS connection with params: " (prn-str params))
  (let [{:keys [pubkey sig game-id]} params
        k   (pubkey/to-buffer (pubkey/make-public-key pubkey))
        msg (buffer/Buffer.from
             (str pubkey
                  " sign with game "
                  game-id
                  " for RACE Poker."))]
    (if (nacl/sign.detached.verify msg (buffer/Buffer.from sig "hex") k)
      (do (println "Signature check succeed.")
          [game-id pubkey])
      (println "Signature check failed."))))
