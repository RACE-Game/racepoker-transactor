(ns depk.transactor.auth
  "NaCL based auth."
  (:require
   ["buffer"            :as buffer]
   ["tweetnacl"         :as nacl]
   [solana-clj.publickey :as pubkey]
   [depk.transactor.log :as log]
   [goog.string         :as gstr]))

(def sign-tmpl
  (str
   "Race Protocol\n"
   "A decentralized protocol for competitive games.\n"
   "This signature will be used for authentication of encrypted data.\n\n"
   "Player: \n%s\n"
   "Encryption Key: \n%s\n"
   "Verification Key: \n%s\n"))

(defn user-id-fn
  [{:keys [params], :as _req}]
  (let
    [{:keys [pubkey rsa-pub ed-pub sig game-id]} params
     k   (pubkey/to-buffer (pubkey/make-public-key pubkey))
     msg
     (buffer/Buffer.from
      (gstr/format sign-tmpl pubkey rsa-pub ed-pub sig))]
    (if (nacl/sign.detached.verify msg (buffer/Buffer.from sig "hex") k)
      (do (log/log "🤝" game-id "Receive valid connecting request from player[%s]" pubkey)
          (println ed-pub)
          [game-id pubkey rsa-pub ed-pub sig])
      (do (log/log "⭕" game-id "Receive invalid connecting request from player[%s]" pubkey)
          (throw (ex-info "Reject connection" {:reason "Signature check failed"}))))))
