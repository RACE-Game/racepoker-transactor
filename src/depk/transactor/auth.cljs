(ns depk.transactor.auth
  "NaCL based auth."
  (:require
   ["buffer"            :as buffer]
   ["tweetnacl"         :as nacl]
   [solana-clj.publickey :as pubkey]
   [solana-clj.keypair  :as keypair]
   [depk.transactor.log :as log]
   [goog.string         :as gstr]))

(def sign-tmpl
  (str
   "I request to attach to game %s\n"
   "Wallet address: %s\n"
   "RSA: %s\n"
   "ED25519: %s"))

(defn user-id-fn
  [{:keys [params], :as _req}]
  (log/infof "💫Receive WS connection with params: %s" (prn-str params))
  (let
    [{:keys [pubkey rsa-pub ed-pub sig game-id]} params
     k   (pubkey/to-buffer (pubkey/make-public-key pubkey))
     msg
     (buffer/Buffer.from
      (gstr/format sign-tmpl game-id pubkey rsa-pub ed-pub sig))]
    (if (nacl/sign.detached.verify msg (buffer/Buffer.from sig "hex") k)
      (do (log/infof "✅Signature check succeed.")
          [game-id pubkey rsa-pub ed-pub sig])
      (do (log/infof "⭕Signature check failed.")
          (throw (ex-info "Reject connection" {:reason "Signature check failed"}))))))
