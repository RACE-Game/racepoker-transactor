(ns depk.transactor.util
  (:refer-clojure :exclude [abs])
  (:require-macros depk.transactor.util)
  (:require
   ["buffer"             :as buffer]
   ["tweetnacl"          :as nacl]
   [clojure.string       :as str]
   [cognitect.transit    :as transit]
   [depk.transactor.log]
   [solana-clj.publickey :as pubkey]
   [taoensso.sente.packers.transit :as sente-transit]
   [goog.string          :refer [format]]))

(def request-log-ignores
  #{"/api/v1/game/state"})

(defn current-unix-timestamp
  []
  (quot (.getTime (js/Date.)) 1000))

(def bigint-writer
  (transit/write-handler
   (constantly "n")
   (fn [v] (str v))))

(def bigint-reader
  (transit/read-handler
   (fn [t] (js/BigInt t))))

(def transit-writer-opts
  {:handlers {js/BigInt bigint-writer}})

(def transit-reader-opts
  {:handlers {"n" bigint-reader}})

(def sente-packer
  (sente-transit/->TransitPacker
   :json
   transit-writer-opts
   transit-reader-opts))

(def transit-writer
  (transit/writer :json transit-writer-opts))

(def transit-reader
  (transit/reader :json transit-reader-opts))

(defn transit-write
  [o]
  (transit/write transit-writer o))

(defn transit-read
  [o]
  (transit/read transit-reader o))

(defn abs
  "Abs for js/BigInt."
  [x]
  (if (> x (js/BigInt 0))
    x
    (- x)))

(defn verify-signature
  [message signed-message pubkey]
  (let [msg-buf    (.encode (js/TextEncoder.) message)
        signed-buf (buffer/Buffer.from signed-message "hex")
        key-buf    (pubkey/to-buffer (pubkey/make-public-key pubkey))]
    (when-not (nacl/sign.detached.verify msg-buf signed-buf key-buf)
      (throw (ex-info "Invalid signature" {})))))

(defn verify-signature2
  [message signed-message ed-pub-hex]
  (let [msg-buf    (.encode (js/TextEncoder.) message)
        signed-buf (buffer/Buffer.from signed-message "hex")
        key-buf    (buffer/Buffer.from ed-pub-hex "hex")]
    (when-not (nacl/sign.detached.verify msg-buf signed-buf key-buf)
      (throw (ex-info "Invalid signature" {})))))

(defn register-global-error-handler!
  [label]
  (.on js/process
       "uncaughtException"
       (fn [err]
         (js/console.error
          err
          (format "[%s]There was an uncaught error" label)))))

(defn tournament-game-id?
  [id]
  (str/includes? id "#"))

(defn same-message-channel?
  [id1 id2]
  (or (= id1 id2)
      (and (tournament-game-id? id1)
           (tournament-game-id? id2)
           (= (first (str/split id1 #"#"))
              (first (str/split id2 #"#"))))))
