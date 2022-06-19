(ns depk.transactor.game.encrypt
  "Encryption/Decryption in game.

  Naming conventions:
  - card: a vector of [suit-keyword, kind-keyword], example: [:s :a], means Spade Ace
  - card-str: string formatted card, example: \"sa\"
  - cards: vector of cards
  - card-strs: vector of card-str
  - ciphers: vector of arraybuffer, each arraybuffer represent an encrypted card
  - hex: string, hex formatted ciphers, used between programs

  We get ciphers by encrypting card-strs.
  "
  (:require
   #?@(:node
         [["crypto" :as crypto]]
       :cljs
         [])
   ["buffer"             :as buffer]
   [cljs.core.async.interop :refer [<p!]]
   [clojure.string       :as str]
   [depk.transactor.util :refer [go-try go-loop-try <!?]]))

#?(:node
     (def subtle (.. crypto -webcrypto -subtle))
   :cljs
     (def subtle js/crypto.subtle))

(def public-exponent (js/Uint8Array. #js [1 0 1]))

;; length = 16
;; random generated
(defonce aes-counter
         (js/Uint8Array.of 1 166 94 171 100 195 26 180 154 92 57 206 92 165 107 45))

(defonce ^{:doc "Default shuffle AES key, used for init card ciphers."}
         default-shuffle-key
         "909c47683d7d3257dff236f2785d37b2")

(def suits [:s :h :d :c])

(def kinds [:a :2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k])

;; helpers

(defn to-hex
  [array]
  (.toString (buffer/Buffer.from array) "hex"))

(defn to-utf8
  [array]
  (.toString (buffer/Buffer.from array) "UTF-8"))

(defn from-hex
  [hex]
  (js/Uint8Array.from (buffer/Buffer.from hex "hex")))

(defn- gen-rsa-keypair-p
  []
  (.generateKey subtle
                #js
                 {:name           "RSA-OAEP",
                  :modulusLength  1024,
                  :publicExponent public-exponent,
                  :hash           "SHA-256"}
                true
                #js ["wrapKey" "unwrapKey"]))

(defn gen-rsa-keypair
  "Generate RSA [pubkey, privkey]."
  []
  (go-try (let [keypair (<p! (gen-rsa-keypair-p))]
            [(.-publicKey keypair)
             (.-privateKey keypair)])))

(defn gen-aes-key-p
  []
  (.generateKey subtle
                #js
                 {:name    "AES-CTR",
                  :counter aes-counter,
                  :length  128}
                true
                #js ["encrypt" "decrypt"]))

(defn multi-gen-aes-key
  [cnt]
  (go-loop-try [ret []
                n   0]
    (if (= cnt n)
      ret
      (recur (conj ret (<p! (gen-aes-key-p))) (inc n)))))

(defn encrypt-aes-p
  [plain-text aes-key]
  (.encrypt subtle
            #js
             {:name    "AES-CTR",
              :counter aes-counter,
              :length  128}
            aes-key
            (.encode (js/TextEncoder.) plain-text)))

(defn decrypt-aes-p
  [cipher-text aes-key]
  (.decrypt subtle
            #js
             {:name    "AES-CTR",
              :counter aes-counter,
              :length  128}
            aes-key
            cipher-text))

(defn wrap-aes-key-p
  [aes-key rsa-public-key]
  (.wrapKey subtle
            "raw"
            aes-key
            rsa-public-key
            #js
             {:name "RSA-OAEP",
              :hash "SHA-256"}))

(defn unwrap-aes-key-p
  [wrapped-aes-key rsa-private-key]
  (.unwrapKey subtle
              "raw"
              wrapped-aes-key
              rsa-private-key
              #js
               {:name           "RSA-OAEP",
                :modulusLength  1024,
                :publicExponent public-exponent,
                :hash           "SHA-256"}
              #js
               {:name    "AES-CTR",
                :counter aes-counter,
                :length  128}
              true
              #js ["encrypt" "decrypt"]))

(defn export-aes-key
  [key]
  (go-try (to-hex
           (try (<p! (.exportKey subtle "raw" key))
                (catch ExceptionInfo e
                  (throw e))))))

(defn import-aes-key-p
  [raw]
  (.importKey subtle
              "raw"
              (from-hex raw)
              #js {:name "AES-CTR"}
              true
              #js ["encrypt" "decrypt"]))

(defn import-aes-key
  [raw]
  (go-try (<p! (import-aes-key-p raw))))

(defn import-rsa-public-key-p
  [raw]
  (.importKey subtle
              "spki"
              raw
              #js {:name "RSA-OAEP", :hash "SHA-256"}
              true
              #js ["wrapKey"]))

(defn get-default-shuffle-key-p
  []
  (.importKey subtle
              "raw"
              (from-hex default-shuffle-key)
              #js {:name "AES-CTR"}
              true
              #js ["encrypt" "decrypt"]))

(def default-deck-of-cards
  (for [s suits
        k kinds]
    [s k]))

(defn cards->card-strs
  [cards]
  (->> (for [[s k] cards]
         (str (name s) (name k)))
       (into [])))

(defn card-strs->cards
  [card-strs]
  (->> (for [s (partition 2 card-strs)]
         [(keyword (subs s 0 1)) (keyword (subs s 1 2))])
       (into [])))

(defn decrypted-ciphers->cards
  [ciphers]
  (->> (for [c ciphers]
         (if (= :unknown c)
           :unknown
           (let [[s k] (to-utf8 c)]
             [(keyword s) (keyword k)])))
       (into [])))

(defn ciphers->hex
  [ciphers]
  (->> (for [c ciphers]
         (to-hex c))
       (str/join "-")))

(defn hex->ciphers
  [hex]
  (->> (str/split hex #"-")
       (mapv from-hex)))

;; public apis

(defn encrypt-ciphers-with-keys
  "Encrypt ciphers with keys, the length of keys should equal to the length of ciphers.

  * ciphers [ArrayBuffer]
  * keys [CryptoKey]"
  [ciphers keys]
  {:pre [(<= (count ciphers) (count keys))]}
  (go-loop-try [[c & cs] ciphers
                [k & ks] keys
                ret      []]
    (if-not c
      ret
      (let [e (<p! (encrypt-aes-p c k))]
        (recur cs ks (conj ret e))))))

(defn decrypt-ciphers-with-keys
  "Decrypt ciphers with keys, the length of keys should equal to the length of ciphers.
  Use keyword :unknown to represent an unknown cipher / key.

  * ciphers [ArrayBuffer]
  * keys [CryptoKey]"
  [ciphers keys]
  {:pre [(<= (count ciphers) (count keys))]}
  ;; (println ">>>keys:" keys)
  (go-loop-try [[c & cs] ciphers
                [k & ks] keys
                ret      []]
    (if-not c
      ret
      (let [e (if (or (= :unknown c)
                      (= :unknown k))
                :unknown
                (<p! (decrypt-aes-p c k)))]
        (recur cs ks (conj ret e))))))

(defn decrypt-ciphers-with-keys-2d
  [ciphers keys-2d]
  (go-loop-try [ciphers    ciphers
                [ks & kss] keys-2d]
    (if-not ks
      ciphers
      (recur (<!? (decrypt-ciphers-with-keys ciphers ks)) kss))))

(defn encrypt-ciphers-with-default-shuffle-key
  [ciphers]
  (go-try
   (let [key (<p! (get-default-shuffle-key-p))]
     (<!? (encrypt-ciphers-with-keys ciphers (repeat (count ciphers) key))))))

(defn decrypt-ciphers-with-default-shuffle-key
  [ciphers]
  (go-try
   (let [key (<p! (get-default-shuffle-key-p))]
     (<!? (decrypt-ciphers-with-keys ciphers (repeat (count ciphers) key))))))

(defn gen-random-shuffle-vector
  "Generate a random shuffle-vector."
  []
  (->> (shuffle (range 52))
       (into [])))

(defn shuffle-cards
  "Shuffle cards, return {:keys [ciphers, key]}.

  shuffle-vec is a vector of shuffled original index(0 to 51)."
  [ciphers shuffle-vec]
  (go-try
   (let [key     (<p! (gen-aes-key-p))
         ciphers (map #(nth ciphers %) shuffle-vec)
         ciphers (<!? (encrypt-ciphers-with-keys ciphers (repeat (count ciphers) key)))]
     [ciphers key])))

(defn encrypt-cards-and-remove-shuffle-key
  "Encrypt cards, return [ciphers, keys]."
  [ciphers shuffle-key]
  (go-try
   (let [cnt     (count ciphers)
         keys    (<!? (multi-gen-aes-key cnt))
         ciphers (<!? (decrypt-ciphers-with-keys ciphers (repeat cnt shuffle-key)))
         ciphers (<!? (encrypt-ciphers-with-keys ciphers keys))]
     [ciphers keys])))

(defn encrypt-share-keys
  "Encrypt share key with RSA public key."
  [share-keys rsa-public-key]
  (go-loop-try [[k & ks] share-keys
                ret      []]
    (if-not k
      ret
      (let [w (<p! (wrap-aes-key-p k rsa-public-key))]
        (recur ks (conj ret w))))))

(defn decrypt-share-keys
  "Decrypt share key with RSA private key."
  [wrapped-keys rsa-private-key]
  (go-loop-try [[w & ws] wrapped-keys
                ret      []]
    (if-not w
      ret
      (let [k (<p! (unwrap-aes-key-p w rsa-private-key))]
        (recur ws (conj ret k))))))

(comment

  (go-try (let [ka0       (<p! (import-aes-key-p "24918f4ffc066105629863d68a92596b"))
                ka1       (<p! (import-aes-key-p "c35ad7e22a9a7164e7980ad3c16b39c7"))
                ka2       (<p! (import-aes-key-p "69478fb7684ca45644630ce173c76200"))
                ka3       (<p! (import-aes-key-p "294dfbaeb33849ab44689163a335ebcc"))

                kb0       (<p! (import-aes-key-p "af65251070d5a0386b2e7c97881a978c"))
                kb1       (<p! (import-aes-key-p "72dc6cfefd32565c610b5cd892bc1f7d"))
                kb2       (<p! (import-aes-key-p "d5f9ea1a7f4c0ca3eab9e239cea0ae8f"))
                kb3       (<p! (import-aes-key-p "867f57da487c4f9672359c70ac528271"))


                keys-a    [ka0 ka1 ka2 ka3]
                keys-b    [kb0 kb1 kb2 kb3]

                h         "563a-e6cb-da47-2a7c"

                ciphers   (hex->ciphers h)

                decrypted (<!? (decrypt-ciphers-with-keys-2d ciphers [keys-a keys-b]))]

            (println (ciphers->hex decrypted))))


  (go-try
   (let [ciphers   (<!? (cards->card-strs [[:s :a]
                                           [:s :k]
                                           [:s :q]
                                           [:d :q]]))
         decrypted (<!? (decrypt-ciphers-with-default-shuffle-key ciphers))]
     (prn)
     (println (ciphers->hex ciphers))
     (println (ciphers->hex decrypted))

   ))

  (go-try
   (let [ka0       (<p! (import-aes-key-p "24918f4ffc066105629863d68a92596b"))
         ka1       (<p! (import-aes-key-p "c35ad7e22a9a7164e7980ad3c16b39c7"))
         ka2       (<p! (import-aes-key-p "69478fb7684ca45644630ce173c76200"))
         ka3       (<p! (import-aes-key-p "294dfbaeb33849ab44689163a335ebcc"))

         kb0       (<p! (import-aes-key-p "af65251070d5a0386b2e7c97881a978c"))
         kb1       (<p! (import-aes-key-p "72dc6cfefd32565c610b5cd892bc1f7d"))
         kb2       (<p! (import-aes-key-p "d5f9ea1a7f4c0ca3eab9e239cea0ae8f"))
         kb3       (<p! (import-aes-key-p "867f57da487c4f9672359c70ac528271"))


         keys-a    [ka0 ka1 ka2 ka3]
         keys-b    [kb0 kb1 kb2 kb3]

         origin    (cards->card-strs [[:s :a]
                                      [:s :k]
                                      [:s :q]
                                      [:d :q]])

         ;; [ka0 ka1 ka2 ka3 :as keys-a] (<!? (multi-gen-aes-key 4))
         ;; [kb0 kb1 kb2 kb3 :as keys-b] (<!? (multi-gen-aes-key 4))

         encrypted (<!? (encrypt-ciphers-with-keys origin keys-a))
         encrypted (<!? (encrypt-ciphers-with-keys encrypted keys-b))

         decrypted (<!? (decrypt-ciphers-with-keys-2d encrypted [keys-a keys-b]))]

     ;; (println "cards:" (card-strs->cards origin))
     (println "--------------------------------")
     (println "origin:" origin)
     (println "encrypted:" (ciphers->hex encrypted))
     (println "decrypted:" (ciphers->hex decrypted))
     (println "decrypted cards:" (decrypted-ciphers->cards decrypted))
     (println (<!? (export-aes-key ka0)))
     (println (<!? (export-aes-key ka1)))
     (println (<!? (export-aes-key ka2)))
     (println (<!? (export-aes-key ka3)))
     (println "--------")
     (println (<!? (export-aes-key kb0)))
     (println (<!? (export-aes-key kb1)))
     (println (<!? (export-aes-key kb2)))
     (println (<!? (export-aes-key kb3)))
     (println "--------------------------------"))))
