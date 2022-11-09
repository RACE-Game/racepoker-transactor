(ns depk.transactor.game.encrypt-test
  (:require
   [depk.transactor.game.encrypt :as sut]
   [cljs.core.async :refer [go <!]]
   [cljs.core.async.interop :refer [<p!]]
   [cljs.test :as t
              :include-macros true]))

(def shuffle-vec [3 0 1 2])
(def deck-of-cards [[:s :a] [:s :k] [:s :q] [:s :j]])

(t/deftest encrypt-and-decrypt
  (t/async done
    (go
     (let [ciphers   (sut/cards->card-strs deck-of-cards)

           keys      (<! (sut/multi-gen-aes-key 4))

           encrypted (<! (sut/encrypt-ciphers-with-keys ciphers keys))

           decrypted (<! (sut/decrypt-ciphers-with-keys encrypted keys))]
       (t/is (= (sut/ciphers->hex ciphers)
                (sut/ciphers->hex decrypted))))
     (done))))

(t/deftest encrypt-and-decrypt-in-different-orders
  (t/async done
    (go
     (let [ciphers        (sut/cards->card-strs deck-of-cards)

           keys-a         (<! (sut/multi-gen-aes-key 4))

           keys-b         (<! (sut/multi-gen-aes-key 4))

           encrypted-temp (<! (sut/encrypt-ciphers-with-keys ciphers keys-a))
           encrypted      (<! (sut/encrypt-ciphers-with-keys encrypted-temp keys-b))

           decrypted      (<! (sut/decrypt-ciphers-with-keys-2d encrypted [keys-b keys-a]))]
       (t/is (= (str (sut/ciphers->hex ciphers))
                (sut/ciphers->hex decrypted))))
     (done))))

(t/deftest wrap-and-unwrap-keys
  (t/async done
    (go
     (let [ciphers          (sut/cards->card-strs deck-of-cards)

           [pubkey privkey] (<! (sut/gen-rsa-keypair))

           origin           (<! (sut/multi-gen-aes-key 4))

           wrapped          (<! (sut/encrypt-share-keys origin pubkey))
           unwrapped        (<! (sut/decrypt-share-keys wrapped privkey))

           encrypted        (<! (sut/encrypt-ciphers-with-keys ciphers origin))

           decrypted        (<! (sut/decrypt-ciphers-with-keys encrypted unwrapped))]
       (t/is (= (sut/ciphers->hex ciphers)
                (sut/ciphers->hex decrypted))))
     (done))))

(t/deftest import-and-export-key
  (t/async done
    (go
     (let [ciphers   (sut/cards->card-strs [[:h :k]])

           key       (<p! (sut/gen-aes-key-p))

           exported  (<! (sut/export-aes-key key))

           imported  (<p! (sut/import-aes-key-p exported))

           encrypted (<! (sut/encrypt-ciphers-with-keys ciphers [key]))

           decrypted (<! (sut/decrypt-ciphers-with-keys encrypted [imported]))]
       (t/is (= (sut/ciphers->hex ciphers)
                (sut/ciphers->hex decrypted))))
     (done)))

  (t/async done
    (go
     (let [ciphers     (sut/cards->card-strs [[:h :k]])

           key         (<p! (sut/gen-aes-key-p))

           exported    (<! (sut/export-aes-key key))

           imported-1  (<p! (sut/import-aes-key-p exported))
           imported-2  (<p! (sut/import-aes-key-p exported))

           encrypted   (<! (sut/encrypt-ciphers-with-keys ciphers [key]))

           decrypted-1 (<! (sut/decrypt-ciphers-with-keys encrypted [imported-1]))
           decrypted-2 (<! (sut/decrypt-ciphers-with-keys encrypted [imported-2]))]
       (t/is (= (sut/ciphers->hex ciphers)
                (sut/ciphers->hex decrypted-1)
                (sut/ciphers->hex decrypted-2))))
     (done))))

(t/deftest shuffle-and-encrypt-cards

  ;; simulating all the encryption/decryption in a game
  ;; assuming there are two players: A and B
  ;; assuming both player shuffle by 0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0
  ;; ---
  ;; start from transactor, init ciphers which encrypted by the default shuffle key
  ;; player A shuffle and encrypt with its shuffle key: shuffle-key-a
  ;; player B shuffle and encrypt with its shuffle key: shuffle-key-b
  ;; player A remove its shuffle key, then encrypt each cards with keys-a
  ;; player B remove its shuffle key, then encrypt each cards with keys-b
  ;; transactor remove the default shuffle key
  ;; ----
  ;; player A get the 1st and 2nd cards
  ;; player B get the 3rd and 4th cards

  (t/async done
    (go
     (let [;; create init data
           ciphers          (sut/cards->card-strs deck-of-cards)
           ;; transactor encrypt with default shuffle key
           ciphers          (<! (sut/encrypt-ciphers-with-default-shuffle-key ciphers))
           ;; player A shuffle
           [ciphers shuffle-key-a] (<! (sut/shuffle-cards ciphers shuffle-vec))
           ;; player B shuffle
           [ciphers shuffle-key-b] (<! (sut/shuffle-cards ciphers shuffle-vec))
           ;; player A remove shuffle key, encrypt cards
           [ciphers keys-a] (<! (sut/encrypt-cards-and-remove-shuffle-key ciphers shuffle-key-a))
           ;; player B remove shuffle key, encrypt cards
           [ciphers keys-b] (<! (sut/encrypt-cards-and-remove-shuffle-key ciphers shuffle-key-b))
           ;; transactor remove the default shuffle key
           ciphers          (<! (sut/decrypt-ciphers-with-default-shuffle-key ciphers))
           ;; ciphers as hex
           hex              (sut/ciphers->hex ciphers)
           ;; player A get the 1st and 2nd
           ciphers          (sut/hex->ciphers hex)
           cards-a          (-> (<! (sut/decrypt-ciphers-with-keys-2d ciphers
                                                                      [[(first keys-a)
                                                                        (second keys-a)
                                                                        :unknown
                                                                        :unknown]
                                                                       [(first keys-b)
                                                                        (second keys-b)
                                                                        :unknown
                                                                        :unknown]]))
                                (sut/decrypted-ciphers->cards))
           cards-b          (-> (<! (sut/decrypt-ciphers-with-keys-2d ciphers
                                                                      [[:unknown
                                                                        :unknown
                                                                        (nth keys-a 2)
                                                                        (nth keys-a 3)]
                                                                       [:unknown
                                                                        :unknown
                                                                        (nth keys-b 2)
                                                                        (nth keys-b 3)]]))
                                (sut/decrypted-ciphers->cards))]
       (t/is (= [[:s :q] [:s :j] :unknown :unknown] cards-a))
       (t/is (= [:unknown :unknown [:s :a] [:s :k]] cards-b))
       (done)))))
