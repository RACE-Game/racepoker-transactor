(ns depk.transactor.handlers
  (:require
   [depk.transactor.log :as log]
   [clojure.string :as str]
   [depk.transactor.util :refer [<!? def-async-handler]]
   [depk.transactor.game :as game]
   [solana-clj.publickey :as pubkey]
   [depk.transactor.state.game-manager :refer [game-manager]]
   [depk.transactor.game.api :as api]
   [depk.transactor.state.api :refer [chain-api]]
   ["buffer" :as buffer]
   ["tweetnacl" :as nacl]))

(defn verify-signature
  "Verify signature.

  A signature is a signed message of player-id + game-id."
  [sig player-id game-id]
  (let [pubkey (pubkey/to-buffer (pubkey/make-public-key player-id))
        msg    (buffer/Buffer.from
                (str "player-id=" player-id "&game-id=" game-id))]
    (when-not (nacl/sign.detached.verify msg sig pubkey)
      (throw (ex-info "Invalid signature" {:msg msg, :key player-id})))))

(def-async-handler alive-handler
  [_]
  {:status 200,
   :body   {:message "RACE transactor is running"}})

(def-async-handler histories
  [{:keys [query-params]}]
  (let [{:strs [game-id]} query-params]
    {:status 200,
     :body   (game/fetch-histories @game-manager game-id)}))

(def-async-handler attach-game
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig]} body]
    (verify-signature sig player-id game-id)
    (if (<!? (game/attach-game @game-manager game-id player-id))
      {:status 200}
      {:status 400})))

(def-async-handler state
  [{:keys [body]}]
  (let [{:keys [game-id]} body
        resp (<!? (game/state @game-manager game-id))]
    {:status 200,
     :body   {:state resp}}))

(def-async-handler leave
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig released-keys]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/leave @game-manager game-id player-id released-keys))
    {:status 200, :body "ok"}))

(def-async-handler alive
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/alive @game-manager game-id player-id))
    {:status 200, :body "ok"}))

(def-async-handler shuffle-cards
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig data]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/shuffle-cards @game-manager game-id player-id data))
    {:status 200, :body "ok"}))

(def-async-handler encrypt-cards
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig data]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/encrypt-cards @game-manager game-id player-id data))
    {:status 200, :body "ok"}))

(def-async-handler share-keys
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig share-keys]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/share-keys @game-manager game-id player-id share-keys))
    {:status 200, :body "ok"}))

(def-async-handler release
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig released-keys]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/release @game-manager game-id player-id released-keys))
    {:status 200, :body "ok"}))

(def-async-handler call
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/player-call @game-manager game-id player-id))
    {:status 200, :body "ok"}))

(def-async-handler fold
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig share-keys]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/player-fold @game-manager game-id player-id share-keys))
    {:status 200, :body "ok"}))

(def-async-handler bet
  [{:keys [body]}]
  (let [{:keys [game-id player-id amount sig]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/player-bet @game-manager game-id player-id amount))
    {:status 200, :body "ok"}))

(def-async-handler raise
  [{:keys [body]}]
  (let [{:keys [game-id player-id amount sig]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/player-raise @game-manager game-id player-id amount))
    {:status 200, :body "ok"}))

(def-async-handler check
  [{:keys [body]}]
  (let [{:keys [game-id player-id sig]} body]
    (verify-signature sig player-id game-id)
    (<!? (game/player-check @game-manager game-id player-id))
    {:status 200, :body "ok"}))

(def-async-handler request-test-token
  [{:keys [body]}]
  (let [{:keys [player-id]} body]
    (<!? (api/faucet-request @chain-api player-id))
    {:status 200, :body "ok"}))

(def-async-handler musk
  [req]
  {:status 200, :body "ok"})
