(ns depk.transactor.helpers
  (:require
   [depk.transactor.game.api :as api]
   [depk.transactor.game :as game]
   [depk.transactor.game.handle :as handle]
   [depk.transactor.game.manager :as manager]
   [clojure.walk :as walk]
   [depk.transactor.state.game-manager :refer [game-manager]]))


(defn format-vals
  [m]
  (walk/postwalk
   (fn [x]
     (cond
       (and (string? x) (> (count x) 8))
       (str (subs x 0 4)
            "…"
            (subs x (- (count x) 4)))

       (and (number? x)
            (or (>= x 1000000000)
                (<= x -1000000000)))
       (/ x 1000000000)

       :else
       x))
   m))

;; (defn his
;;   [game-id]
;;   (js/console.log
;;    (->> (game/fetch-histories @game-manager game-id)
;;         (sort-by :game-no >)
;;         first
;;         :records
;;         (map (fn [[evt st]]
;;                [(str (name (:type evt)) "->" (name (:status st)))
;;                 (-> st
;;                     (dissoc :game-id :game-no)
;;                     (assoc :event evt)
;;                     (format-vals))])))))

(defn st
  [game-id]
  (js/console.log
   (-> (manager/find-game @game-manager game-id)
       (handle/get-snapshot))))

(comment
  (his "3uefwNfjDu7kxwEjQnBsp5qJkaeyBW18jGjydEH5EgyQ")
  (st "7AA21TvQT7WTa6AxcVh6CgRpr2zvUYPqfDvQGKf9v8kd"))
