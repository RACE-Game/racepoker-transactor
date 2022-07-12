(ns depk.transactor.log
  (:require
   [goog.string :refer [format]]))

(goog-define disable-log false)

(defn log
  [icon id fmt & args]
  (when-not disable-log
    (let [msg (apply format
                     (str "%s [%s] %s " fmt)
                     (.toLocaleString (js/Date.))
                     (if id
                       (let [l (count id)]
                         (subs id (- l 6) l))
                       "GLOBAL")
                     icon
                     args)]
      (println msg))))
