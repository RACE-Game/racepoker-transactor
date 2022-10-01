(ns depk.transactor.log
  (:require
   [goog.string :refer [format]]))

(goog-define disable-log false)

(def log-ignore-tags #{"➡️"
                       ;; "🔈"
                       })

(defn log
  [tag id fmt & args]
  (when-not (or disable-log (log-ignore-tags tag))
    (let [msg (apply format
                     (str "%s [%s] %s " fmt)
                     (.toLocaleString (js/Date.))
                     (if id
                       (let [l (count id)]
                         (subs id (- l 6) l))
                       "GLOBAL")
                     tag
                     args)]
      (println msg))))
