(ns depk.transactor.log
  (:require-macros depk.transactor.log)
  (:require
   [taoensso.timbre :as log]
   [clojure.string  :as str]))

(log/merge-config! {:output-fn (fn [d]
                                 (str
                                  (.toLocaleString (js/Date.))
                                  " "
                                  (str/upper-case (name (:level d)))
                                  " "
                                  (force (:msg_ d))))})

(goog-define disable-log false)
