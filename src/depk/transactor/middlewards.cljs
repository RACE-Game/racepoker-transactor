(ns depk.transactor.middlewards
  (:require [depk.transactor.log :as log]))

(defn wrap-exception [handler]
  (fn [request callback raise]
    (try
      (handler request callback raise)
      (catch js/Error e
        (log/error e "error processing request")
        (callback {:status 500
                   :body "Error"})))))
