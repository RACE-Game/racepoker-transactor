(ns depk.transactor.util
  (:require [depk.transactor.log]))

(defmacro def-async-handler
  [name binding & body]
  {:pre [(vector? binding) (= 1 (count binding))]}
  (let [req (first binding)]
    `(defn ~name
       [req# callback# raise#]
       (let [~req req#]
         (cljs.core.async/go
          (let [uri# (:uri req#)
                log# (nil? (get depk.transactor.util/request-log-ignores uri#))]
            (when-not
              (try
                (when log# (depk.transactor.log/infof "request:[%s]" (:uri req#)))
                ;; (when log# (info "params:[%s]" (prn-str (:body req#))))
                (let [resp# (do ~@body)]
                  (callback# resp#))
                (catch js/Error e#
                  (when log# (depk.transactor.log/warnf "error:[%s]" (ex-message e#)))
                  (callback# {:status 500, :body {:error (ex-message e#)}}))))))))))

(defmacro go-try
  [& body]
  `(cljs.core.async/go
    (try ~@body
         (catch js/Error e#
           (depk.transactor.log/error e# (ex-message e#))
           (if-let [cause# (:cause e#)]
             cause#
             e#)))))

(defmacro go-loop-try
  [binding & body]
  `(cljs.core.async/go-loop ~binding
                            (try ~@body
                                 (catch js/Error e#
                                   (depk.transactor.log/error (ex-message e#))
                                   e#))))

(defmacro <!?
  [port]
  `(let [v# (cljs.core.async/<! ~port)]
     (if (instance? js/Error v#)
       (throw v#)
       v#)))
