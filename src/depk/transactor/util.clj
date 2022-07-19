(ns depk.transactor.util)

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
                (let [resp# (do ~@body)]
                  (callback# resp#))
                (catch js/Error e#
                  (depk.transactor.log/log "💥" nil (ex-message e#))
                  (callback# {:status 500, :body {:error (ex-message e#)}}))))))))))

(defmacro go-try
  [& body]
  `(cljs.core.async/go
    (try ~@body
         (catch js/Error e#
           (depk.transactor.log/log "💥" nil (ex-message e#))
           (if-let [cause# (:cause e#)]
             cause#
             e#)))))

(defmacro go-loop-try
  [binding & body]
  `(cljs.core.async/go-loop ~binding
                            (try ~@body
                                 (catch js/Error e#
                                   (depk.transactor.log/log "💥" nil (ex-message e#))
                                   e#))))

(defmacro <!?
  [port]
  `(let [v# (cljs.core.async/<! ~port)]
     (if (instance? js/Error v#)
       (throw v#)
       v#)))
