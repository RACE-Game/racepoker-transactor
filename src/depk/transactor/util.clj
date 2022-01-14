(ns depk.transactor.util)

(defmacro log-group-collapsed
  [& args]
  `(when-not disable-log
     (.groupCollapsed js/console ~@args)))

(defmacro log-group-end
  []
  `(when-not disable-log
     (.groupEnd js/console)))

(defmacro info
  [& args]
  `(when-not disable-log
     (.info js/console ~@args)))

(defmacro warn
  [& args]
  `(when-not disable-log
     (.warn js/console ~@args)))

(defmacro error
  [& args]
  `(when-not disable-log
     (.error js/console ~@args)))

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
                (when log# (log-group-collapsed "request[%s]" (:uri req#)))
                (when log# (info "params:" (:body req#)))
                (let [resp# (do ~@body)]
                  (when log# (info "success:" resp#))
                  (callback# resp#))
                (catch js/Error e#
                  (when log# (warn "error:" e#))
                  (callback# {:status 500, :body {:error (ex-message e#)}}))
                (finally
                 (when log# (log-group-end)))))))))))

(defmacro go-try
  [& body]
  `(cljs.core.async/go
    (try ~@body
         (catch js/Error e#
           (error e#)
           (if-let [cause# (:cause e#)]
             cause#
             e#)))))

(defmacro go-loop-try
  [binding & body]
  `(cljs.core.async/go-loop ~binding
                            (try ~@body
                                 (catch js/Error e#
                                   (error e#)
                                   e#))))

(defmacro <!?
  [port]
  `(let [v# (cljs.core.async/<! ~port)]
     (if (instance? js/Error v#)
       (throw v#)
       v#)))
