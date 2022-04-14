(ns depk.transactor.log)

(defmacro debug
  [& args]
  `(when-not disable-log
     (taoensso.timbre/debug ~@args)))

(defmacro info
  [& args]
  `(when-not disable-log
     (taoensso.timbre/info ~@args)))

(defmacro warn
  [& args]
  `(when-not disable-log
     (taoensso.timbre/warn ~@args)))

(defmacro error
  [& args]
  `(when-not disable-log
     (taoensso.timbre/error ~@args)))

(defmacro debugf
  [& args]
  `(when-not disable-log
     (taoensso.timbre/debugf ~@args)))

(defmacro infof
  [& args]
  `(when-not disable-log
     (taoensso.timbre/infof ~@args)))

(defmacro warnf
  [& args]
  `(when-not disable-log
     (taoensso.timbre/warnf ~@args)))

(defmacro errorf
  [& args]
  `(when-not disable-log
     (taoensso.timbre/errorf ~@args)))
