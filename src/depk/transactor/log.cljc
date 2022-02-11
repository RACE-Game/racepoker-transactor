(ns depk.transactor.log
  #?(:cljs
     (:require
      [taoensso.timbre :as log]))
  #?(:cljs
     (:require-macros depk.transactor.log)))

#?(:cljs
     (goog-define disable-log false))

#?(:clj
     (defmacro debug
       [& args]
       `(when-not disable-log
          (taoensso.timbre/debug ~@args))))

#?(:clj
     (defmacro info
       [& args]
       `(when-not disable-log
          (taoensso.timbre/info ~@args))))

#?(:clj
     (defmacro warn
       [& args]
       `(when-not disable-log
          (taoensso.timbre/warn ~@args))))

#?(:clj
     (defmacro error
       [& args]
       `(when-not disable-log
          (taoensso.timbre/error ~@args))))

#?(:clj
     (defmacro debugf
       [& args]
       `(when-not disable-log
          (taoensso.timbre/debugf ~@args))))

#?(:clj
     (defmacro infof
       [& args]
       `(when-not disable-log
          (taoensso.timbre/infof ~@args))))

#?(:clj
     (defmacro warnf
       [& args]
       `(when-not disable-log
          (taoensso.timbre/warnf ~@args))))

#?(:clj
     (defmacro errorf
       [& args]
       `(when-not disable-log
          (taoensso.timbre/errorf ~@args))))
