(ns depk.transactor.state.config
  (:require
   [depk.transactor.util.config :as conf]
   [mount.core      :as mount]
   [depk.transactor.log :as log]
   [clojure.string  :as str]))

(def env (atom :local))

(defn use-env
  [env-kw]
  (reset! env env-kw))

;; quick and dirty hack
(defn build-config
  "build configuration map based on current env."
  []
  (let [conf (conf/env)
        e    (str (name @env) "-")]
    (log/infof "🌐Current environment: %s" @env)
    (->> (for [[k v] conf]
           (if (str/starts-with? (name k) e)
             [(keyword (str/replace-first (name k) e ""))
              v]
             [k v]))
         (into {}))))

(mount/defstate config
  :start
  (build-config))
