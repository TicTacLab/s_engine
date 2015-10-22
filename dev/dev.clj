 (ns dev
   (:require [ns-tracker.core :refer (ns-tracker)]
             [s-engine.system :as s]
             [clojure.pprint :refer [pprint]]
             [com.stuartsierra.component :as component]
             [s-engine.config :as c]))

(defonce system nil)

(defn init []
  (alter-var-root #'system (constantly (s/new-system @c/config))))

(defn start []
  (alter-var-root #'system component/start))

(defn go []
  (init)
  (start))

(defn stop []
  (when system
    (alter-var-root #'system component/stop)))

(def ^:private modified-ns
  (ns-tracker ["src"]))

(defn reload-ns []
  (doseq [ns-sym (modified-ns)]
    (require ns-sym :reload)))

(defn reset []
  (stop)
  (reload-ns)
  (go))
