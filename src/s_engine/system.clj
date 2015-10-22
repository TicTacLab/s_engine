(ns s-engine.system
  (:require
    [com.stuartsierra.component :as component]
    [clojure.string :refer (split)]
    [s-engine.web :as w]
    [s-engine.session :as s]))

(defn new-system [config]
  (component/map->SystemMap
    {:web           (w/new-web config)
     :session-store (s/new-session-storage)}))
