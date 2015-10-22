(ns s-engine.system
  (:require
    [com.stuartsierra.component :as component]
    [clojure.string :refer (split)]
    [s-engine.web :refer [new-web]]
    [s-engine.storage :refer [new-storage]]
    [s-engine.session :refer [new-session-storage]]))

(defn new-system [config]
  (component/map->SystemMap
    {:web           (new-web config)
     :storage       (new-storage config)
     :session-store (component/using
                      (new-session-storage)
                      [:storage])}))
