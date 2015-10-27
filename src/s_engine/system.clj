(ns s-engine.system
  (:require
    [com.stuartsierra.component :as component]
    [clojure.string :refer (split)]
    [s-engine.web :refer [new-web]]
    [s-engine.storage :refer [new-storage]]
    [s-engine.session :refer [new-session-storage]]))

(defn new-system [config]
  (component/map->SystemMap
    {:storage         (new-storage config)
     :session-storage (component/using
                        (new-session-storage)
                        [:storage])
     :web             (component/using
                        (new-web config)
                        [:storage :session-storage])}))
