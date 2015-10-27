(ns s-engine.system
  (:require
    [com.stuartsierra.component :as component]
    [clojure.string :refer (split)]
    [s-engine.web :refer [new-web]]
    [s-engine.storage :refer [new-storage]]
    [s-engine.storage.model :refer [new-model-storage]]
    [s-engine.session :refer [new-session-storage]]))

(defn new-system [config]
  (component/map->SystemMap
    {:storage         (new-storage config)
     :model-storage   (component/using
                        (new-model-storage)
                        [:storage])
     :session-storage (component/using
                        (new-session-storage)
                        [:storage])
     :web             (component/using
                        (new-web config)
                        [:storage :session-storage :model-storage])}))
