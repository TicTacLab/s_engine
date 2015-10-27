(ns s-engine.system
  (:require
    [com.stuartsierra.component :as component]
    [clojure.string :refer (split)]
    [s-engine.web :refer [new-web]]
    [s-engine.storage :refer [new-storage]]
    [s-engine.storage.model :refer [new-cassandra-model-storage new-test-model-storage]]
    [s-engine.session :refer [new-session-storage]]))

(defn new-system [config & {:keys [test] :or {test false}}]
  (component/map->SystemMap
    {:storage         (new-storage config)
     :model-storage   (component/using
                        (if test
                          (new-test-model-storage)
                          (new-cassandra-model-storage))
                        [:storage])
     :session-storage (component/using
                        (new-session-storage)
                        [:storage])
     :web             (component/using
                        (new-web config)
                        [:storage :session-storage :model-storage])}))
