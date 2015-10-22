(ns s-engine.storage
  (:require [com.stuartsierra.component :as component]
            [schema.core :as s]
            [clojurewerkz.cassaforte.client :as cc]
            [clojurewerkz.cassaforte.policies :as cp]
            [clojure.tools.logging :as log]))


(defn try-connect-times [times delay-ms nodes keyspace opts]
  (let [result (try
                 (cc/connect nodes keyspace opts)
                 (catch NoHostAvailableException ex ex))]
    (cond
      (and (instance? Exception result) (zero? times)) (throw result)
      (instance? Exception result) (do
                                     (log/warnf "Failed to connect to Cassandra, will retry after %d ms" delay-ms)
                                     (Thread/sleep delay-ms)
                                     (recur (dec times) delay-ms nodes keyspace opts))
      :else result)))

(defrecord Storage [conn
                    storage-nodes
                    storage-keyspace
                    storage-user
                    storage-password]

  component/Lifecycle
  (start [component]
    (let [conn (try-connect-times 1000
                                  1000
                                  storage-nodes
                                  storage-keyspace
                                  {:credentials         {:username storage-user
                                                         :password storage-password}
                                   :reconnection-policy (cp/constant-reconnection-policy 100)})]
      (log/info "Storage started")
      (assoc component
        :conn conn)))

  (stop [component]
    (when conn
      (cc/disconnect conn))
    (log/info "Storage stopped")
    (assoc component
      :conn nil)))


(def StorageSchema
  {:storage-nodes    [s/Str]
   :storage-keyspace s/Str
   :storage-user     s/Str
   :storage-password s/Str})

(defn new-storage [m]
  (as-> m $
        (select-keys $ (keys StorageSchema))
        (s/validate StorageSchema $)
        (map->Storage $)))
