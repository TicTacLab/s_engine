(ns s-engine.storage.event-log
  (:require [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer [where columns limit]]))

(def ^:const event-log-table "event_log" )

(defmacro with-cassandra-storage [storage body]
  `(let [{:keys [~'conn]} ~storage]
     ~body))

(defn fetch [storage event-id]
  (with-cassandra-storage storage
    (cql/select conn event-log-table
                (columns :log-item)
                (where [[= :id event-id]]))))

(defn clear! [storage event-id]
  (with-cassandra-storage storage
    (cql/delete conn event-log-table
                (where [[= :id event-id]]))))

(defn append! [storage event-id log-rows]
  (with-cassandra-storage storage
    (doseq [r log-rows]
      (cql/insert conn event-log-table {:id event-id :log-item r}))))

(defn reset! [storage event-id log-rows]
  (clear! storage event-id)
  (append! storage event-id log-rows))


