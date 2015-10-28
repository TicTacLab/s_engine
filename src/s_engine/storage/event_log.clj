(ns s-engine.storage.event-log
  (:require [cheshire.core :as json]
            [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer
             [append where columns limit]]))

(def ^:const event-log-table "event_log")

(defn fetch
  "Returns coll of event-log's for given event id or nil if not found."
  [storage event-id]
  (let [{:keys [conn]} storage
        record (first (cql/select conn event-log-table
                                  (columns :log_item)
                                  (where [[= :id event-id]])
                                  (limit 1)))]
    (when record
      (->> (:log_item record)
           (mapv json/parse-string)))))

(defn clear!
  [storage event-id]
  (let [{:keys [conn]} storage]
    (cql/delete conn event-log-table
                (where {:id event-id}))))

(defn append!
  [storage event-id events]
  (when events
    (let [log-rows (map json/generate-string events)
          {:keys [conn]} storage]
      (cql/update conn event-log-table
                  (array-map :log_item (append log-rows))
                  (where [[= :id event-id]])))))

(defn refresh!
  [storage event-id events]
  (>trace events)
  (let [log-rows (map json/generate-string events)
        {:keys [conn]} storage]
    (cql/insert conn event-log-table
                {:id event-id :log_item log-rows})))
