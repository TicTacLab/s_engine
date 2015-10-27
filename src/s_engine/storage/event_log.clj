(ns s-engine.storage.event-log
  (:require [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer
             [append where columns]]))

(def ^:const event-log-table "event_log" )

(defn fetch [storage event-id]
  {:pre [(string? event-id)]
   :post [(vector? %)]}
  (let [{:keys [conn]} storage]
    (mapv :log_item (cql/select conn event-log-table
                                (columns :log_item)
                                (where [[= :id event-id]])))))

(defn clear! [storage event-id]
  {:pre [(string? event-id)]}
  (let [{:keys [conn]} storage]
    (cql/delete conn event-log-table
                (where {:id event-id}))))

(defn append! [storage event-id log-rows]
  {:pre [(string? event-id) (not-empty log-rows)]}
  (let [{:keys [conn]} storage]
    (cql/update conn event-log-table
                (array-map :log_item (append log-rows))
                (where [[= :id event-id]]))))

(defn refresh! [storage event-id log-rows]
  {:pre [(string? event-id) (not-empty log-rows)]}
  (let [{:keys [conn]} storage]
    (cql/insert conn event-log-table
                {:id event-id :log_item log-rows})))
