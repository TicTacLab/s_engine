(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [malcolmx.core :as mx]
            [s-engine.storage.workbook :refer [write-workbook!]])
  (:import (org.apache.poi.ss.usermodel Workbook)))

(def ^:const event-log-sheet "EventLog")

(def ^:const out-sheet "OUT")

(defrecord SessionEvent [event-type min sec attrs])

(defrecord Session [id workbook model-id])

(defn create!
  [session-storage model-id workbook-bytes session-id]
  (let [workbook (mx/parse workbook-bytes)
        session (->Session session-id workbook model-id)]
    (swap! (:session-table session-storage) assoc session-id session)
    session))

(defn get-one
  "Return single session by id"
  [session-storage session-id]
  (get @(:session-table session-storage) session-id))

(defn append-event!
  "Add event to session's event log"
  [session event]
  )

(defn get-events
  "Get event log of session as sequence of events"
  [session]
  (let [wb (:workbook session)]
    (mx/get-sheet wb event-log-sheet)))

(defn set-events!
  "Set event log of session to given seq of events"
  [session events-coll]
  )

(defn get-out
  "Get market outcomes values"
  [session]
  (let [wb (:workbook session)
        out-rows (mx/get-sheet wb out-sheet)]
    (map
      (fn [row]
        {:market (get row "MarketID")
         :out (get row "Out")})
      out-rows)))

(defn finalize
  "Closes session and saves final workbook"
  [session-storage storage session]
  (let [wb (:workbook session)]
    (write-workbook! storage wb)
    (swap! (:session-table session-storage) dissoc (:id session))
    (.close ^Workbook wb)))

(defrecord SessionStorage [session-table storage]
  component/Lifecycle
  (start [component]
    (let [session-table (atom {})]
      (assoc component
        :session-table session-table)))

  (stop [component]
    (assoc component
      :session-table nil)))

(defn new-session-storage []
  (map->SessionStorage {}))