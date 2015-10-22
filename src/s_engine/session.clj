(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [malcolmx.core :as mx]
            [s-engine.model :as model]))

(def ^:const event-log-sheet "EventLog")

(def ^:const out-sheet "OUT")

(defrecord SessionEvent [event-type min sec attrs])

(defrecord Session [id workbook model-id])

(defn create!
  [storage model-id session-id]
  (let [{wb-file :file} (model/get-one model-id)
        workbook (mx/parse wb-file)
        session (->Session session-id workbook model-id)]
    (swap! (:session-table storage) assoc session-id session)
    session))

(defn get-one
  "Return single session by id"
  [storage session-id]
  (get @(:session-table storage) session-id))

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

(defrecord SessionStorage
  [session-table]
  component/Lifecycle
  (start [component]
    (let [session-table (atom {})]
      (assoc component
        :session-table session-table)))

  (stop  [component]
    (assoc component
      :session-table nil)))

(defn new-session-storage []
  (map->SessionStorage {}))