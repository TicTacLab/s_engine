(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [s-engine.storage.workbook :refer [write-workbook!]]
            [s-engine.storage.model :as model]
            [clojure.tools.logging :as log]))

(defrecord Session [id model-wb model-id])

(defn create!
  [session-storage model-storage model-id session-id]
  (let [model (model/get-one model-storage model-id)
        model-wb (model/model-workbook model)
        session (->Session session-id model-wb (:id model))]
    (swap! (:session-table session-storage) assoc session-id session)
    session))

(defn get-one
  "Return single session by id or nil if not found"
  [session-storage session-id]
  (get @(:session-table session-storage) session-id nil))

(defn exists?
  [session-storage session-id]
  (contains? @(:session-table session-storage) session-id))

(defn valid-event?
  [session event]
  (model/valid-event? (:model-wb session) event))

(defn append-event!
  "Add event to session's event log"
  [session event]
  (let [{:keys [model-wb]} session]
    (model/append-events! model-wb [event])))

(defn get-events
  "Get event log of session as sequence of events"
  [session]
  (model/get-event-log-rows (:model-wb session)))

(defn set-events!
  "Set event log of session to given seq of events"
  [session events]
  (model/set-event-log! (:model-wb session) events))

(defn get-out
  "Get market outcome sheet values"
  [session]
  (model/get-out-rows (:model-wb session)))

(defn finalize
  "Closes session and saves final workbook"
  [session-storage storage session]
  (let [model-wb (:model-wb session)]
    (write-workbook! storage (:id session) model-wb)
    (model/finalize! (:model-wb session))
    (swap! (:session-table session-storage) dissoc (:id session))))

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