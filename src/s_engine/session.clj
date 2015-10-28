(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [s-engine.storage.workbook :refer [write-workbook!]]
            [s-engine.storage.file :as file]
            [s-engine.storage.event-log :as ev]))

(defrecord Session [id model-wb model-id])

(defn get-one
  "Return single session by id or nil if not found"
  [session-storage session-id]
  (get @(:session-table session-storage) session-id nil))

(defn get-all
  "Returns all sessions"
  [session-storage]
  (vals @(:session-table session-storage)))

(defn exists?
  [session-storage session-id]
  (contains? @(:session-table session-storage) session-id))

(defn valid-event?
  [session event]
  (file/valid-event? (:model-wb session) event))

(defn append-event!
  "Add event to session's event log"
  [storage session event]
  (let [{:keys [model-wb]} session]
    (file/append-events! model-wb [event])
    (ev/append! storage (:id session) [event])))

(defn get-events
  "Get event log of session as sequence of events"
  [session]
  (file/get-event-log-rows (:model-wb session)))

(defn set-events!
  "Set event log of session to given seq of events"
  [storage session events]
  (file/set-event-log! (:model-wb session) events)
  (ev/refresh! storage (:id session) events))

(defn get-out
  "Get market outcome sheet values"
  [session]
  (file/get-out-rows (:model-wb session)))

(defn create!
  "Creates new session."
  [session-storage storage model-id session-id]
  (let [model (>trace (file/get-one storage model-id))
        model-wb (file/new-model-workbook model)
        session (->Session session-id model-wb (:id model))
        events (ev/fetch storage session-id)]
    (>trace events)
    (swap! (:session-table session-storage) assoc session-id session)
    (when events
      (set-events! storage session events))
    session))

(defn finalize!
  "Closes session and saves final workbook"
  [session-storage storage session]
  (let [model-wb (:model-wb session)]
    (write-workbook! storage (:id session) model-wb)
    (file/finalize! (:model-wb session))
    (swap! (:session-table session-storage) dissoc (:id session))
    (ev/clear! storage (:id session))))

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