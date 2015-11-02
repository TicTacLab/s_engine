(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [s-engine.storage.workbook :refer [write-workbook!]]
            [s-engine.storage.file :as file]
            [s-engine.storage.event-log :as ev]))

(defrecord Session [id file-wb file-id])

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
  (file/valid-event? (:file-wb session) event))

(defn append-event!
  "Add event to session's event log"
  [storage session events]
  (let [{:keys [file-wb]} session]
    (file/append-events! file-wb events)
    (ev/append! storage (:id session) events)))

(defn get-events
  "Get event log of session as sequence of events"
  [session]
  (file/get-event-log-rows (:file-wb session)))

(defn set-events!
  "Set event log of session to given seq of events"
  [storage session events]
  (file/set-event-log! (:file-wb session) events)
  (ev/refresh! storage (:id session) events))

(defn get-out
  "Get market outcome sheet values"
  [session]
  (file/get-out-rows (:file-wb session)))

(defn get-workbook [session]
  (file/get-workbook (:file-wb session)))

(defn create!
  "Creates new session."
  [session-storage storage file-id session-id]
  (let [file (file/get-one storage file-id)
        file-wb (file/new-file-workbook file)
        session (->Session session-id file-wb (:id file))
        events (ev/fetch storage session-id)]
    (swap! (:session-table session-storage) assoc session-id session)
    (when events
      (set-events! storage session events))
    session))

(defn finalize!
  "Closes session and saves final workbook"
  [session-storage storage session]
  (let [file-wb (:file-wb session)]
    (write-workbook! storage (:id session) file-wb)
    (file/finalize! (:file-wb session))
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