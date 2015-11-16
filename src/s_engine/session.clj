(ns s-engine.session
  (:require [com.stuartsierra.component :as component]
            [s-engine.storage.workbook :refer [write-workbook!]]
            [s-engine.storage.file :as file]
            [s-engine.storage.event-log :as ev]))

(defrecord Session [id file-wb file-id description])

(defn get-one
  "Return single session by id or nil if not found"
  [session-storage session-id]
  (get @(:session-table session-storage) session-id nil))

(defn get-all-ids
  "Returns all sessions"
  [session-storage]
  (->> session-storage
       :session-table
       (deref)
       (vals)
       (map #(select-keys % [:id :description]))))

(defn exists?
  [session-storage session-id]
  (contains? @(:session-table session-storage) session-id))

(defn get-out
  "Get market outcome sheet values"
  [session]
  (file/get-out-rows (:file-wb session)))

(defn is-field-equal?
  [k v record]
  (= v (get record k)))

(defn filter-out
  [k-v-filters out-sheet]
  (let [filter-predicates (->>
                            k-v-filters
                            (mapcat (fn [[k v]] (mapv #(partial is-field-equal? k %) v)))
                            (apply some-fn))]
    (filter #(not (filter-predicates %)) out-sheet)))

(defn clean-out!
  "Filter and Set OUT markets"
  [session k-v-filters]
  (let [current-out (file/get-out-row-numbers (:file-wb session))
        rows-for-clean (->> (filter-out k-v-filters current-out) (mapv :row-number))]
    (file/remove-out-row-numbers! (:file-wb session) rows-for-clean)
    (get-out session)))

(defn append-events!
  "Add event to session's event log"
  [storage session events]
  (let [{:keys [file-wb]} session]
    (file/append-events! file-wb events)
    (ev/append! storage (:id session) events)
    (get-out session)))

(defn get-event-log
  "Get event log of session as sequence of events"
  [session]
  (file/get-event-log-rows (:file-wb session)))

(defn get-cached-event-log
  [session]
  (let [event-log (file/get-cached-event-log-rows (:file-wb session))]
    (if (seq event-log)
      event-log
      (file/get-event-log-rows (:file-wb session)))))

(defn set-events!
  "Set event log of session to given seq of events"
  [storage session events]
  (file/set-event-log! (:file-wb session) events)
  (ev/refresh! storage (:id session) events)
  (get-out session))

(defn get-cached-out
  "Get market outcome sheet values"
  [session]
  (let [out-rows (file/get-cached-out-rows (:file-wb session))]
    (if (seq out-rows)
      out-rows
      (file/get-out-rows (:file-wb session)))))

(defn get-workbook [session]
  (file/get-workbook (:file-wb session)))

(defn create!
  "Creates new session."
  [session-storage storage file-id session-id description]
  (let [file (file/get-one storage file-id)
        file-wb (file/new-file-workbook file)
        session (->Session session-id file-wb (:id file) description)
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

(defn extra-event-types [session events]
  (file/extra-event-types (:file-wb session) events))

(defn extra-attributes [session events]
  (file/extra-attributes (:file-wb session) events))

(defn invalid-values [session events]
  (file/invalid-values (:file-wb session) events))

