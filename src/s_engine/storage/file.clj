(ns s-engine.storage.file
  (:import (java.nio.file Paths Files)
           (org.apache.poi.ss.usermodel Workbook)
           (com.datastax.driver.core.utils Bytes))
  (:require [clojure.set :as set]
            [malcolmx.core :as mx]
            [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer [where columns limit]]))

(def ^:const event-type-sheet "EventType")
(def ^:const event-log-sheet "EventLog")
(def ^:const out-sheet "OUT")

(def ^:const event-type-column "EventType")
(def ^:const event-type-attr-column "MetaKey")
(def ^:const event-type-value-column "MetaValue")

;;
;; Persistense
;;

(def ^:private table-name "sengine_files")

(defrecord File [id file file-name])

(defn- row->file [row]
  (->File (:id row)
           (Bytes/getArray (:raw_file row))
           (:file_name row)))

(defn exists?
  [storage file-id]
  (let [{:keys [conn]} storage]
    (boolean (seq (cql/select conn table-name
                              (columns :id)
                              (where [[= :id file-id]])
                              (limit 1))))))

(defn write!
  [storage file-id file-bytes filename]
  (let [{:keys [conn]} storage
        file {:id file-id, :file_name filename, :raw_file file-bytes}]
    (cql/insert conn table-name file)))

(defn delete!
  [storage file-id]
  (let [{:keys [conn]} storage]
    (cql/delete conn table-name
                (where [[= :id file-id]]))))

(defn get-one
  "Retrieves file from storage, returns nil if not found"
  [storage file-id]
  (let [{:keys [conn]} storage
        row (first (cql/select conn table-name
                               (columns :id :raw_file :file_name)
                               (where [[= :id file-id]])))]
    (when row
      (row->file row))))

(defn read-bytes [^File file]
  (Files/readAllBytes (Paths/get (.toURI file))))

;;
;; ModelWorkbook
;;

(defrecord FileWorkbook [workbook event-types column-order])


(defn get-event-type-columns
  [rows]
  (->> rows
       (map #(get % event-type-attr-column))
       (remove #(nil? %))
       distinct))

(defn get-event-log-columns
  [workbook]
  (->> (mx/get-sheet-header workbook event-log-sheet)
       (remove #(empty? %))))

(defn validate-workbook
  [workbook]
  (let [event-type-columns (-> workbook
                             (mx/get-sheet event-type-sheet)
                             (get-event-type-columns))
        meta-columns ["min" "sec" "EventType"]
        event-log-columns (set (get-event-log-columns workbook))
        required-columns (set (concat meta-columns event-type-columns))]
    (concat
      (->> (set/difference required-columns event-log-columns)
           (map (fn [name] [::missing-column name])))
      (->> (set/difference event-log-columns  required-columns)
           (map (fn [name] [::extra-column name]))))))

(defn validate-file
  [^File file]
  (-> (read-bytes file)
      (mx/parse)
      (validate-workbook)))

(defn get-event-types [rows]
  "Returns attributes of event types in workbook:
  {(Event type name) {(Attribute name) #{(Possible values)}}
   \"Goal\" {\"Team\" #{\"Team1\" \"Team2\"}}}"
  (reduce
    (fn [acc row]
      (let [event-name (get row event-type-column)
            attr-name (get row event-type-attr-column)
            attr-val (get row event-type-value-column)]
        (if (empty? attr-name)
          acc
          (update-in acc [event-name attr-name] (fnil conj #{}) attr-val))))
    {}
    rows))

(defn valid-event-attrs?
  [file-wb event]
  (let [{event-type "EventType"} event]
    (->> (get-in file-wb [:event-types event-type])
         (every?
           (fn [[attr-name valid-values]]
             (contains? valid-values (get event attr-name)))))))

(defn valid-event?
  [file-wb event]
  (let [{event-type "EventType" :strs [min sec]} event]
    (and (contains? (:event-types file-wb) event-type)
         min sec
         (valid-event-attrs? file-wb event))))

(defn clear-event-log! [file-wb]
  (mx/remove-rows! (:workbook file-wb) event-log-sheet 1))

(defn new-file-workbook [file]
  (let [workbook (mx/parse (:file file))
        rows (mx/get-sheet workbook event-type-sheet)
        event-types (get-event-types rows)
        column-order (get-column-order rows)
        file-wb (->FileWorkbook workbook event-types column-order)]
    (clear-event-log! file-wb)
    file-wb))

(defn finalize! [file-wb]
  (.close ^Workbook (:workbook file-wb)))

(defn event->row-data
  "Transforms event into vector of row cells"
  [column-order event]
  (let [{event-type "EventType" :strs [min sec]} event]
    (->> column-order
         (mapv #(get event % ""))
         (concat [min sec event-type]))))

(defn append-events!
  "Appends given events coll to end of event log sheet"
  [file-wb events]
  (let [{:keys [workbook column-order]} file-wb]
    (->> events
         (map #(event->row-data column-order %))
         (mx/append-rows! workbook event-log-sheet))))

(defn get-event-log-rows
  "Returns contents of event log sheet"
  [file-wb]
  (->> event-log-sheet
       (mx/get-sheet (:workbook file-wb))
       (map #(dissoc % ""))))

(defn set-event-log!
  "Sets contents of event log sheet to given coll of events"
  [file-wb events]
  (clear-event-log! file-wb)
  (append-events! file-wb events))

(defn get-out-rows
  "Returns contents of out sheet"
  [file-wb]
  (mx/get-sheet (:workbook file-wb) out-sheet))
