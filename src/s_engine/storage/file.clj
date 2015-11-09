(ns s-engine.storage.file
  (:require [malcolmx.core :as mx]
            [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer [where columns limit]]
            [clojure.data :refer [diff]])
  (:import (java.nio.file Paths Files)
           (org.apache.poi.ss.usermodel Workbook)
           (com.datastax.driver.core.utils Bytes)
           (java.io ByteArrayOutputStream OutputStream)
           (java.util Date)))

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

(defrecord File [id file file-name last-modified])

(defn- row->file [row]
  (->File (:id row)
          (when (:raw_file row)
            (Bytes/getArray (:raw_file row)))
          (:file_name row)
          (:last_modified row)))

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
        file {:id            file-id,
              :file_name     filename
              :raw_file      file-bytes
              :last_modified (Date.)}]
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

(defn get-all
  [storage]
  (let [{:keys [conn]} storage]
    (->> (cql/select conn table-name
                     (columns :id :file_name :last_modified))
         (map row->file))))

(defn read-bytes [^File file]
  (Files/readAllBytes (Paths/get (.toURI file))))

;;
;; ModelWorkbook
;;

(defrecord FileWorkbook [workbook event-types file-name out event-log])

(defn get-event-type-attrs
  "Returns seq of attribute names declared in EventType sheet"
  [rows]
  (->> rows
       (map #(get % event-type-attr-column))
       (remove #(nil? %))
       distinct))

(defn get-event-log-columns
  "Returns seq of column names declared in header of EventLog sheet"
  [workbook]
  (->> (mx/get-sheet-header workbook event-log-sheet)
       (remove #(empty? %))))

(defn validate-columns
  "Checks that column names in EventLog sheet corresponds to attributes
  defined in EventType sheet. Vector [missing-attrs extra-attrs]."
  [attr-names event-log-columns]
  (let [required (conj (set attr-names) event-type-column)]
    (take 2 (diff required (set event-log-columns)))))

(defn validate-event-log-header
  "Checks that given file contains valid EventLog header."
  [^File file]
  (let [workbook (mx/parse (read-bytes file))
        attrs (-> workbook
                  (mx/get-sheet event-type-sheet)
                  (get-event-type-attrs))
        event-log-columns (get-event-log-columns workbook)]
    (validate-columns attrs event-log-columns)))

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

(defn new-file-workbook
  [file]
  (let [workbook (mx/parse (:file file))
        rows (mx/get-sheet workbook event-type-sheet)
        event-types (get-event-types rows)
        out (atom [])
        event-log (atom [])
        file-wb (->FileWorkbook workbook event-types (:file-name file) out event-log)]
    (clear-event-log! file-wb)
    file-wb))

(defn finalize! [file-wb]
  (.close ^Workbook (:workbook file-wb)))

(defn event->row-data
  "Transforms event into vector of row cells"
  [column-order event]
  (mapv #(get event % "") column-order))

(defn append-events!
  "Appends given events coll to end of event log sheet"
  [file-wb events]
  (reset! (:event-log file-wb) events)
  (let [{:keys [workbook]} file-wb
        column-order (get-event-log-columns workbook)]
    (->> events
         (map #(event->row-data column-order %))
         (mx/append-rows! workbook event-log-sheet))))

(defn get-event-log-rows
  "Returns contents of event log sheet"
  [file-wb]
  (->> event-log-sheet
       (mx/get-sheet (:workbook file-wb))
       (map #(dissoc % ""))
       (map (fn [row]
              (->> row
                   (remove (comp empty? second))
                   (into {}))))))

(defn get-cached-event-log-rows
  "Return cached event-log"
  [file-wb]
  @(:event-log file-wb))

(defn set-event-log!
  "Sets contents of event log sheet to given coll of events"
  [file-wb events]
  (clear-event-log! file-wb)
  (append-events! file-wb events))

(defn get-workbook
  "Returns name of workbook file and workbook contents as bytes array."
  [file-wb]
  (let [^Workbook workbook (:workbook file-wb)
        ^OutputStream out (ByteArrayOutputStream.)
        _ (.write workbook out)
        bytes-arr (.toByteArray out)]
    (.close out)
    {:file-name (:file-name file-wb)
     :bytes     bytes-arr}))

(defn get-out-rows
  "Returns contents of out sheet"
  [file-wb]
  (let [result (mx/get-sheet (:workbook file-wb) out-sheet)]
    (reset! (:out file-wb) result)
    result))

(defn get-cached-out-rows
  "Returns cached out"
  [file-wb]
  @(:out file-wb))