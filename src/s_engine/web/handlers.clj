(ns s-engine.web.handlers
  (:require [clojure.string :as str]
            [s-engine.storage.file :as file]
            [malcolmx.core :as mx]
            [s-engine.session :as session]
            [ring.util.response :as res]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))
;; ======
;; Utils
;; ======

(defn new-error [status code message]
  {:status status
   :errors [{:code    code
             :message message}]})

(defn success-response
  ([status]
   (success-response status nil))
  ([status json-body]
   (-> {:status status
        :body (json/generate-string {:status status
                                       :data   json-body})}
       (res/content-type "application/json")
       (res/charset "utf-8"))))

(defn error-response [status code message]
  (log/errorf "Status: \"%s\", Code: \"%s\", Message \"%s\"."
              status code message)
  (-> {:status status
       :body   (json/generate-string (new-error status code message))}
      (res/content-type "application/json")
      (res/charset "utf-8")))

(defn file-response
  [bytes-arr file-name]
  (-> (res/response (io/input-stream bytes-arr))
      (res/content-type "application/octet-stream")
      (res/header "Content-Length" (count bytes-arr))
      (res/header "Content-Disposition" (format "attachment; filename=%s" file-name))))

(defn json->clj [s]
  (try
    (json/parse-string s false)
    (catch Exception e
      (log/error e "Error parsing json")
      nil)))

(defn string->int [s]
  (try
    (Integer/valueOf s)
    (catch Exception e
      (println (.getLocalizedMessage e))
      nil)))

(defn call [f obj web]
  ((f identity) obj web))

;; ======
;; Handlers
;; ======

(defn check-event-id [h]
  (fn [{event-id :event-id :as p} w]
    (if (seq event-id)
      (h p w)
      (error-response 400 "MFP" "Invalid event id"))))

(defn check-session-exists [h]
  (fn [{event-id :event-id :as p} {session-storage :session-storage :as w}]
    (if (session/exists? session-storage event-id)
      (h p w)
      (error-response 404 "ENF" (format "Event with id '%s' is not created" event-id)))))

(defn check-session-not-exists [h]
  (fn [{event-id :event-id :as p} {session-storage :session-storage :as w}]
    (if-not (session/exists? session-storage event-id)
      (h p w)
      (error-response 400 "EAC" (format "Event with id '%s' is already created" event-id)))))

(defn check-file-not-exists [h]
  (fn [{file-id :file-id :as p} {storage :storage :as w}]
    (if (file/exists? storage file-id)
      (error-response 400 "FAE" "File already exists")
      (h p w))))

(defn check-file-exists [h]
  (fn [{file-id :file-id :as p} {storage :storage :as w}]
    (if (file/exists? storage file-id)
      (h p w)
      (error-response 404 "FNF" (format "File with id '%s' not found" file-id)))))

(defn write-file! [h]
  (fn [{file-id :file-id file :file} {storage :storage}]
    (let [file-bytes (file/read-bytes (:tempfile file))]
      (file/write! storage file-id file-bytes (:filename file))
      (success-response 200))))

(defn delete-file! [h]
  (fn [{file-id :file-id} {storage :storage}]
    (file/delete! storage file-id)
    (success-response 200)))

(defn create-session! [h]
  (fn [{:keys [file-id event-id description]} {:keys [session-storage storage]}]
    (session/create! session-storage storage file-id event-id description)
    (success-response 200)))

(defn finalize-session! [h]
  (fn [{:keys [event-id]} {:keys [session-storage storage]}]
    (let [session (session/get-one session-storage event-id)]
      (session/finalize! session-storage storage session)
      (success-response 200))))

(defn get-event-log [h]
  (fn [{:keys [event-id]} {:keys [session-storage]}]
    (->> (session/get-one session-storage event-id)
         (session/get-cached-event-log)
         (success-response 200))))

(defn append-events! [h]
  (fn [{:keys [events event-id]} {:keys [storage session-storage]}]
    (let [session (session/get-one session-storage event-id)]
      (success-response 200 (session/append-events! storage session events)))))

(defn set-events! [h]
  (fn [{:keys [events event-id]} {:keys [storage session-storage]}]
    (let [session (session/get-one session-storage event-id)]
      (success-response 200 (session/set-events! storage session events)))))

(defn get-settlements [h]
  (fn [{:keys [event-id]} {:keys [session-storage]}]
    (->> (session/get-one session-storage event-id)
         (session/get-cached-out)
         (success-response 200))))

(defn get-workbook [h]
  (fn [{:keys [event-id]} {:keys [session-storage]}]
    (let [session (session/get-one session-storage event-id)
          {:keys [file-name bytes]} (session/get-workbook session)]
      (file-response bytes file-name))))

(defn download-file [h]
  (fn [{file-id :file-id} {storage :storage}]
    (let [{:keys [file file-name]} (file/get-one storage file-id)]
      (file-response file file-name))))

(defn parse-file-id [h]
  (fn [params web]
    (if-let [file-id (string->int (:file-id params))]
      (h (assoc params :file-id file-id) web)
      (error-response 400 "MFP" "Invalid file id"))))

(defn parse-params [h]
  (fn [params-str web]
    (if-let [params-strs (json->clj params-str)]
      (let [params-keywords (->> (get params-strs "params")
                                 (into {} (map (fn [[k v]] [(keyword k) v]))))]
        (h params-keywords web))
      (error-response 400 "MFP" "Invalid params"))))

(defn check-file-present [h]
  (fn [params web]
    (let [file (:file params)]
      (if (and file (:tempfile file))
        (h params web)
        (error-response 400 "MFP" "No file sent")))))

(defn check-file-type [h]
  (fn [params web]
    (let [tempfile (:tempfile (:file params))]
      (if (or (mx/excel-file? tempfile "xlsx")
              (mx/excel-file? tempfile "xls"))
        (h params web)
        (error-response 400 "MFP" "Invalid file type")))))

(defn check-file-validity [h]
  (fn [params web]
    (let [[missing extra] (file/validate-event-log-header (:tempfile (:file params)))]
      (if (or (seq missing) (seq extra))
        (->> (format "Missing Columns: [%s]; Extra Columns [%s];"
                     (str/join ", " missing)
                     (str/join ", " extra))
             (error-response 400 "MFP"))
        (h params web)))))

(defn parse-events [h]
  (fn [{events-str :events :as p} w]
    (if-let [events (get (json->clj events-str) "params")]
      (h (assoc p :events events) w)
      (error-response 400 "MFP" "Malformed json body"))))

(defn sequential-events [h]
  (fn [{events :events :as p} w]
    (if (sequential? events)
      (h p w)
      (error-response 400 "MFP" "Events should be inside json array"))))

(defn at-least-1-event [h]
  (fn [{events :events :as p} w]
    (if (>= (count events) 1)
      (h p w)
      (error-response 400 "MFP" "At least 1 event should be sent"))))

(defn check-EventType-key [h]
  (fn [{events :events :as p} w]
    (if (every? #(contains? % "EventType") events)
      (h p w)
      (error-response 400 "MFP" "All events should have 'EventType' key"))))

(defn check-extra-event-types [h]
  (fn [{:keys [event-id events] :as p} {:keys [session-storage] :as w}]
    (let [session (session/get-one session-storage event-id)
          event-types (session/extra-event-types session events)]
      (if (seq event-types)
        (error-response 400 "MFP" (format "Event types %s does not exists"
                                          (vec event-types)))
        (h p w)))))

(defn check-extra-attributes [h]
  (fn [{:keys [event-id events] :as p} {:keys [session-storage] :as w}]
    (let [session (session/get-one session-storage event-id)
          extra-attributes (session/extra-attributes session events)]
      (if (seq extra-attributes)
        (error-response 400 "MFP" (->> extra-attributes
                                       (map (fn [[et attrs]]
                                              (format "Event type \"%s\" does not have attributes %s"
                                                      et (vec attrs))))
                                       (str/join ";")))
        (h p w)))))

(defn check-valid-values [h]
  (fn [{:keys [event-id events] :as p} {:keys [session-storage] :as w}]
    (let [session (session/get-one session-storage event-id)
          invalid-values (session/invalid-values session events)]
      (if (seq invalid-values)
        (error-response 400 "MFP" (->> invalid-values
                                       (map (fn [[et attr val]]
                                              (format "Attribute \"%s\" of Event type \"%s\" has invalid value: \"%s\""
                                                      attr et val)))
                                       (str/join ";")))
        (h p w)))))

(def check-events
  (comp sequential-events
        at-least-1-event
        check-EventType-key
        check-extra-event-types
        check-extra-attributes
        check-valid-values))

(defn parse-out-filters [h]
  (fn [params w]
    (if-let [filters-map (json->clj (:filters params))]
      (h (assoc params :filters filters-map) w)
      (error-response 400 "MFP" "Malformed json body"))))

(defn filter-and-clean-out! [h]
  (fn [{:keys [filters event-id] :as r} {:keys [session-storage]}]
    (let [session (session/get-one session-storage event-id)]
      (session/clean-out! session filters)
      (success-response 200))))


