(ns s-engine.web
  (:require [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [compojure.core
             :refer
             (defroutes ANY GET POST PUT DELETE wrap-routes)]
            [schema.core :as s]
            [schema.coerce :as c]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]
            [ring.middleware
             [params :refer (wrap-params)]
             [keyword-params :refer (wrap-keyword-params)]
             [multipart-params :refer (wrap-multipart-params)]]
            [ring.util.request :as req]
            [ring.util.response :as res]
            [ring.adapter.jetty :as jetty]
            [cheshire.core :as json]
            [s-engine.session :as session]
            [s-engine.storage.file :as file]
            [clojure.java.io :as io]
            [malcolmx.core :as mx])
  (:import (org.eclipse.jetty.server Server)
           (java.io File)))


;;
;; Utils
;;

(defn new-error [status code message]
  {:status status
   :errors [{:code    code
             :message message}]})

(defn success-response
  ([status]
   (success-response status nil))
  ([status json-body]
   (-> {:status status
        :body   (when json-body
                  (json/generate-string {:status status
                                         :data   json-body}))}
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

(def error-400-mfp (error-response 400 "MFP" "Malformed body"))
(def error-404-fnf (error-response 404 "FNF" "File not found"))
(def error-404-rnf (error-response 404 "RNF" "Resource not found"))
(def error-423-cip (error-response 423 "CIP" "Calculation is in progress"))
(def error-500-ise (error-response 500 "ISE" "Internal server error"))

(defn try-string->json [value]
  (try
    (json/parse-string value false)
    (catch Exception _
      value)))

(defn wrap-with-web [h web]
  (fn [req]
    (h (assoc req :web web))))

(defn wrap-errors [h]
  (fn [r]
    (try
      (h r)
      (catch Exception e
        (log/error e "while request handling")
        error-500-ise))))

(defmacro resp->
  "Evaluates forms sequentially and returns first valid response. Returns last item if no response found.
  Does not inserts result of previous form as second item in next form."
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (nil? (:status ~g)) ~step ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep forms))]
       ~g)))

(defn check-event-id [h]
  (fn [{event-id :event-id :as p} w]
    (if (seq event-id)
      (h p w)
      (error-response 400 "MFP" "Invalid event id"))))

(defn- check-session-exists-leg [session-storage s-id]
  (when-not (session/exists? session-storage s-id)
    (log/info "Session with id not found" s-id)
    error-404-fnf))

(defn check-session-exists [h]
  (fn [{event-id :event-id :as p} {session-storage :session-storage :as w}]
    (if (session/exists? session-storage event-id)
      (h p w)
      (error-response 404 "SNF" (format "Session with id '%s' is not created" event-id)))))

(defn check-session-not-exists [h]
  (fn [{event-id :event-id :as p} {session-storage :session-storage :as w}]
    (if-not (session/exists? session-storage event-id)
      (h p w)
      (error-response 400 "MFP" (format "Session with id '%s' is already created" event-id)))))

(defn check-file-exists [h]
  (fn [{file-id :file-id :as p} {storage :storage :as w}]
    (if (file/exists? storage file-id)
      (h p w)
      (error-response 404 "FNF" (format "File with id '%s' not found" file-id)))))

(def ^:const event-schema
  {(s/required-key "EventType") s/Str
   (s/required-key "min")       s/Num
   (s/required-key "sec")       s/Num
   String s/Any})

(def ^:const events-schema
  [(s/one event-schema "e")
   event-schema])

(defn check-valid-events [events]
  (let [errors (s/check events-schema events)]
    (when errors
      (log/info "Events does not match schema" errors)
      error-400-mfp)))

;;
;; Routes
;;

(defn call [f obj web]
  (>trace ((f identity) obj web)))

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
  (fn [{:keys [file-id event-id]} {:keys [session-storage storage]}]
    (session/create! session-storage storage file-id event-id)
    (success-response 201)))

(defn finalize-session! [h]
  (fn [{:keys [event-id]} {:keys [session-storage storage]}]
    (let [session (session/get-one session-storage event-id)]
      (session/finalize! session-storage storage session)
      (success-response 204))))

(defn get-event-log [h]
  (fn [{:keys [event-id]} {:keys [session-storage]}]
    (->> (session/get-one session-storage event-id)
         (session/get-events)
         (success-response 200))))

(defn string->int [s]
  (try
    (Integer/valueOf s)
    (catch Exception e
      (println (.getLocalizedMessage e))
      nil)))

(defn parse-file-id [h]
  (fn [params web]
    (if-let [file-id (string->int (:file-id params))]
      (h (assoc params :file-id file-id) web)
      (error-response 400 "MFP" "Invalid file id"))))

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

(def file-upload
  (comp parse-file-id
        check-file-present
        check-file-type
        check-file-validity
        write-file!))

(def file-replace
  (comp parse-file-id
        check-file-exists
        check-file-present
        check-file-type
        check-file-validity
        write-file!))

(def file-delete
  (comp parse-file-id
        check-file-exists
        delete-file!))

(def session-create
  (comp check-event-id
        check-session-not-exists
        parse-file-id
        check-file-exists
        create-session!))

(def session-finalize
  (comp check-session-exists
        finalize-session!))

(def session-get-event-log
  (comp check-event-id
        check-session-exists
        get-event-log))

#_(defn session-get-event-log
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web}]
  (resp-> (check-session-exists-leg session-storage event-id)
          ))

(defn session-append-event
  [{{:keys [event-id]}                :params
    {:keys [session-storage storage]} :web :as r}]
  (let [event-str (req/body-string r)
        event (try-string->json event-str)]
    (resp-> (check-session-exists-leg session-storage event-id)
            (check-valid-events [event])
            (let [session (session/get-one session-storage event-id)]
              (session/append-event! storage session event)
              (success-response 200 (session/get-out session))))))

(defn session-set-event-log
  [{{:keys [event-id]}                :params
    {:keys [session-storage storage]} :web :as r}]
  (let [events-str (req/body-string r)
        events (try-string->json events-str)]
    (resp-> (check-session-exists-leg session-storage event-id)
            (check-valid-events events)
            (let [session (session/get-one session-storage event-id)]
              (session/set-events! storage session events)
              (success-response 200 (session/get-out session))))))

(defn session-get-settlements
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web}]
  (resp-> (check-session-exists-leg session-storage event-id)
          (->> (session/get-one session-storage event-id)
               (session/get-out)
               (success-response 200))))

(defn session-get-workbook
  [{{:keys [event-id]} :params
    {:keys [session-storage]} :web}]
  (resp-> (check-session-exists-leg session-storage event-id)
          (let [session (session/get-one session-storage event-id)
                {:keys [file-name bytes]} (session/get-workbook session)]
            (file-response bytes file-name))))

(defroutes routes
  (POST "/files/:file-id/upload" {:keys [web params]} (call file-upload params web))
  (POST "/files/:file-id" {:keys [web params]} (call file-replace params web))
  (DELETE "/files/:file-id" {:keys [web params]} (call file-delete params web))

  (POST "/files/:file-id/:event-id" {:keys [web params]} (call session-create params web))
  (GET "/files/:file-id/:event-id" req (session-get-workbook req))
  (DELETE "/files/:file-id/:event-id" {:keys [web params]} (call session-finalize params web))
  (GET "/files/:file-id/:event-id/event-log" {:keys [web params]} (call session-get-event-log params web))
  (POST "/files/:file-id/:event-id/event-log/append" req (session-append-event req))
  (POST "/files/:file-id/:event-id/event-log/set" req (session-set-event-log req))
  (GET "/files/:file-id/:event-id/settlements" req (session-get-settlements req))

  (ANY "/*" _ error-404-rnf))

(defn app [web]
  (-> routes
      (wrap-params)
      (wrap-keyword-params)
      (wrap-multipart-params)
      (wrap-with-web web)
      (wrap-errors)))

(defrecord Web [host port server storage api]
  component/Lifecycle

  (start [component]
    (let [srv (jetty/run-jetty (app component) {:port  port
                                                :host  host
                                                :join? false})]
      (log/info "Web service started at:" (str host ":" port))
      (assoc component :server srv)))

  (stop [component]
    (when server
      (.stop ^Server server)
      (log/info "Web service stopped"))
    (assoc component :server nil)))

(def WebSchema
  {:port s/Int
   :host s/Str})

(defn new-web [m]
  (as-> m $
        (select-keys $ (keys WebSchema))
        (s/validate WebSchema $)
        (map->Web $)))
