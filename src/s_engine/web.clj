(ns s-engine.web
  (:require [compojure.core :refer (defroutes ANY GET POST PUT DELETE wrap-routes)]
            [schema.core :as s]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]
            [formative.parse :as fp]
            [ring.middleware
             [params :refer (wrap-params)]
             [keyword-params :refer (wrap-keyword-params)]
             [multipart-params :refer (wrap-multipart-params)]]
            [ring.util.request :as req]
            [ring.util.response :as res]
            [ring.adapter.jetty :as jetty]
            [cheshire.core :as json]
            [clojure.walk :refer [keywordize-keys]]
            [s-engine.session :as session]
            [s-engine.form :refer [upload-model-form]]
            [s-engine.storage.model :as model])
  (:import (org.eclipse.jetty.server Server)))

;;
;; Utils
;;

(defn success-response
  ([status]
    (success-response status nil))
  ([status json-body]
   {:status status
    :body   (when json-body
              (json/generate-string {:status status
                                     :data json-body}))}))

(defn error-response [status code message]
  {:status status
   :body (json/generate-string {:status status
                                :errors [{:code code
                                          :message message}]})})

(def error-400-mfp (error-response 400 "MFP" "Malformed body"))
(def error-404-fnf (error-response 404 "FNF" "File not found"))
(def error-404-rnf (error-response 404 "RNF" "Resource not found"))
(def error-423-cip (error-response 423 "CIP" "Calculation is in progress"))
(def error-500-ise (error-response 500 "ISE" "Internal server error"))

(defn try-string->json [value]
  (try
    (json/parse-string value true)
    (catch Exception _
      value)))

(defn wrap-with-web [h web]
  (fn [req]
    (h (assoc req :web web))))

(defn wrap-json-content-type [h]
  (fn [req]
    (-> req
        (h)
        (res/content-type "application/json")
        (res/charset "utf-8"))))

(defmulti handle-error :type)

(defmethod handle-error ::model/not-found
  [{:keys [id]}]
  (log/info "Model not found: %s" id)
  error-404-fnf)

(defmethod handle-error ::s/error
  [{:keys [error value schema]}]
  (log/info (format "Validation failed: %s does not match %s. Errors: %s" value schema error))
  error-400-mfp)

(defmethod handle-error :exception
  [{:keys [exception]}]
  (log/error exception)
  error-500-ise)

(defn wrap-errors [h]
  (fn [r]
    (try
      (h r)
      (catch clojure.lang.ExceptionInfo ex
        (handle-error (ex-data ex)))
      (catch Exception ex
        (handle-error {:type :exception :exception ex})))))

;;
;; Routes
;;

(defn model-upload
  [{{:keys [storage]} :web :as r}]
  (fp/with-fallback
    (fn [pr]
      (log/error pr)
      (error-response 400 "MFP" "Error"))
    (let [{:keys [id file]} (fp/parse-request upload-model-form r)
          {:keys [filename tempfile]} file]
      (model/write-model! storage id tempfile filename)
      (success-response 200))))

(defn model-delete
  [{{:keys [model-id]} :params
    {:keys [storage]}  :web}]
  (when (model/model-exists? storage model-id)
    (model/delete-model! storage model-id))
  (success-response 204 ""))

(defn session-get-event-log
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)
        events (session/get-events session)]
    (success-response 200 events)))

(def ^:const event-schema
  {:event-type s/Str
   :min s/Int
   :sec s/Int
   :attrs [{:name s/Str
            :value s/Str}]})

(defn session-append-event
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web :as r}]
  (let [params (try-string->json (req/body-string r))
        event (:event params)
        session (session/get-or-create! session-storage storage model-id event-id)]
    (session/append-event! session event)
    (success-response 200)))

(defn session-set-event-log
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)]
    (success-response 200 (session/get-events session))))

(defn session-get-settlements
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)]
    (->> (session/get-out session) >trace
         (success-response 200))))

(defn session-finalize
  [{{:keys [event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-one session-storage event-id)]
    (when session
      (session/finalize session-storage storage session))
    (success-response 204)))

(defroutes routes
  (POST "/files/upload" req (model-upload req))
  (DELETE "/files/:model-id" req (model-delete req))

  (GET "/files/:model-id/:event-id/event-log" req (session-get-event-log req))
  (POST "/files/:model-id/:event-id/event-log/append" req (session-append-event req))
  (POST "/files/:model-id/:event-id/event-log/set" req (session-set-event-log req))
  (GET "/files/:model-id/:event-id/settlements" req (session-get-settlements req))
  (DELETE "/files/:model-id/:event-id" req (session-finalize req))

  (ANY "/*" _ error-404-rnf))

(defn app [web]
  (-> routes
      (wrap-params)
      (wrap-keyword-params)
      (wrap-multipart-params)
      (wrap-with-web web)
      (wrap-errors)
      (wrap-json-content-type)))

(defrecord Web [host port server storage api]
  component/Lifecycle

  (start [component]
    (let [srv (jetty/run-jetty (app component) {:port port
                                                :host host
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
