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

(defn error-response [status code message]
  {:status status
   :errors [{:code    code
             :message message}]})

(defn success-response [status result]
  {:status status
   :data   result})

(def error-400-mfp (error-response 400 "MFP" "Malformed body"))
(def error-404-fnf (error-response 404 "FNF" "File not found"))
(def error-404-rnf (error-response 404 "RNF" "Resource not found"))
(def error-423-cip (error-response 423 "CIP" "Calculation is in progress"))
(def error-500-ise (error-response 500 "ISE" "Internal server error"))

(defn return-with-log [value msgf & args]
  (log/info (apply format (cons msgf args)))
  value)

(defn response->json-response [json]
  {:status (:status json)
   :body (json/generate-string json)})

(defn try-string->json [value]
  (try
    (json/parse-string value true)
    (catch Exception _
      value)))

(defn wrap-with-web [h web]
  (fn [req]
    (h (assoc req :web web))))

(defn wrap-internal-server-error [h]
  (fn [req]
    (try
      (h req)
      (catch Exception e
        (log/error e "while request handling")
        (response->json-response error-500-ise)))))

(defn wrap-json-content-type [h]
  (fn [req]
    (-> req
        (h)
        (res/content-type "application/json")
        (res/charset "utf-8"))))

;;
;; Routes
;;

(defn model-upload
  [{{:keys [storage]} :web :as r}]
  (response->json-response
    (fp/with-fallback
      (fn [pr]
        (log/error pr)
        (error-response 400 "MFP" "Error"))
     (let [{:keys [id file]} (fp/parse-request upload-model-form r)
           {:keys [filename tempfile]} file]
       (model/write-model! storage id tempfile filename)
       (success-response 200 "")))))

(defn model-delete
  [{{:keys [model-id]} :params
    {:keys [storage]}  :web}]
  (model/delete-model! storage model-id)
  (->> (success-response 204 "")
       (response->json-response)))

(defn session-get-event-log
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)
        events (session/get-events session)]
    (->> events
         (success-response 200)
         (response->json-response))))

(def ^:const event-schema
  {:event-type s/Str
   :min s/Int
   :sec s/Int
   :attrs [{:name s/Str
            :value s/Str}]})

(defn session-append-event
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web :as r}]
  (let [event (try-string->json (req/body-string r))
        session (session/get-or-create! session-storage storage model-id event-id)]
    (s/validate event-schema event)
    (session/append-event! session event)
    (->> (success-response 200 "")
         (response->json-response))))

(defn session-set-event-log
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)]
    (->> (session/get-events session)
         (success-response 200)
         (response->json-response))))

(defn session-get-settlements
  [{{:keys [model-id event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-or-create! session-storage storage model-id event-id)]
    (->> (session/get-out session)
         (success-response 200)
         (response->json-response))))

(defn session-finalize
  [{{:keys [event-id]} :params
    {:keys [session-storage storage]} :web}]
  (let [session (session/get-one session-storage event-id)]
    (when session
      (session/finalize session-storage storage session))
    (->> (success-response 204 "")
         (response->json-response))))

(defroutes routes
  (POST "/api/files/upload" req (model-upload req))
  (DELETE "/api/files/:model-id" req (model-delete req))

  (GET "/api/files/:model-id/:event-id/event-log" req (session-get-event-log req))
  (POST "/api/files/:model-id/:event-id/event-log/append" req (session-append-event req))
  (POST "/api/files/:model-id/:event-id/event-log/set" req (session-set-event-log req))
  (GET "/api/files/:model-id/:event-id/settlements" req (session-get-settlements req))
  (DELETE "/api/files/:model-id/:event-id/" req (session-finalize req))

  (ANY "/*" _ (response->json-response error-404-rnf)))

(defn app [web]
  (-> routes
      (wrap-params)
      (wrap-keyword-params)
      (wrap-multipart-params)
      (wrap-with-web web)
      (wrap-internal-server-error)
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
