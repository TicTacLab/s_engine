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
                                     :data   json-body}))}))

(defn error-response [status code message]
  {:status status
   :body   (json/generate-string {:status status
                                  :errors [{:code    code
                                            :message message}]})})

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

(defn wrap-json-content-type [h]
  (fn [req]
    (-> req
        (h)
        (res/content-type "application/json")
        (res/charset "utf-8"))))

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

(defn- check-event-id [e-id]
  (when (empty? e-id)
    (log/info "Invalid event id" e-id)
    error-400-mfp))

(defn- check-model-id [m-id]
  )

(defn- check-session-exists [session-storage s-id]
  (when-not (session/exists? session-storage s-id)
    (log/info "Session with id not found" s-id)
    error-404-fnf))

(defn check-session-not-exists [session-storage s-id]
  (when (session/exists? session-storage s-id)
    (log/info "Session with id already created" s-id)
    error-400-mfp))

(defn- check-model-exists [storage m-id]
  (when-not (model/exists? storage m-id)
    (log/info "Model with id not found" m-id)
    error-404-fnf))

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
  (resp-> (check-model-exists storage model-id)
          (do
            (model/delete-model! storage model-id)
            (success-response 204))))

(defn session-create
  [{{:keys [event-id] :as params}     :params
    {:keys [storage session-storage]} :web}]
  (let [model-id (try-string->json (:model-id params))]
    (resp-> (check-event-id event-id)
            (check-session-not-exists session-storage event-id)
            (check-model-id model-id)
            (check-model-exists storage model-id)
            (let [model (model/get-model storage model-id)]
              (session/create! session-storage model event-id)
              (success-response 201)))))

(defn session-finalize
  [{{:keys [event-id]}                :params
    {:keys [session-storage storage]} :web}]
  (resp-> (check-session-exists session-storage event-id)
          (let [session (session/get-one session-storage event-id)]
            (session/finalize session-storage storage session)
            (success-response 204))))

(defn session-get-event-log
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web}]
  (resp-> (check-session-exists session-storage event-id)
          (->> (session/get-one session-storage event-id)
               (session/get-events)
               (success-response 200))))

(defn session-append-event
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web :as r}]
  (let [event (try-string->json (req/body-string r))]
    (resp-> (check-session-exists session-storage event-id)
            (check-valid-events [event])
            (let [session (session/get-one session-storage event-id)]
              (session/append-event! session event)
              (success-response 200 (session/get-out session))))))

(defn session-set-event-log
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web :as r}]
  (let [events (try-string->json (req/body-string r))]
    (resp-> (check-session-exists session-storage event-id)
            (check-valid-events events)
            (let [session (session/get-one session-storage event-id)]
              (session/set-events! session events)
              (success-response 200 (session/get-out session))))))

(defn session-get-settlements
  [{{:keys [event-id]}        :params
    {:keys [session-storage]} :web}]
  (resp-> (check-session-exists session-storage event-id)
          (->> (session/get-one session-storage event-id)
               (session/get-out)
               (success-response 200))))

(defroutes routes
           (POST "/files/upload" req (model-upload req))
           (DELETE "/files/:model-id" req (model-delete req))

           (POST "/files/:model-id/:event-id" req (session-create req))
           (DELETE "/files/:model-id/:event-id" req (session-finalize req))
           (GET "/files/:model-id/:event-id/event-log" req (session-get-event-log req))
           (POST "/files/:model-id/:event-id/event-log/append" req (session-append-event req))
           (POST "/files/:model-id/:event-id/event-log/set" req (session-set-event-log req))
           (GET "/files/:model-id/:event-id/settlements" req (session-get-settlements req))

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
