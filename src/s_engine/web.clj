(ns s-engine.web
  (:require [compojure.core :refer (defroutes ANY GET POST PUT DELETE wrap-routes)]
            [schema.core :as s]
            [ring.adapter.jetty :as jetty]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]
            [ring.middleware
             [params :refer (wrap-params)]
             [keyword-params :refer (wrap-keyword-params)]]
            [ring.util.request :as req]
            [ring.util.response :as res]
            [cheshire.core :as json]
            [malcolmx.core :as malx]
            [clojure.walk :refer [keywordize-keys]])
  (:import (org.eclipse.jetty.server Server)))

(defn error-response [status code message]
  {:status status
   :errors [{:code    code
             :message message}]})

(defn success-response [status result]
  {:status status
   :data   result})

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

(defn settlement-handler [req]
  req)

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

(defroutes routes
           (POST "/api/files/:model-id/:event-id/event-log/append" req (settlement-handler req))
           (ANY "/*" _ (response->json-response error-404-rnf)))

(defn app [web]
  (-> routes
      (wrap-keyword-params)
      (wrap-params)
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
