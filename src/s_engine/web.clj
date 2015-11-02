(ns s-engine.web
  (:require [s-engine.web.handlers :as hd]
            [compojure.core :refer
             (defroutes ANY GET POST PUT DELETE wrap-routes)]
            [schema.core :as s]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]
            [ring.middleware
             [params :refer (wrap-params)]
             [keyword-params :refer (wrap-keyword-params)]
             [multipart-params :refer (wrap-multipart-params)]]
            [ring.util.request :as req]
            [ring.adapter.jetty :as jetty]
            [s-engine.storage.file :as file])
  (:import (org.eclipse.jetty.server Server)))

(defn files-list
  [{{:keys [storage]} :web}]
  (->> (file/get-all storage)
       (hd/success-response 200)))

(def file-download
  (comp hd/parse-file-id
        hd/check-file-exists
        hd/download-file))

(def file-upload
  (comp hd/parse-file-id
        hd/check-file-present
        hd/check-file-type
        hd/check-file-validity
        hd/write-file!))

(def file-replace
  (comp hd/parse-file-id
        hd/check-file-exists
        hd/check-file-present
        hd/check-file-type
        hd/check-file-validity
        hd/write-file!))

(def file-delete
  (comp hd/parse-file-id
        hd/check-file-exists
        hd/delete-file!))

(def session-create
  (comp hd/check-event-id
        hd/check-session-not-exists
        hd/parse-file-id
        hd/check-file-exists
        hd/create-session!))

(def session-finalize
  (comp hd/check-session-exists
        hd/finalize-session!))

(def session-get-event-log
  (comp hd/check-event-id
        hd/check-session-exists
        hd/get-event-log))

(def session-append-event
  (comp hd/check-event-id
        hd/check-session-exists
        hd/parse-events
        hd/check-events
        hd/append-events!))

(def session-set-event-log
  (comp hd/check-event-id
        hd/check-session-exists
        hd/parse-events
        hd/check-events
        hd/set-events!))

(def session-get-settlements
  (comp hd/check-event-id
        hd/check-session-exists
        hd/get-settlements))

(def session-get-workbook
  (comp hd/check-event-id
        hd/check-session-exists
        hd/get-workbook))

(defroutes routes
  (GET "/files" req
    (files-list req))

  (GET "/files/:file-id" {:keys [web params]}
    (hd/call file-download params web))

  (POST "/files/:file-id/upload" {:keys [web params]}
    (hd/call file-upload params web))

  (POST "/files/:file-id" {:keys [web params]}
    (hd/call file-replace params web))

  (DELETE "/files/:file-id" {:keys [web params]}
    (hd/call file-delete params web))

  (POST "/files/:file-id/:event-id" {:keys [web params]}
    (hd/call session-create params web))

  (GET "/files/:file-id/:event-id" {:keys [web params]}
    (hd/call session-get-workbook params web))

  (DELETE "/files/:file-id/:event-id" {:keys [web params]}
    (hd/call session-finalize params web))

  (GET "/files/:file-id/:event-id/event-log" {:keys [web params]}
    (hd/call session-get-event-log params web))

  (POST "/files/:file-id/:event-id/event-log/append" {:keys [web params] :as r}
    (let [events (req/body-string r)]
      (hd/call session-append-event (assoc params :events events) web)))

  (POST "/files/:file-id/:event-id/event-log/set" {:keys [web params] :as r}
    (let [events (req/body-string r)]
      (hd/call session-set-event-log (assoc params :events events) web)))

  (GET "/files/:file-id/:event-id/settlements" {:keys [web params]}
    (hd/call session-get-settlements params web))

  (ANY "/*" _ (hd/error-response 404 "RNF" "Resource not found")))

(defn wrap-with-web [h web]
  (fn [req]
    (h (assoc req :web web))))

(defn wrap-errors [h]
  (fn [r]
    (try
      (h r)
      (catch Exception e
        (log/error e "while request handling")
        (hd/error-response 500 "ISE" "Internal server error")))))

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
