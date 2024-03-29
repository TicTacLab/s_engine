(ns s-engine.client
  (:require [s-engine.test-helper :refer :all]
            [clojure.java.io :as io]))

(defn check-error [res]
  (if-let [error (:error res)]
    (throw (Exception. (println "Error!" error)))
    (if (not= 200 (:status res))
      (throw (Exception. (pr-str res)))
      res)))

(defn upload-file [file-id file]
  (check-error
    (req! :post (urlf "/files/%s/upload" file-id) nil
          {:multipart [{:name     "file"
                        :content  (io/file file)
                        :filename (str file-id)}]})))

(defn create-session [file-id session-id]
  (check-error
    (json-req! :post (urlf "/events")
               {:params {:file-id  file-id
                         :event-id session-id}})))

(defn append-event [session-id event]
  (check-error
    (json-req! :post (urlf "/events/%s/event-log/append" session-id)
               {:params [event]})))

(defn get-settlements [session-id]
  (check-error
    (req! :get (urlf "/events/%s/settlements" session-id))))

(defn set-events [session-id events]
  (check-error
    (json-req! :post (urlf "/events/%s/event-log/set" session-id)
               {:params events})))