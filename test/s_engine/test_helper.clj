(ns s-engine.test-helper
  (:require [com.stuartsierra.component :as component]
            [clojure.test :refer :all]
            [s-engine.config :as c]
            [s-engine.system :as s]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [clojure.string :as str]
            [s-engine.web.handlers :as hd]
            [cheshire.core :as json])
  (:import (java.util UUID)))

(def ^:const test-file-id 1)
(def ^:const test-file "test/resources/AutoCalc_Soccer_EventLog.xlsx")
(def ^:const test-file2 "test/resources/AutoCalc_Soccer_EventLog_FOR_REPLACE.xlsx")
(def ^:const test-file3 "test/resources/AutoCalc_Soccer_EventLog1.xlsx")
(def ^:const invalid-file "test/resources/AutoCalc_Soccer_EventLog.invalid.xlsx")

(defmacro with-started-system
  [[sys-name config] & body]
  `(let [~sys-name (component/start (s/new-system (or ~config @c/config)))]
     (try
       ~@body
       (finally
         (component/stop ~sys-name)))))

(defn req!
  ([method url]
   (req! method url nil))
  ([method url body & [opts]]
   @(http/request (merge {:method method
                          :url    url
                          :body   body}
                         opts))))

(defn json-req!
  [method url body]
  (req! method url (json/generate-string body)))

(defn urlf [path-fmt & args]
  (let [port (:port @c/config)
        path (apply format path-fmt args)]
    (format "http://localhost:%d%s" port path)))

(defn load-test-file!
  ([]
   (load-test-file! test-file-id))
  ([file-id]
   (load-test-file! file-id test-file))
  ([file-id file-name]
   (let [file-resp (req! :post (urlf "/files/%s/upload" file-id) nil
                         {:multipart [{:name     "file"
                                       :content  (io/file file-name)
                                       :filename "test-model.xlsx"}]})]
     (when (not= (:status file-resp) 200)
       (throw (Exception. (format "Error during creating test file: %s" file-resp))))))
  )

(defn create-test-session! [file-id session-id]
  (let [resp (json-req! :post (urlf "/events") {:params {:file-id file-id,
                                                         :event-id session-id}})]
    (is (= 200 (:status resp)) "Should successfully create session!")))

(defn gen-session-id []
  (str (UUID/randomUUID)))

(defn gen-file-id []
  (mod (System/nanoTime) Integer/MAX_VALUE))

(defn empty-body [status]
  (:body (hd/success-response status)))



