(ns s-engine.test-helper
  (:require [com.stuartsierra.component :as component]
            [clojure.test :refer :all]
            [s-engine.config :as c]
            [s-engine.system :as s]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [clojure.string :as str]
            [s-engine.web.handlers :as hd])
  (:import (java.util UUID)))

(def ^:const test-file-id 1)
(def ^:const test-file "test/resources/AutoCalc_Soccer_EventLog.xlsx")
(def ^:const test-file2 "test/resources/AutoCalc_Soccer_EventLog_FOR_REPLACE.xlsx")
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

(defn urlf [path-fmt & args]
  (let [port (:port @c/config)
        path (apply format path-fmt args)]
    (format "http://localhost:%d%s" port path)))

(defn load-test-file! []
  (let [file-resp (req! :post (urlf "/files/%s/upload" test-file-id) nil
                         {:multipart [{:name     "file"
                                       :content  (io/file test-file)
                                       :filename "test-model.xlsx"}]})]
    (is (= 200 (:status file-resp)) "Should upload file successfully!")))

(defn create-test-session! [session-id]
  (let [resp (req! :post (urlf "/files/%s/%s" test-file-id session-id))]
    (is (= 200 (:status resp)) "Should successfully create session!")))

(defn gen-session-id []
  (str (UUID/randomUUID)))

(defn empty-body [status]
  (:body (hd/success-response status)))



