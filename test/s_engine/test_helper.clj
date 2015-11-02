(ns s-engine.test-helper
  (:require [com.stuartsierra.component :as component]
            [s-engine.config :as c]
            [s-engine.system :as s]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [clojure.string :as str]))

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

(defn url [& paths]
  (let [port (:port @c/config)
        path (str/join "/" paths)]
    (format "http://localhost:%d%s" port path)))

(defn file= [file1 file2]
  (let [digest1 ()]))

(defn load-test-file! []
  (let [file-resp (-> (url "/files" test-file-id "upload")
                      (http/post {:multipart [{:name     "file"
                                               :content  (io/file test-file)
                                               :filename "test-model.xlsx"}]})
                      (deref))]
    (assert (= (:status file-resp) 200) "Should upload file successfully!")))

