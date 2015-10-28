(ns s-engine.test-helper
  (:require [com.stuartsierra.component :as component]
            [s-engine.config :as c]
            [s-engine.storage.file :as file]
            [s-engine.system :as s])
  (:import (java.io File)))

(def ^:const test-file-id 1)
(def ^:const test-file "test/resources/AutoCalc_Soccer_EventLog.xlsx")

(defmacro with-started-system
  [[sys-name config] & body]
  `(let [~sys-name (component/start (s/new-system (or ~config @c/config)))]
     (try
       ~@body
       (finally
         (component/stop ~sys-name)))))

(defn load-test-file!
  [{:keys [storage]}]
  (let [file-bytes (file/read-bytes (File. test-file))]
    (file/write! storage test-file-id file-bytes "test-model.xlsx")))

