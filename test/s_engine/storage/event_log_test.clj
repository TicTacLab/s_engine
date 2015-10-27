(ns s-engine.storage.event-log-test
  (:require [clojure.test :refer :all]
            [s-engine.storage.event-log :as event-log]
            [s-engine.system :as s]
            [com.stuartsierra.component :as component]
            [s-engine.config :as c]
            [clojurewerkz.cassaforte.query :refer [batch-query]]))

(def ^:const event-id "1")


(def system nil)

(defn start-system []
  (alter-var-root #'system (constantly (s/new-system @c/config)))
  (alter-var-root #'system component/start))

(defn stop-system []
  (when system
    (alter-var-root #'system component/stop)))

(defn wrap-with-system [f]
  (try
    (stop-system)
    (start-system)
    (f)
    (finally
      (stop-system))))
(use-fixtures :each wrap-with-system)

(deftest clear-test
  (let [{:keys [storage]} system]
    (event-log/clear! storage event-id)
    (is (= []
           (event-log/fetch storage event-id)))))

(deftest refresh-test
  (let [{:keys [storage]} system]
    (event-log/refresh! storage event-id ["test-log-row1" "test-log-row2"])
    (is (= [["test-log-row1" "test-log-row2"]]
           (event-log/fetch storage event-id)))))

(deftest append-test
  (let [{:keys [storage]} system]
    (event-log/append! storage event-id ["test-log-row3" "test-log-row4"])
    (is (= [["test-log-row1" "test-log-row2" "test-log-row3" "test-log-row4"]]
           (event-log/fetch storage event-id)))
    (event-log/clear! storage event-id)))

