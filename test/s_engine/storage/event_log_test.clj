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
    (start-system)
    (f)
    (finally
      (stop-system))))
(use-fixtures :each wrap-with-system)

(deftest clear-test
  (let [{:keys [storage]} system]
    (event-log/clear! storage event-id)
    (is (empty? (event-log/fetch storage event-id)))))

(deftest refresh-test
  (let [{:keys [storage]} system
        events [{"EventType" "test-log-row1"}
                {"EventType" "test-log-row2"}]]
    (event-log/refresh! storage event-id events)
    (is (= events
           (event-log/fetch storage event-id)))
    (event-log/clear! storage event-id)))

(deftest append-test
  (let [{:keys [storage]} system
        events [{"EventType" "test-log-row1"}
                {"EventType" "test-log-row2"}]]
    (event-log/append! storage event-id events)
    (is (= events
           (event-log/fetch storage event-id)))
    (event-log/clear! storage event-id)))

