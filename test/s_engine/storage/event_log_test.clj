(ns s-engine.storage.event-log-test
  (:require [clojure.test :refer :all]
            [s-engine.storage.event-log :as event-log]
            [s-engine.test-helper :refer :all]
            [clojurewerkz.cassaforte.query :refer [batch-query]]))

(def ^:const event-id "1")

(deftest clear-test
  (with-started-system [system]
    (let [{:keys [storage]} system]
     (event-log/clear! storage event-id)
     (is (empty? (event-log/fetch storage event-id))))))

(deftest refresh-test
  (with-started-system [system]
    (let [{:keys [storage]} system
         events [{"EventType" "test-log-row1"}
                 {"EventType" "test-log-row2"}]]
     (event-log/refresh! storage event-id events)
     (is (= events
            (event-log/fetch storage event-id)))
     (event-log/clear! storage event-id))))

(deftest append-test
  (with-started-system [system]
    (let [{:keys [storage]} system
         events [{"EventType" "test-log-row1"}
                 {"EventType" "test-log-row2"}]]
     (event-log/append! storage event-id events)
     (is (= events
            (event-log/fetch storage event-id)))
     (event-log/clear! storage event-id))))

