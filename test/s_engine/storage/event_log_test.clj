(ns s-engine.storage.event-log-test
  (:require [clojure.test :refer :all]
            [s-engine.storage.event-log :as event-log]
            [s-engine.test-helper :refer :all]
            [clojurewerkz.cassaforte.query :refer [batch-query]])
  (:import (java.util UUID)))

(deftest clear-test
  (with-started-system [system]
    (let [{:keys [storage]} system
          session-id (str (UUID/randomUUID))]
      (event-log/clear! storage session-id)
      (is (empty? (event-log/fetch storage session-id))))))

(deftest refresh-test
  (with-started-system [system]
    (let [{:keys [storage]} system
          events [{"EventType" "test-log-row1"}
                  {"EventType" "test-log-row2"}]
          session-id (str (UUID/randomUUID))]
      (event-log/refresh! storage session-id events)
      (is (= events
             (event-log/fetch storage session-id)))
      (event-log/clear! storage session-id))))

(deftest append-test
  (with-started-system [system]
    (let [{:keys [storage]} system
          events [{"EventType" "test-log-row1"}
                  {"EventType" "test-log-row2"}]
          session-id (str (UUID/randomUUID))]
      (event-log/append! storage session-id events)
      (is (= events
             (event-log/fetch storage session-id)))
      (event-log/clear! storage session-id))))

