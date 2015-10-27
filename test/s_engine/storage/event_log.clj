(ns s-engine.storage.event-log
  (:require [clojure.test :refer :all]
            [s-engine.storage.event-log :as event-log]))

(def ^:const event-id 1)


(deftest append-fetch-delete-test
  (event-log/append! storage event-id ["test-log-row1"])
  (is (= ["test-log-row1"]
         (event-log/fetch storage event-id)))
  (event-log/append! storage event-id ["test-log-row2"])
  (is (= ["test-log-row1" "test-log-row2"]
         (event-log/fetch storage event-id)))
  (event-log/reset! storage event-id ["test-log-row3" "test-log-row4"])
  (is (= ["test-log-row3" "test-log-row4"]
         (event-log/fetch storage event-id)))
  (event-log/clear! storage event-id)
  (is (= []
         (event-log/fetch storage event-id))))
