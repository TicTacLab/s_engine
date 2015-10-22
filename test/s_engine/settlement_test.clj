(ns s-engine.settlement-test
  (:require
    [clojure.test :refer :all]
    [malcolmx.core :as malx]
    [clojure.tools.trace :refer [trace]]))

(def test-excel "test.xlsx")
(def update-test-data [{"id"    1.0
                        "min"   "46"
                        "sec"   "36"
                        "event" "goal"
                        "team"  "T1"}
                       {"id"    1.0
                        "min"   "47"
                        "sec"   "36"
                        "event" "goal"
                        "team"  "T1"}])


(deftest read-out-test
  (is (= 3
         (-> (malx/parse test-excel)
             (malx/get-sheet "OUT")
             count)
         )))

(deftest event-log-append-test
  (is (nil?
        (-> (malx/parse test-excel)
            (malx/update-sheet! "EventLog" update-test-data :by "id")
            trace))))


