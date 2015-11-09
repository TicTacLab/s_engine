(ns s-engine.storage.file-test
  (:require [clojure.test :refer :all]
            [s-engine.storage.file :refer :all :as file])
  (:import (java.util Date)))

(def ^:const test-file
  (->File 1 "test/resources/AutoCalc_Soccer_EventLog.xlsx" "AutoCalc_Soccer_EventLog.xlsx" (Date.)))

(deftest get-event-log-sheet-test
  (let [file-wb (new-file-workbook test-file)
        event {"EventType" "Match"
               "Action"    "start"}]
    (clear-event-log! file-wb)
    (append-events! file-wb [event])
    (is (= [{"EventType"  "Match"
             "Action"     "start"}]
           (get-event-log-rows file-wb)))))

(deftest validate-columns-test
  (is (= [#{"Attr1"} nil]
         (validate-columns ["Attr1"] ["EventType"])))
  (is (= [#{"EventType"} nil]
         (validate-columns ["Attr1"] ["Attr1"])))
  (is (= [nil #{"Attr1"}]
         (validate-columns [] ["EventType" "Attr1"])))
  (is (= [#{"EventType"} #{"Attr3"}]
         (validate-columns ["Attr1" "Attr2"] ["Attr1" "Attr2" "Attr3"])))
  (is (= [nil nil] (validate-columns ["Attr1" "Attr2"] ["EventType" "Attr1" "Attr2"])))
  (is (= [nil nil] (validate-columns ["Attr1" "Attr2"] ["EventType" "Attr1" "Attr2"]))))

(deftest get-event-type-attrs-test
  (is (= []
         (get-event-type-attrs [])))
  (is (= ["Team" "Attribute"]
         (get-event-type-attrs [{"EventType" "Goal"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team1"}
                                {"EventType" "Goal"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team2"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Attribute"
                                 "MetaValue" "True"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team1"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team2"}])))
  (is (= ["Game Part" "Attribute" "Team"]
         (get-event-type-attrs [{"EventType" "Red Card"
                                 "MetaKey"   "Game Part"
                                 "MetaValue" "Half2"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Attribute"
                                 "MetaValue" "True"}
                                {"EventType" "Goal"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team1"}
                                {"EventType" "Goal"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team2"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Team"
                                 "MetaValue" "Team1"}
                                {"EventType" "Red Card"
                                 "MetaKey"   "Game Part"
                                 "MetaValue" "Half1"}]))))

(deftest get-event-types-test
  (is (= {}
         (get-event-types []))))

(deftest clear-event-log!-test
  (let [file-wb (new-file-workbook test-file)
        event {:type  "Match"
               :min   0
               :sec   0
               :attrs {"Action" "start"}}]
    (append-events! file-wb [event])
    (clear-event-log! file-wb)
    (is (empty? (get-event-log-rows file-wb)))))


(deftest finalize-test
  (let [file-wb (new-file-workbook test-file)]
    ;; java.io.Closeable does not have "isClosed" or similar
    (finalize! file-wb)))

(deftest event->row-data-test
  (let [column-order ["EventType" "Attr1" "Action" "Attr2"]
        event {"EventType" "Match"
               "Action"    "start"}]
    (is (= ["Match" "" "start" ""]
           (event->row-data column-order event)))))

(deftest append-events!-test
  (testing "Consecutive appends"
    (let [file-wb (new-file-workbook test-file)
          event {"EventType"  "Match"
                 "Action"     "start"}]
      (append-events! file-wb [event])
      (append-events! file-wb [event])
      (is (= [event event]
             (get-event-log-rows file-wb))))))

(deftest get-event-log-test
  (testing "Empty event log"
    (let [file-wb (new-file-workbook test-file)]
      (clear-event-log! file-wb)
      (is (empty? (get-event-log-rows file-wb)))))
  (testing "Not empty"
    (let [file-wb (new-file-workbook test-file)
          event1 {"EventType"  "Match"
                  "Action"     "start"}
          event2 {"EventType"  "Match"
                  "Action"     "end"}]
      (append-events! file-wb [event1 event2])
      (is (= [event1 event2]
             (get-event-log-rows file-wb))))))

(deftest set-event-log-sheet!-test
  (let [file-wb (new-file-workbook test-file)
        event1 {"EventType"  "Match"
                "Action"     "start"}
        event2 {"EventType"  "Match"
                "Action"     "start"}]
    (append-events! file-wb [event1])
    (set-event-log! file-wb [event2])
    (is (= [event2]
           (get-event-log-rows file-wb)))))