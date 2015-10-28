(ns s-engine.storage.file-test
  (:require [clojure.test :refer :all]
            [s-engine.storage.file :refer :all]))

(def ^:const test-file
  (->File 1 "test/resources/AutoCalc_Soccer_EventLog.xlsx" ""))

(deftest get-event-log-sheet-test
  (let [file-wb (new-file-workbook test-file)
        event {"EventType" "Match"
               "min"       0
               "sec"       1
               "Action"    "start"}]
    (clear-event-log! file-wb)
    (append-events! file-wb [event])
    (is (= [{"EventType" "Match"
             "Team" ""
             "GamePart" ""
             "Standart" ""
             "BodyPart" ""
             "Accidental" ""
             "Action" "start"
             "min" 0.0
             "sec" 1.0}]
           (get-event-log-rows file-wb)))))

(deftest get-column-order-test
  (is (= []
         (get-column-order [])))
  (is (= ["Team" "Attribute"]
         (get-column-order [{"EventType" "Goal"
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
         (get-column-order [{"EventType" "Red Card"
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
        event {:type "Match"
               :min 0
               :sec 0
               :attrs {"Action" "start"}}]
    (append-events! file-wb [event])
    (clear-event-log! file-wb)
    (is (empty? (get-event-log-rows file-wb)))))

(deftest file-workbook-test
  (let [event-types {"Goal"
                     {"Team" #{"Team1" "Team2"}
                      "GamePart" #{"Half1" "Half2" "Extratime1" "Extratime2"
                                   "PenaltyShootOut"}
                      "Standart" #{"Corner" "Penalty" "FreeKick"}
                      "BodyPart" #{"Head" "Leg"}
                      "Accidental" #{"OwnGoal"}}
                     "Red Card"
                     {"Team" #{"Team1" "Team2"}
                      "GamePart" #{"Half1" "Half2" "Extratime1" "Extratime2"}}
                     "Yellow Card"
                     {"Team" #{"Team1" "Team2"}
                      "GamePart" #{"Half1" "Half2" "Extratime1" "Extratime2"}}
                     "Corner"
                     {"Team" #{"Team1" "Team2"}
                      "GamePart" #{"Half1" "Half2" "Extratime1" "Extratime2"}}
                     "Penalty"
                     {"Team" #{"Team1" "Team2"}
                      "GamePart" #{"Half1" "Half2" "Extratime1" "Extratime2"
                                   "PenaltyShootOut"}}
                     "Half1" {"Action" #{"start" "stop"}}
                     "Half2" {"Action" #{"start" "stop"}}
                     "Extratime1" {"Action" #{"start" "stop"}}
                     "Extratime2" {"Action" #{"start" "stop"}}
                     "PenaltyShootOut" {"Action" #{"start" "stop"}}
                     "break" {"Action" #{"start" "stop"}}
                     "FullTime" {"Action" #{"start" "stop"}}
                     "Match" {"Action" #{"start" "stop"}}}
        column-order ["Team"
                      "GamePart"
                      "Standart"
                      "BodyPart"
                      "Accidental"
                      "Action"]]
    (is (= (->FileWorkbook :bytes event-types column-order)
           (-> (new-file-workbook test-file)
               (assoc :workbook :bytes))))))

(deftest finalize-test
  (let [file-wb (new-file-workbook test-file)]
    ;; java.io.Closeable does not have "isClosed" or similar
    (finalize! file-wb)))

(deftest event->row-data-test
  (let [column-order ["Attr1" "Action" "Attr2"]
        event {"EventType" "Match"
               "min"       0
               "sec"       0
               "Action"    "start"}]
    (is (= [0 0 "Match" "" "start" ""]
           (event->row-data column-order event)))))

(deftest append-events!-test
  (testing "Consecutive appends"
    (let [file-wb (new-file-workbook test-file)
          event {"EventType" "Match"
                 "min"       0.
                 "sec"       0.
                 "Team"       ""
                 "GamePart"   ""
                 "Standart"   ""
                 "BodyPart"   ""
                 "Accidental" ""
                 "Action"    "start"}]
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
          event1 {"EventType" "Match"
                  "min"       0.
                  "sec"       1.
                  "Team"       ""
                  "GamePart"   ""
                  "Standart"   ""
                  "BodyPart"   ""
                  "Accidental" ""
                  "Action"    "start"}
          event2 {"EventType" "Match"
                  "min"       0.
                  "sec"       2.
                  "Team"       ""
                  "GamePart"   ""
                  "Standart"   ""
                  "BodyPart"   ""
                  "Accidental" ""
                  "Action"    "end"}]
      (append-events! file-wb [event1 event2])
      (is (= [event1 event2]
             (get-event-log-rows file-wb))))))

(deftest set-event-log-sheet!-test
  (let [file-wb (new-file-workbook test-file)
        event1 {"EventType"  "Match"
                "min"        0.
                "sec"        0.
                "Team"       ""
                "GamePart"   ""
                "Standart"   ""
                "BodyPart"   ""
                "Accidental" ""
                "Action"     "start"}
        event2 {"EventType"  "Match"
                "min"        1.
                "sec"        0.
                "Team"       ""
                "GamePart"   ""
                "Standart"   ""
                "BodyPart"   ""
                "Accidental" ""
                "Action"     "start"}]
    (append-events! file-wb [event1])
    (set-event-log! file-wb [event2])
    (is (= [event2]
           (get-event-log-rows file-wb)))))