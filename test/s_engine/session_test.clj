(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [com.stuartsierra.component :as component]))

(def ^:const test-workbook "test/resources/AutoCalc_Soccer_EventLog.xlsx")

(defn make-storage []
  (-> (new-session-storage)
      (component/start)))

(deftest create-test
  (let [storage (make-storage)
        session (create! storage {:id "model1", :file test-workbook} "session1")]
    (is (= session
           (get @(:session-table storage) "session1")))))

(deftest get-one-test
  (let [storage (make-storage)
        session (create! storage {:id "model1", :file test-workbook} "session1")]
    (is (= session
           (get-one storage "session1")))))

(deftest append-event-test
  (let [storage (make-storage)
        session (create! storage {:id "model1", :file test-workbook} "session1")
        event {"EventType"  "Goal"
               "min"        0.
               "sec"        0.
               "Team"       "Team1"
               "GamePart"   "Half1"
               "Standart"   "Corner"
               "BodyPart"   "Leg"
               "Accidental" "OwnGoal"
               "Action" ""}]
    (append-event! session event)
    (is (= [event]
           (get-events session)))))

(deftest get-events-test
  (let [storage (make-storage)
        session (create! storage {:id "model1", :file test-workbook} "session1")
        event1 {"EventType"  "Goal"
                "min"        0.
                "sec"        0.
                "Team"       "Team1"
                "GamePart"   "Half1"
                "Standart"   "Corner"
                "BodyPart"   "Leg"
                "Accidental" "OwnGoal"
                "Action" ""}
        event2 {"EventType"  "Goal"
                "min"        1.
                "sec"        0.
                "Team"       "Team2"
                "GamePart"   "Half1"
                "Standart"   "Corner"
                "BodyPart"   "Leg"
                "Accidental" "OwnGoal"
                "Action" ""}]
    (do
      (is (empty? (get-events session)))
      (append-event! session event1)
      (is (= [event1] (get-events session)))
      (append-event! session event2)
      (is (= [event1 event2] (get-events session))))))

(deftest get-out-test
  (let [storage (make-storage)
        session (create! storage {:id "model1", :file test-workbook} "session1")]
    (is (= [{"Market name" "MATCH_BETTING"
             "Outcome" "HOME"
             "Calc" "lose"}
            {"Market name" "MATCH_BETTING"
             "Outcome" "DRAW"
             "Calc" "win"}
            {"Market name" "MATCH_BETTING"
             "Outcome" "AWAY"
             "Calc" "lose"}]
           (get-out session)))))