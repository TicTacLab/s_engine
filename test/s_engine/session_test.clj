(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [s-engine.test-helper :refer :all]
            [s-engine.storage.event-log :as ev])
  (:import (java.util UUID)))

;;
;; Tests
;;

(deftest create-test-without-logs
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)]
      (is (= session
             (get @(:session-table session-storage) session-id)))
      (is (empty? (get-events session))))))

(deftest create-test-with-logs
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          event {"EventType"  "Test"
                 "Team"       ""
                 "BodyPart"   ""
                 "GamePart"   ""
                 "Standart"   ""
                 "Accidental" ""
                 "Action"     ""}
          session-id (str (UUID/randomUUID))]
      (ev/refresh! storage session-id [event])
      (let [session (create! session-storage storage test-file-id session-id)]
        (is (= session
               (get @(:session-table session-storage) session-id)))
        (is (= [event]
               (get-events session)))))))

(deftest get-one-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)]
      (is (= session
             (get-one session-storage session-id))))))

(deftest append-event-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)
          event {"EventType"  "Goal"
                 "Team"       "Team1"
                 "GamePart"   "Half1"
                 "Standart"   "Corner"
                 "BodyPart"   "Leg"
                 "Accidental" "OwnGoal"
                 "Action"     ""}]
      (append-events! storage session [event])
      (is (= [event]
             (get-events session)))
      (is (= [event]
             (ev/fetch storage session-id))))))

(deftest get-events-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)
          event1 {"EventType"  "Goal"
                  "Team"       "Team1"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Leg"
                  "Accidental" "OwnGoal"
                  "Action"     ""}
          event2 {"EventType"  "Goal"
                  "Team"       "Team2"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Leg"
                  "Accidental" "OwnGoal"
                  "Action"     ""}]
      (do
        (is (empty? (get-events session)))
        (append-events! storage session [event1])
        (is (= [event1] (get-events session)))
        (append-events! storage session [event2])
        (is (= [event1 event2] (get-events session)))))))

(deftest set-events!-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          event1 {"EventType"  "Goal"
                  "Team"       "Team1"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Head"
                  "Accidental" "OwnGoal"
                  "Action"     ""}
          event2 {"EventType"  "Goal"
                  "Team"       "Team2"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Head"
                  "Accidental" "OwnGoal"
                  "Action"     ""}
          session-id (>trace (str (UUID/randomUUID)))
          session (create! session-storage storage test-file-id session-id)]
      (append-events! storage session [event1])
      (set-events! storage session [event2])
      (is (= [event2]
             (get-events session)))
      (is (= [event2]
             (ev/fetch storage session-id))))))

(deftest get-out-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)]
      (is (= [{"Market name" "MATCH_BETTING"
               "Outcome"     "HOME"
               "Calc"        "lose"}
              {"Market name" "MATCH_BETTING"
               "Outcome"     "DRAW"
               "Calc"        "win"}
              {"Market name" "MATCH_BETTING"
               "Outcome"     "AWAY"
               "Calc"        "lose"}]
             (get-out session))))))

(deftest finalize-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))
          session (create! session-storage storage test-file-id session-id)
          event {"EventType"  "Goal"
                 "min"        0.
                 "sec"        0.
                 "Team"       "Team1"
                 "GamePart"   "Half1"
                 "Standart"   "Corner"
                 "BodyPart"   "Head"
                 "Accidental" "OwnGoal"
                 "Action"     ""}]
      (append-events! storage session [event])
      (finalize! session-storage storage session)
      (is (nil? (get-one session-storage session-id)))
      (is (= (ev/fetch storage session-id))))))