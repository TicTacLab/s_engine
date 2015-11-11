(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [s-engine.test-helper :refer :all]
            [s-engine.storage.event-log :as ev]))

;;
;; Tests
;;

(deftest create-test-without-logs
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (let [{:keys [session-storage storage]} system
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id "test event")]
        (is (= session
               (get @(:session-table session-storage) session-id)))
        (is (empty? (get-event-log session)))))))

(deftest create-test-with-logs
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (let [{:keys [session-storage storage]} system
            event {"EventType" "Test"}
            session-id (gen-session-id)]
        (ev/refresh! storage session-id [event])
        (let [session (create! session-storage storage file-id session-id "test event")]
          (is (= session
                 (get @(:session-table session-storage) session-id)))
          (is (= [event]
                 (get-event-log session))))))))

(deftest get-one-test
  (with-started-system [system]


    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (let [{:keys [session-storage storage]} system
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id "test event")]
        (is (= session
               (get-one session-storage session-id)))))))

(deftest append-event-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (let [{:keys [session-storage storage]} system
           session-id (gen-session-id)
           session (create! session-storage storage file-id session-id "test event")
           event {"EventType"  "Goal"
                  "Team"       "Team1"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Leg"
                  "Accidental" "OwnGoal"}]
       (append-events! storage session [event])
       (is (= [event]
              (get-event-log session)))
       (is (= [event]
              (ev/fetch storage session-id)))))))

(deftest get-events-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)
      (let [{:keys [session-storage storage]} system
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id "test event")
            event1 {"EventType"  "Goal"
                    "Team"       "Team1"
                    "GamePart"   "Half1"
                    "Standart"   "Corner"
                    "BodyPart"   "Leg"
                    "Accidental" "OwnGoal"}
            event2 {"EventType"  "Goal"
                    "Team"       "Team2"
                    "GamePart"   "Half1"
                    "Standart"   "Corner"
                    "BodyPart"   "Leg"
                    "Accidental" "OwnGoal"}]
        (do
          (is (empty? (get-event-log session)))
          (append-events! storage session [event1])
          (is (= [event1] (get-event-log session)))
          (append-events! storage session [event2])
          (is (= [event1 event2] (get-event-log session))))))))

(deftest set-events!-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)
      (let [{:keys [session-storage storage]} system
            event1 {"EventType"  "Goal"
                    "Team"       "Team1"
                    "GamePart"   "Half1"
                    "Standart"   "Corner"
                    "BodyPart"   "Head"
                    "Accidental" "OwnGoal"}
            event2 {"EventType"  "Goal"
                    "Team"       "Team2"
                    "GamePart"   "Half1"
                    "Standart"   "Corner"
                    "BodyPart"   "Head"
                    "Accidental" "OwnGoal"}
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id "test event")]
        (append-events! storage session [event1])
        (set-events! storage session [event2])
        (is (= [event2]
               (get-event-log session)))
        (is (= [event2]
               (ev/fetch storage session-id)))))))

(deftest get-out-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)
      (let [{:keys [session-storage storage]} system
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id "test event")]
        (is (= [{"Market name" "MATCH_BETTING"
                 "Outcome"     "HOME"
                 "Calc"        "lose"}
                {"Market name" "MATCH_BETTING"
                 "Outcome"     "DRAW"
                 "Calc"        "win"}
                {"Market name" "MATCH_BETTING"
                 "Outcome"     "AWAY"
                 "Calc"        "lose"}]
               (get-out session)))))))

(deftest finalize-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]
      (load-test-file! file-id)
      (let [{:keys [session-storage storage]} system
            session-id (gen-session-id)
            session (create! session-storage storage file-id session-id  "test event")
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
        (is (= (ev/fetch storage session-id)))))))

(deftest create-event-test
  (with-started-system [system]

                       (let [file-id (gen-file-id)]

                         (load-test-file! file-id test-file3)

                         (let [{:keys [session-storage storage]} system
                               session-id (gen-session-id)
                               session (create! session-storage storage file-id session-id "test event")
                               markets {"Market name" ["MATCH_BETTING" "MATCH_DRAW_NO_BET"]}

                               out-markets [{"Calc triger" "YES"
                                             "Calc"        "LOSE"
                                             "Game Part"   "Full Time"
                                             "Market name" "MATCH_BETTING"
                                             "Outcome"     "HOME"
                                             "Param"       999999.0
                                             "id"          1.0}
                                            {"Calc triger" "YES"
                                             "Calc"        "WIN"
                                             "Game Part"   "Full Time"
                                             "Market name" "MATCH_BETTING"
                                             "Outcome"     "DRAW"
                                             "Param"       999999.0
                                             "id"          2.0}
                                            {"Calc triger" "YES"
                                             "Calc"        "LOSE"
                                             "Game Part"   "Full Time"
                                             "Market name" "MATCH_BETTING"
                                             "Outcome"     "AWAY"
                                             "Param"       999999.0
                                             "id"          3.0}
                                            {"Calc triger" "YES"
                                             "Calc"        "RETURN"
                                             "Game Part"   "Full Time"
                                             "Market name" "MATCH_DRAW_NO_BET"
                                             "Outcome"     "HOME"
                                             "Param"       999999.0
                                             "id"          7.0}
                                            {"Calc triger" "YES"
                                             "Calc"        "RETURN"
                                             "Game Part"   "Full Time"
                                             "Market name" "MATCH_DRAW_NO_BET"
                                             "Outcome"     "AWAY"
                                             "Param"       999999.0
                                             "id"          8.0}]]

                           (clean-out! session markets)
                           (is (= out-markets
                                  (get-out session)
                                  ))
                           ))))