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
            session (create! session-storage storage file-id session-id)]
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
        (let [session (create! session-storage storage file-id session-id)]
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
            session (create! session-storage storage file-id session-id)]
        (is (= session
               (get-one session-storage session-id)))))))

(deftest append-event-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (let [{:keys [session-storage storage]} system
           session-id (gen-session-id)
           session (create! session-storage storage file-id session-id)
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
            session (create! session-storage storage file-id session-id)
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
            session (create! session-storage storage file-id session-id)]
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
            session (create! session-storage storage file-id session-id)]
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
            session (create! session-storage storage file-id session-id)
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