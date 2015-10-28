(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [com.stuartsierra.component :as component]
            [s-engine.storage.model :as model]
            [s-engine.system :as s]
            [s-engine.config :as c]
            [s-engine.storage.event-log :as ev])
  (:import (java.io File)))

;;
;; Setup
;;

(def ^:const test-model-id 1)
(def ^:const test-model-file "test/resources/AutoCalc_Soccer_EventLog.xlsx")

(defn- load-test-model!
  [{:keys [storage]}]
  (let [file-bytes (model/read-bytes (File. test-model-file))]
    (model/write! storage test-model-id file-bytes "test-model.xlsx")))

(defn- finalize-sessions
  [{:keys [storage session-storage]}]
  (->> (get-all session-storage)
       (mapv #(finalize! session-storage storage %))))

(def system nil)

(defn start-system []
  (alter-var-root #'system (constantly (s/new-system @c/config)))
  (alter-var-root #'system component/start))

(defn stop-system []
  (when system
    (alter-var-root #'system component/stop)))

(defn wrap-with-system [f]
  (try
    (stop-system)
    (start-system)
    (load-test-model! system)
    (f)
    (finally
      (finalize-sessions system)
      (stop-system))))

(use-fixtures :each wrap-with-system)

;;
;; Tests
;;

(deftest create-test-without-logs
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")]
    (is (= session
           (get @(:session-table session-storage) "session1")))
    (is (empty? (get-events session)))))

(deftest create-test-with-logs
  (let [{:keys [session-storage storage]} system
        event {"EventType"  "Test"
               "min"        ""
               "sec"        ""
               "Team"       ""
               "BodyPart"   ""
               "GamePart"   ""
               "Standart"   ""
               "Accidental" ""
               "Action"     ""}]
    (ev/refresh! storage "session1" [event])
    (let [session (create! session-storage storage test-model-id "session1")]
      (is (= session
             (get @(:session-table session-storage) "session1")))
      (is (= [event]
             (get-events session))))))

(deftest get-one-test
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")]
    (is (= session
           (get-one session-storage "session1")))))

(deftest append-event-test
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")
        event {"EventType"  "Goal"
               "min"        0.
               "sec"        0.
               "Team"       "Team1"
               "GamePart"   "Half1"
               "Standart"   "Corner"
               "BodyPart"   "Leg"
               "Accidental" "OwnGoal"
               "Action" ""}]
    (append-event! storage session event)
    (is (= [event]
           (get-events session)))
    (is (= [event]
           (ev/fetch storage "session1")))))

(deftest get-events-test
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")
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
      (append-event! storage session event1)
      (is (= [event1] (get-events session)))
      (append-event! storage session event2)
      (is (= [event1 event2] (get-events session))))))

(deftest set-events!-test
  (let [{:keys [session-storage storage]} system
        event1 {"EventType"  "Goal"
                "min"        0.
                "sec"        0.
                "Team"       "Team1"
                "GamePart"   "Half1"
                "Standart"   "Corner"
                "BodyPart"   "Head"
                "Accidental" "OwnGoal"
                "Action"     ""}
        event2 {"EventType"  "Goal"
                "min"        1.
                "sec"        1.
                "Team"       "Team2"
                "GamePart"   "Half1"
                "Standart"   "Corner"
                "BodyPart"   "Head"
                "Accidental" "OwnGoal"
                "Action"     ""}
        session (create! session-storage storage test-model-id "session1")]
    (append-event! storage session event1)
    (set-events! storage session [event2])
    (is (= [event2]
           (get-events session)))
    (is (= [event2]
           (ev/fetch storage "session1")))))

(deftest get-out-test
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")]
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

(deftest finalize-test
  (let [{:keys [session-storage storage]} system
        session (create! session-storage storage test-model-id "session1")
        event {"EventType"  "Goal"
               "min"        0.
               "sec"        0.
               "Team"       "Team1"
               "GamePart"   "Half1"
               "Standart"   "Corner"
               "BodyPart"   "Head"
               "Accidental" "OwnGoal"
               "Action"     ""}]
    (append-event! storage session event)
    (finalize! session-storage storage session)
    (is (nil? (get-one session-storage "session1")))
    (is (= (ev/fetch storage "session1")))))