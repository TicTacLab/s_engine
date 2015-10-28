(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [com.stuartsierra.component :as component]
            [s-engine.storage.file :as file]
            [s-engine.system :as s]
            [s-engine.config :as c]
            [s-engine.storage.event-log :as ev]
            [clojure.tools.logging :as log])
  (:import (java.io File)
           (java.util UUID)))

;;
;; Setup
;;

(def ^:const test-file-id 1)
(def ^:const test-file "test/resources/AutoCalc_Soccer_EventLog.xlsx")

(defn- load-test-file!
  [{:keys [storage]}]
  (let [file-bytes (file/read-bytes (File. test-file))]
    (file/write! storage test-file-id file-bytes "test-model.xlsx")))

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
    (load-test-file! system)
    (f)
    (catch Exception e
      (log/error e "Error"))
    (finally
      (stop-system))))

(use-fixtures :each wrap-with-system)

;;
;; Tests
;;

(deftest create-test-without-logs
  (let [{:keys [session-storage storage]} system
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)]
    (is (= session
           (get @(:session-table session-storage) session-id)))
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
               "Action"     ""}
        session-id (str (UUID/randomUUID))]
    (ev/refresh! storage session-id [event])
    (let [session (create! session-storage storage test-file-id session-id)]
      (is (= session
             (get @(:session-table session-storage) session-id)))
      (is (= [event]
             (get-events session))))))

(deftest get-one-test
  (let [{:keys [session-storage storage]} system
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)]
    (is (= session
           (get-one session-storage session-id)))))

(deftest append-event-test
  (let [{:keys [session-storage storage]} system
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)
        event {"EventType"  "Goal"
               "min"        0.
               "sec"        0.
               "Team"       "Team1"
               "GamePart"   "Half1"
               "Standart"   "Corner"
               "BodyPart"   "Leg"
               "Accidental" "OwnGoal"
               "Action"     ""}]
    (append-event! storage session event)
    (is (= [event]
           (get-events session)))
    (is (= [event]
           (ev/fetch storage session-id)))))

(deftest get-events-test
  (let [{:keys [session-storage storage]} system
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)
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
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)]
    (append-event! storage session event1)
    (set-events! storage session [event2])
    (is (= [event2]
           (get-events session)))
    (is (= [event2]
           (ev/fetch storage session-id)))))

(deftest get-out-test
  (let [{:keys [session-storage storage]} system
        session-id (str (UUID/randomUUID))
        session (create! session-storage storage test-file-id session-id)]
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
    (append-event! storage session event)
    (finalize! session-storage storage session)
    (is (nil? (get-one session-storage session-id)))
    (is (= (ev/fetch storage session-id)))))