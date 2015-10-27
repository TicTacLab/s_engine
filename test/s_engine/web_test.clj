(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.config :as c]
            [s-engine.system :as s]
            [malcolmx.core :as mx]
            [s-engine.session :as session])
  (:import (java.net URLEncoder)))

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
    (f)
    (finally
      (stop-system))))

(defn make-url [& paths]
  (let [port (:port @c/config)
        path (apply str (interpose "/" paths))]
    (format "http://localhost:%d%s" port path)))

(defn resp->status+body
  [{:keys [status body]}]
  [status (json/parse-string body false)])

(use-fixtures :each wrap-with-system)

(def ^:const test-model "test/resources/AutoCalc_Soccer_EventLog.xlsx")
(def ^:const test-model-id (URLEncoder/encode test-model))

(deftest session-get-event-log-test
  (let [{:keys [session-storage]} system]
    (session/create! session-storage {:file test-model} "1")
    (is (= [200 {"data" [] "status" 200}]
           (-> (make-url "/files" test-model-id 1 "event-log")
               (http/get)
               (deref)
               (resp->status+body))))))

(deftest session-append-event-test
  (let [{:keys [session-storage]} system
        event {"EventType" "Goal"
               "min"       1.
               "sec"       1.
               "Team"      "Team1"
               "GamePart"  "Half1"
               "Standart"  "Corner"
               "BodyPart"  "Head"
               "Accidental" "OwnGoal"}]
    (session/create! session-storage {:file test-model} "1")
    (is (= [200 {"status" 200
                 "data"   [{"Market name" "MATCH_BETTING"
                            "Outcome" "HOME"
                            "Calc" "win"}
                           {"Market name" "MATCH_BETTING"
                            "Outcome" "DRAW"
                            "Calc" "lose"}
                           {"Market name" "MATCH_BETTING"
                            "Outcome" "AWAY"
                            "Calc" "lose"}]}]
           (-> (make-url "/files" test-model-id 1 "event-log/append")
               (http/post {:body (json/generate-string event)})
               (deref)
               (resp->status+body))))
    (is (= [200 {"status" 200
                 "data"   [{"Accidental" "OwnGoal"
                            "Action"     ""
                            "BodyPart"   "Head"
                            "EventType"  "Goal"
                            "GamePart"   "Half1"
                            "Standart"   "Corner"
                            "Team"       "Team1"
                            "min"        1.0
                            "sec"        1.0}]}]
           (-> (make-url "/files" test-model-id 1 "event-log")
               (http/get) (deref)
               (resp->status+body))))))

(deftest session-set-event-log-test
  (let [{:keys [session-storage storage]} system
        event1 {"EventType" "Goal"
                "min"       0
                "sec"       0
                "Team"      "Team1"
                "GamePart"  "Half1"
                "Standart"  "Corner"
                "BodyPart"  "Head"
                "Accidental" "OwnGoal"}
        event2 {"EventType" "Goal"
                "min"       1
                "sec"       1
                "Team"      "Team2"
                "GamePart"  "Half1"
                "Standart"  "Corner"
                "BodyPart"  "Head"
                "Accidental" "OwnGoal"}
        session (session/get-or-create! session-storage storage test-model "1")]
    (session/append-event! session event1)
    (is (= [200 {"status" 200
                 "data" [{"Market name" "MATCH_BETTING"
                          "Outcome" "HOME"
                          "Calc" "lose"}
                         {"Market name" "MATCH_BETTING"
                          "Outcome" "DRAW"
                          "Calc" "lose"}
                         {"Market name" "MATCH_BETTING"
                          "Outcome" "AWAY"
                          "Calc" "win"}]}]
           (-> (make-url "/files" test-model-id 1 "event-log/set")
               (http/post {:body (json/generate-string [event2])})
               (deref)
               (resp->status+body))))
    (is (= [200 {"status" 200
                 "data"   [{"EventType"  "Goal"
                            "min"        1.0
                            "sec"        1.0
                            "Team"       "Team2"
                            "GamePart"   "Half1"
                            "Standart"   "Corner"
                            "BodyPart"   "Head"
                            "Accidental" "OwnGoal"
                            "Action"     ""}]}]
           (-> (make-url "/files" test-model-id 1 "event-log")
               (http/get) (deref)
               (resp->status+body))))))

(deftest session-get-settlements-test
  (let [{:keys [session-storage storage]} system
        _ (session/get-or-create! session-storage storage test-model "1")]
    (is (= [200 {"status" 200
                 "data"   [{"Market name" "MATCH_BETTING"
                            "Outcome" "HOME"
                            "Calc" "lose"}
                           {"Market name" "MATCH_BETTING"
                            "Outcome" "DRAW"
                            "Calc" "win"}
                           {"Market name" "MATCH_BETTING"
                            "Outcome" "AWAY"
                            "Calc" "lose"}]}]
           (-> (make-url "/files" test-model-id 1 "settlements")
               (http/get) (deref)
               (resp->status+body))))))

(deftest session-finalize-test
  (let [{:keys [session-storage]} system]
    (session/create! session-storage {:file test-model} "1")
    (is (= [404 {"errors" [{"code"    "FNF"
                            "message" "File not found"}]
                 "status" 404}]
           (-> (make-url "/files" test-model-id 2)
               (http/delete) (deref)
               (resp->status+body)))
        "should delete non-existing session")
    (is (= [204 nil]
           (-> (make-url "/files" test-model-id 1)
               (http/delete) (deref)
               (resp->status+body))))))