(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.config :as c]
            [s-engine.system :as s]
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

(defn- resp->status+json
  [{:keys [status body]}]
  [status (json/parse-string body false)])

(defn- resp->status+body
  [{:keys [status body]}]
  [status body])

(use-fixtures :each wrap-with-system)

(def ^:const test-model "test/resources/AutoCalc_Soccer_EventLog.xlsx")
(def ^:const test-model-id (URLEncoder/encode test-model))

;;
;; Utils test
;;

(deftest resp->test
  (is (= nil (resp-> nil)))
  (is (= :3 (resp-> 1 "2" :3)))
  (is (= {:status 200}
         (resp-> {:status 200} nil {:status 300})))
  (is (= {:status 200}
         (resp-> :1 {:status 200} {:status 300}))))

(deftest check-valid-events-test
  (is (not (nil? (check-valid-events ""))))
  (is (not (nil? (check-valid-events []))))
  (is (not (nil? (check-valid-events [{}]))))
  (is (not (nil? (check-valid-events [{"EventType" "TYPE"}]))))
  (is (nil? (check-valid-events [{"EventType" "TYPE"
                                  "min" 0
                                  "sec" 0.0}])))
  (is (nil? (check-valid-events [{"EventType" "TYPE"
                                  "min" 0
                                  "sec" 0.0
                                  "Attr" "Value"}]))))

;;
;; Routes test
;;

(deftest resource-not-found-test
  (is (= (resp->status+body error-404-rnf)
         (-> (make-url "/invalid-url") http/get deref resp->status+body))
      "Should return 404"))

(deftest session-create-test
  (let [{:keys [session-storage]} system]
    (session/create! session-storage {:file test-model} "2")
    (is (= (resp->status+body error-404-fnf)
           (-> (make-url "/files" "unknown-model" "1")
               http/post deref resp->status+body))
        "Model not found")
    (is (false? (session/exists? session-storage "1")))
    (is (= (resp->status+body error-400-mfp)
           (-> (make-url "/files" test-model-id "2")
               http/post deref resp->status+body))
        "Session already exists")
    (is (false? (session/exists? session-storage "1")))
    (is (= [201 ""]
           (-> (make-url "/files" test-model-id "1")
               http/post deref resp->status+body))
        "Session created succesfully")
    (is (true? (session/exists? session-storage "1")))))

(deftest session-get-event-log-test
  (let [{:keys [session-storage]} system]
    (session/create! session-storage {:file test-model} "1")
    (is (= [200 {"data" [] "status" 200}]
           (-> (make-url "/files" test-model-id 1 "event-log")
               (http/get)
               (deref)
               (resp->status+json))))))

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
               (resp->status+json))))
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
               (resp->status+json))))))

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
               (resp->status+json))))
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
               (resp->status+json))))))

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
               (resp->status+json))))))

(deftest session-finalize-test
  (let [{:keys [session-storage]} system]
    (session/create! session-storage {:file test-model} "1")
    (is (= [404 {"errors" [{"code"    "FNF"
                            "message" "File not found"}]
                 "status" 404}]
           (-> (make-url "/files" test-model-id 2)
               (http/delete) (deref)
               (resp->status+json)))
        "should delete non-existing session")
    (is (= [204 nil]
           (-> (make-url "/files" test-model-id 1)
               (http/delete) (deref)
               (resp->status+json))))))