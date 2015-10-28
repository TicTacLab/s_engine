(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.config :as c]
            [s-engine.session :as session]
            [s-engine.storage.file :as file]
            [s-engine.test-helper :refer :all]))

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
  (with-started-system [system]
    (is (= (resp->status+body error-404-rnf)
          (-> (make-url "/invalid-url") http/get deref resp->status+body))
       "Should return 404")))

(deftest file-upload-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
      (file/delete! storage test-file-id)
      (is (= (resp->status+body error-400-mfp)
             (-> (make-url "/files/sdsfdd/upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+body)))
          "Malformed file-id")
      (is (= (resp->status+body error-400-mfp)
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+body)))
          "No file given")
      (is (= [201 ""]
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file test-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+body)))
          "File created succesfully")
      (is (true? (file/exists? storage test-file-id))))))

(deftest file-replace-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" Integer/MAX_VALUE)
                (http/post {:multipart [{:name     "file"
                                         :content  (io/file test-file)
                                         :filename "test-model.xlsx"}]})
                (deref)
                (resp->status+body)))
         "File does not exist")
     (is (= (resp->status+body error-400-mfp)
            (-> (make-url "/files" test-file-id)
                (http/post {:multipart []})
                (deref)
                (resp->status+body)))
         "No file given")
     (is (= [204 ""]
            (-> (make-url "/files" test-file-id)
                (http/post {:multipart [{:name     "file"
                                         :content  (io/file test-file)
                                         :filename "new-model.xlsx"}]})
                (deref)
                (resp->status+body))))
     (is (= "new-model.xlsx"
            (:file-name (file/get-one storage test-file-id)))
         "Replaced file succesfully"))))

(deftest file-delete-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" Integer/MAX_VALUE)
                http/delete deref resp->status+body)))
     (is (= [204 ""]
            (-> (make-url "/files" test-file-id)
                http/delete deref resp->status+body)))
     (is (false? (file/exists? storage test-file-id))))))

(deftest session-create-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system]
     (session/create! session-storage storage test-file-id "2")
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" Integer/MAX_VALUE "1")
                http/post deref resp->status+body))
         "File not found")
     (is (false? (session/exists? session-storage "1")))
     (is (= (resp->status+body error-400-mfp)
            (-> (make-url "/files" test-file-id "2")
                http/post deref resp->status+body))
         "Session already exists")
     (is (false? (session/exists? session-storage "1")))
     (is (= [201 ""]
            (-> (make-url "/files" test-file-id "1")
                http/post deref resp->status+body))
         "Session created succesfully")
     (is (true? (session/exists? session-storage "1"))))))

(deftest session-get-event-log-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id "1" "event-log")
                http/get deref resp->status+body))
         "Session does not exist")
     (session/create! session-storage storage test-file-id "1")
     (is (= [200 {"data" [] "status" 200}]
            (-> (make-url "/files" test-file-id "1" "event-log")
                (http/get)
                (deref)
                (resp->status+json)))))))

(deftest session-append-event-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system
         event {"EventType"  "Goal"
                "min"        1.
                "sec"        1.
                "Team"       "Team1"
                "GamePart"   "Half1"
                "Standart"   "Corner"
                "BodyPart"   "Head"
                "Accidental" "OwnGoal"}]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id "1" "event-log/append")
                http/post deref resp->status+body))
         "Session does not exist")
     (session/create! session-storage storage test-file-id "1")
     (is (= [200 {"status" 200
                  "data"   [{"Market name" "MATCH_BETTING"
                             "Outcome"     "HOME"
                             "Calc"        "win"}
                            {"Market name" "MATCH_BETTING"
                             "Outcome"     "DRAW"
                             "Calc"        "lose"}
                            {"Market name" "MATCH_BETTING"
                             "Outcome"     "AWAY"
                             "Calc"        "lose"}]}]
            (-> (make-url "/files" test-file-id 1 "event-log/append")
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
            (-> (make-url "/files" test-file-id 1 "event-log")
                (http/get) (deref)
                (resp->status+json)))))))

(deftest session-set-event-log-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
         event1 {"EventType"  "Goal"
                 "min"        0
                 "sec"        0
                 "Team"       "Team1"
                 "GamePart"   "Half1"
                 "Standart"   "Corner"
                 "BodyPart"   "Head"
                 "Accidental" "OwnGoal"}
         event2 {"EventType"  "Goal"
                 "min"        1
                 "sec"        1
                 "Team"       "Team2"
                 "GamePart"   "Half1"
                 "Standart"   "Corner"
                 "BodyPart"   "Head"
                 "Accidental" "OwnGoal"}]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id "1" "event-log/set")
                http/post deref resp->status+body))
         "Session does not exist")
     (let [session (session/create! session-storage storage test-file-id "1")]
       (session/append-event! storage session event1)
       (is (= [200 {"status" 200
                    "data"   [{"Market name" "MATCH_BETTING"
                               "Outcome"     "HOME"
                               "Calc"        "lose"}
                              {"Market name" "MATCH_BETTING"
                               "Outcome"     "DRAW"
                               "Calc"        "lose"}
                              {"Market name" "MATCH_BETTING"
                               "Outcome"     "AWAY"
                               "Calc"        "win"}]}]
              (-> (make-url "/files" test-file-id 1 "event-log/set")
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
              (-> (make-url "/files" test-file-id 1 "event-log")
                  (http/get) (deref)
                  (resp->status+json))))))))

(deftest session-get-settlements-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id "1" "settlements")
                http/get deref resp->status+body))
         "Session does not exist")
     (session/create! session-storage storage test-file-id "1")
     (is (= [200 {"status" 200
                  "data"   [{"Market name" "MATCH_BETTING"
                             "Outcome"     "HOME"
                             "Calc"        "lose"}
                            {"Market name" "MATCH_BETTING"
                             "Outcome"     "DRAW"
                             "Calc"        "win"}
                            {"Market name" "MATCH_BETTING"
                             "Outcome"     "AWAY"
                             "Calc"        "lose"}]}]
            (-> (make-url "/files" test-file-id 1 "settlements")
                (http/get) (deref)
                (resp->status+json)))))))

(deftest session-finalize-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id "1")
                http/delete deref resp->status+body))
         "Session does not exist")
     (session/create! session-storage storage test-file-id "1")
     (is (= [404 {"errors" [{"code"    "FNF"
                             "message" "File not found"}]
                  "status" 404}]
            (-> (make-url "/files" test-file-id 2)
                (http/delete) (deref)
                (resp->status+json)))
         "should delete non-existing session")
     (is (= [204 nil]
            (-> (make-url "/files" test-file-id 1)
                (http/delete) (deref)
                (resp->status+json)))))))