(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.config :as c]
            [s-engine.session :as session]
            [s-engine.storage.file :as file]
            [s-engine.test-helper :refer :all])
  (:import (java.util UUID)))

(defn make-url [& paths]
  (let [port (:port @c/config)
        path (apply str (interpose "/" paths))]
    (format "http://localhost:%d%s" port path)))

(defn- resp->status+json
  [{:keys [status body]} & {kw? :keywordize :or {kw? true}}]
  [status (json/parse-string body kw?)])

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

#_(deftest check-valid-events-test
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

(def ^:const invalid-file "test/resources/AutoCalc_Soccer_EventLog.invalid.xlsx")

(deftest file-upload-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
      (file/delete! storage test-file-id)
      (is (= [400 (new-error 400 "MFP" "Invalid file id")]
             (-> (make-url "/files/sdsfdd/upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+json)))
          "should check file-id")
      (is (= [400 (new-error 400 "MFP" "No file sent")]
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+json)))
          "shoudl check file for existance")
      (is (= [400 (new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file invalid-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should check file EventLog header")
      (is (= [400 (new-error 400 "MFP" "Invalid file type")]
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file *file*)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should check file type")
      (is (= [200 ""]
             (-> (make-url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file test-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+body)))
          "should upload file if there are no errors")
      (is (true? (file/exists? storage test-file-id))))))

(deftest file-replace-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
     (is (= [404 (new-error 404 "FNF" (format "File with id '%s' not found"
                                              Integer/MAX_VALUE))]
            (-> (make-url "/files" Integer/MAX_VALUE)
                (http/post {:multipart [{:name     "file"
                                         :content  (io/file test-file)
                                         :filename "test-model.xlsx"}]})
                (deref)
                (resp->status+json)))
         "should validate file for existance in db")
     (is (= [400 (new-error 400 "MFP" "No file sent")]
            (-> (make-url "/files" test-file-id)
                (http/post {:multipart []})
                (deref)
                (resp->status+json)))
         "should validate file existance in params")
     (is (= [400 (new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
            (-> (make-url "/files" test-file-id)
                (http/post {:multipart [{:name     "file"
                                         :content  (io/file invalid-file)
                                         :filename "test-model.xlsx"}]})
                (deref)
                (resp->status+json)))
         "should validate file before replace")
     (is (= [200 ""]
            (-> (make-url "/files" test-file-id)
                (http/post {:multipart [{:name     "file"
                                         :content  (io/file test-file)
                                         :filename "new-model.xlsx"}]})
                (deref)
                (resp->status+body))))
     (is (= "new-model.xlsx"
            (:file-name (file/get-one storage test-file-id)))
         "should replace file successfully"))))

(deftest file-delete-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage]} system]
     (is (= [404 (new-error 404 "FNF" (format "File with id '%s' not found"
                                              Integer/MAX_VALUE))]
            (-> (make-url "/files" Integer/MAX_VALUE)
                http/delete deref resp->status+json))
         "should check file existance before delete")

     (is (= [200 ""]
            (-> (make-url "/files" test-file-id)
                http/delete deref resp->status+body))
         "should successfully delete file")
     (is (false? (file/exists? storage test-file-id))
         "should really delete it"))))

(deftest session-create-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
     (session/create! session-storage storage test-file-id session-id)

     (is (= [404 (new-error 404 "FNF" (format "File with id '%s' not found"
                                              Integer/MAX_VALUE))]
            (-> (make-url "/files" Integer/MAX_VALUE "1")
                http/post deref resp->status+json))
         "should check file before session creating")
     (is (false? (session/exists? session-storage "1")))

     (is (= [400 (new-error 400 "MFP" (format "Session with id '%s' is already created"
                                              session-id))]
            (-> (make-url "/files" test-file-id session-id)
                http/post deref resp->status+json))
         "should fail if session already exists")
     (is (false? (session/exists? session-storage "1")))

     (is (= [201 ""]
            (-> (make-url "/files" test-file-id "1")
                http/post deref resp->status+body))
         "Session created succesfully")
     (is (true? (session/exists? session-storage "1"))))))

(deftest session-get-event-log-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= [404 (new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (make-url "/files" test-file-id session-id "event-log")
                http/get deref resp->status+json))
         "should fail if session not exists")
     (session/create! session-storage storage test-file-id session-id)
     (is (= [200 {:data [] :status 200}]
            (-> (make-url "/files" test-file-id session-id "event-log")
                (http/get)
                (deref)
                (resp->status+json)))
         "should return empty data"))))

(deftest session-append-event-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system
         events [{"EventType"  "Goal"
                  "min"        1.
                  "sec"        1.
                  "Team"       "Team1"
                  "GamePart"   "Half1"
                  "Standart"   "Corner"
                  "BodyPart"   "Head"
                  "Accidental" "OwnGoal"}]
          session-id (str (UUID/randomUUID))]
     (is (= [404 (new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (make-url "/files" test-file-id session-id "event-log/append")
                http/post deref resp->status+json))
         "Session does not exist")
     (session/create! session-storage storage test-file-id session-id)
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
            (-> (make-url "/files" test-file-id session-id "event-log/append")
                (http/post {:body (json/generate-string events)})
                (deref)
                (resp->status+json :keywordize false))))
     (is (= [200 {"status" 200
                  "data"   [{"Accidental" "OwnGoal"
                             "Action"     ""
                             "BodyPart"   "Head"
                             "EventType"  "Goal"
                             "GamePart"   "Half1"
                             "Standart"   "Corner"
                             "Team"       "Team1"}]}]
            (-> (make-url "/files" test-file-id session-id "event-log")
                (http/get) (deref)
                (resp->status+json :keywordize false)))))))

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
                 "Accidental" "OwnGoal"}
          session-id (str (UUID/randomUUID))]
     (is (= [404 (new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (make-url "/files" test-file-id session-id "event-log/set")
                http/post deref resp->status+json))
         "Session does not exist")

     (let [session (session/create! session-storage storage test-file-id session-id)]
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
              (-> (make-url "/files" test-file-id session-id "event-log/set")
                  (http/post {:body (json/generate-string [event2])})
                  (deref)
                  (resp->status+json :keywordize false))))
       (is (= [200 {"status" 200
                    "data"   [{"EventType"  "Goal"
                               "Team"       "Team2"
                               "GamePart"   "Half1"
                               "Standart"   "Corner"
                               "BodyPart"   "Head"
                               "Accidental" "OwnGoal"
                               "Action"     ""}]}]
              (-> (make-url "/files" test-file-id session-id "event-log")
                  (http/get) (deref)
                  (resp->status+json :keywordize false))))))))

(deftest session-get-settlements-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= (resp->status+body error-404-fnf)
            (-> (make-url "/files" test-file-id session-id "settlements")
                http/get deref resp->status+body))
         "Session does not exist")
     (session/create! session-storage storage test-file-id session-id)
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
            (-> (make-url "/files" test-file-id session-id "settlements")
                (http/get) (deref)
                (resp->status+json)))))))

(deftest session-get-workbook-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
      (is (= (resp->status+json error-404-fnf)
             (-> (make-url "/files" test-file-id session-id)
                 (http/get)
                 (deref)
                 (resp->status+json)))
          "Session does not exist")
      (session/create! session-storage storage test-file-id session-id)
      (let [resp (-> (make-url "/files" test-file-id session-id)
                     (http/get)
                     (deref))]
        (is (= 200 (:status resp)))
        (is (= "application/octet-stream"
               (-> resp :headers :content-type)))
        (is (= "attachment; filename=test-model.xlsx"
               (-> resp :headers :content-disposition)))
        (is (pos? (-> resp :headers :content-length (Integer/parseInt))))))))

(deftest session-finalize-test
  (with-started-system [system]
    (load-test-file! system)
    (let [{:keys [storage session-storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= [404 (new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (make-url "/files" test-file-id session-id)
                http/delete deref resp->status+json))
         "Session does not exist")

     (session/create! session-storage storage test-file-id session-id)

     (is (= [404 (new-error 404 "SNF" "Session with id '1' is not created")]
            (-> (make-url "/files" test-file-id "1")
                (http/delete) (deref)
                (resp->status+json)))
         "should delete non-existing session")
     (is (= [204 nil]
            (-> (make-url "/files" test-file-id session-id)
                (http/delete) (deref)
                (resp->status+json)))))))