(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.web.handlers :as hd]
            [s-engine.session :as session]
            [s-engine.storage.file :as file]
            [s-engine.test-helper :refer :all])
  (:import (java.util UUID)))

(defn- resp->status+json
  [{:keys [status body]} & {kw? :keywordize :or {kw? true}}]
  [status (json/parse-string body kw?)])

(defn- resp->status+body
  [{:keys [status body]}]
  [status body])

;;
;; Routes test
;;

(deftest resource-not-found-test
  (with-started-system [system]
    (is (= 404
          (-> (url "/invalid-url") http/get deref :status))
       "Should return 404")))

(deftest file-upload-test
  (with-started-system [system]

    (testing "invalid file-id"
      (is (= [400 (hd/new-error 400 "MFP" "Invalid file id")]
             (-> (url "/files/sdsfdd/upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+json)))
          "should check file-id"))

    (testing "empty body"
      (is (= [400 (hd/new-error 400 "MFP" "No file sent")]
             (-> (url "/files" test-file-id "upload")
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+json)))
          "should check file for existance"))

    (testing "invalid EventLog header"
      (is (= [400 (hd/new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
             (-> (url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file invalid-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should check file EventLog header"))

    (testing "invalid file type"
      (is (= [400 (hd/new-error 400 "MFP" "Invalid file type")]
             (-> (url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file *file*)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should check file type"))

    (testing "normal uploading"
      (is (= [200 ""]
             (-> (url "/files" test-file-id "upload")
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file test-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+body)))
          "should upload file if there are no errors")
      (let [file-resp (-> (url "/files" test-file-id)
                          (http/get)
                          (deref))]
        (is (= 200 (:status (>trace file-resp))))
        (is (= "application/octet-stream"
               (-> file-resp :headers :content-type)))
        (is (= "attachment; filename=test-model.xlsx"
               (-> file-resp :headers :content-disposition)))
        (is (pos? (-> file-resp :headers :content-length (Integer/parseInt))))))))

(deftest file-replace-test
  (with-started-system [system]

    (load-test-file!)

    (testing "replace not uploaded file"
      (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                  Integer/MAX_VALUE))]
             (-> (url "/files" Integer/MAX_VALUE)
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file test-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should validate file for existance in db"))

    (testing "empty body"
      (is (= [400 (hd/new-error 400 "MFP" "No file sent")]
             (-> (url "/files" test-file-id)
                 (http/post {:multipart []})
                 (deref)
                 (resp->status+json)))
          "should validate file existance in params"))

    (testing "invalid EventLog"
      (is (= [400 (hd/new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
             (-> (url "/files" test-file-id)
                 (http/post {:multipart [{:name     "file"
                                          :content  (io/file invalid-file)
                                          :filename "test-model.xlsx"}]})
                 (deref)
                 (resp->status+json)))
          "should validate file before replace"))

    (testing "normal file replacement"
      (let [old-file-length (-> (url "/files" test-file-id)
                                (http/get)
                                (deref)
                                :headers
                                :content-length
                                (Integer/parseInt))]
        (is (= [200 ""]
               (-> (url "/files" test-file-id)
                   (http/post {:multipart [{:name     "file"
                                            :content  (io/file test-file2)
                                            :filename "new-model.xlsx"}]})
                   (deref)
                   (resp->status+body))))

        (let [file-resp (-> (url "/files" test-file-id)
                            (http/get)
                            (deref))]
          (is (= 200 (:status (>trace file-resp))))
          (is (= "application/octet-stream"
                 (-> file-resp :headers :content-type)))
          (is (= "attachment; filename=new-model.xlsx"
                 (-> file-resp :headers :content-disposition)))
          (is (pos? (-> file-resp :headers :content-length (Integer/parseInt))))

          (is (not= old-file-length
                    (-> file-resp :headers :content-length (Integer/parseInt)))))))))

(deftest file-delete-test
  (with-started-system [system]

    (load-test-file!)

    (testing "delete not existed file"
      (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                  Integer/MAX_VALUE))]
             (-> (url "/files" Integer/MAX_VALUE)
                 http/delete deref resp->status+json))
          "should check file existance before delete"))

    (testing "normal deletion"
      (is (= [200 ""]
             (-> (url "/files" test-file-id)
                 http/delete deref resp->status+body))
          "should successfully delete file")

      (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                  test-file-id))]
             (-> (url "/files" test-file-id)
                 (http/get)
                 (deref)
                 (resp->status+json)))
          "should return 404"))))

(deftest session-create-test
  (with-started-system [system]
    (load-test-file!)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
     (session/create! session-storage storage test-file-id session-id)

     (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                              Integer/MAX_VALUE))]
            (-> (url "/files" Integer/MAX_VALUE "1")
                http/post deref resp->status+json))
         "should check file before session creating")
     (is (false? (session/exists? session-storage "1")))

     (is (= [400 (hd/new-error 400 "MFP" (format "Session with id '%s' is already created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id)
                http/post deref resp->status+json))
         "should fail if session already exists")
     (is (false? (session/exists? session-storage "1")))

     (is (= [201 ""]
            (-> (url "/files" test-file-id "1")
                http/post deref resp->status+body))
         "Session created succesfully")
     (is (true? (session/exists? session-storage "1"))))))

(deftest session-get-event-log-test
  (with-started-system [system]
    (load-test-file!)
    (let [{:keys [storage session-storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id "event-log")
                http/get deref resp->status+json))
         "should fail if session not exists")
     (session/create! session-storage storage test-file-id session-id)
     (is (= [200 {:data [] :status 200}]
            (-> (url "/files" test-file-id session-id "event-log")
                (http/get)
                (deref)
                (resp->status+json)))
         "should return empty data"))))

(deftest session-append-event-test
  (with-started-system [system]
    (load-test-file!)
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
     (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id "event-log/append")
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
            (-> (url "/files" test-file-id session-id "event-log/append")
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
            (-> (url "/files" test-file-id session-id "event-log")
                (http/get) (deref)
                (resp->status+json :keywordize false)))))))

(deftest session-set-event-log-test
  (with-started-system [system]
    (load-test-file!)
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
     (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id "event-log/set")
                http/post deref resp->status+json))
         "Session does not exist")

     (let [session (session/create! session-storage storage test-file-id session-id)]
       (session/append-events! storage session [event1])
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
              (-> (url "/files" test-file-id session-id "event-log/set")
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
              (-> (url "/files" test-file-id session-id "event-log")
                  (http/get) (deref)
                  (resp->status+json :keywordize false))))))))

(deftest session-get-settlements-test
  (with-started-system [system]
    (load-test-file!)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id "settlements")
                http/get deref resp->status+json))
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
            (-> (url "/files" test-file-id session-id "settlements")
                (http/get) (deref)
                (resp->status+json :keywordize false)))))))

(deftest session-get-workbook-test
  (with-started-system [system]
    (load-test-file!)
    (let [{:keys [session-storage storage]} system
          session-id (str (UUID/randomUUID))]
      (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                               session-id))]
             (-> (url "/files" test-file-id session-id)
                 (http/get)
                 (deref)
                 (resp->status+json)))
          "Session does not exist")

      (session/create! session-storage storage test-file-id session-id)
      (let [resp (-> (url "/files" test-file-id session-id)
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
    (load-test-file!)
    (let [{:keys [storage session-storage]} system
          session-id (str (UUID/randomUUID))]
     (is (= [404 (hd/new-error 404 "SNF" (format "Session with id '%s' is not created"
                                              session-id))]
            (-> (url "/files" test-file-id session-id)
                http/delete deref resp->status+json))
         "Session does not exist")

     (session/create! session-storage storage test-file-id session-id)

     (is (= [404 (hd/new-error 404 "SNF" "Session with id '1' is not created")]
            (-> (url "/files" test-file-id "1")
                (http/delete) (deref)
                (resp->status+json)))
         "should delete non-existing session")
     (is (= [204 nil]
            (-> (url "/files" test-file-id session-id)
                (http/delete) (deref)
                (resp->status+json)))))))