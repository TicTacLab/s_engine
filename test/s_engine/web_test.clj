(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.web.handlers :as hd]
            [s-engine.test-helper :refer :all]))

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
    (is (= 404 (:status (req! :get (urlf "/invalid-url"))))
       "Should return 404")))

(deftest file-upload-test
  (with-started-system [system]

    (testing "invalid file-id"
      (is (= [400 (hd/new-error 400 "MFP" "Invalid file id")]
             (-> (req! :post (urlf "/files/sdsfdd/upload"))
                 (resp->status+json)))
          "should check file-id"))

    (testing "upload file when it is already exists"
      (let [uploaded-file-id (gen-file-id)]
        (load-test-file! uploaded-file-id)
        (is (= [400 (hd/new-error 400 "FAE" "File already exists")]
               (-> (req! :post (urlf "/files/%s/upload" uploaded-file-id))
                   (resp->status+json))))))

    (testing "empty body"
      (let [file-id (gen-file-id)]
        (is (= [400 (hd/new-error 400 "MFP" "No file sent")]
               (-> (req! :post (>trace (urlf "/files/%s/upload" file-id)))
                   (resp->status+json)))
            "should check file for existance")))

    (testing "invalid EventLog header"
      (let [file-id (gen-file-id)]
        (is (= [400 (hd/new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
               (-> (req! :post (urlf "/files/%s/upload" file-id) nil
                         {:multipart [{:name     "file"
                                       :content  (io/file invalid-file)
                                       :filename "test-model.xlsx"}]})
                   (resp->status+json)))
            "should check file EventLog header")))

    (testing "invalid file type"
      (let [file-id (gen-file-id)]
        (is (= [400 (hd/new-error 400 "MFP" "Invalid file type")]
              (-> (req! :post (urlf "/files/%s/upload" file-id) nil
                        {:multipart [{:name     "file"
                                      :content  (io/file *file*)
                                      :filename "test-model.xlsx"}]})
                  (resp->status+json)))
           "should check file type")))

    (testing "normal uploading"
      (let [file-id (gen-file-id)]
        (is (= [200 (empty-body 200)]
               (-> (req! :post (urlf "/files/%s/upload" file-id) nil
                         {:multipart [{:name     "file"
                                       :content  (io/file test-file)
                                       :filename "test-model.xlsx"}]})
                   (resp->status+body)))
            "should upload file if there are no errors")

        (let [file-resp (req! :get (urlf "/files/%s" file-id))]
          (is (= 200 (:status file-resp)))
          (is (= "application/octet-stream"
                 (-> file-resp :headers :content-type)))
          (is (= "attachment; filename=test-model.xlsx"
                 (-> file-resp :headers :content-disposition)))
          (is (pos? (-> file-resp :headers :content-length (Integer/parseInt)))))))))

(deftest file-replace-test
  (with-started-system [system]

    (testing "replace not uploaded file"
      (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                  Integer/MAX_VALUE))]
             (-> (req! :post (urlf "/files/%s" Integer/MAX_VALUE) nil
                       {:multipart [{:name     "file"
                                     :content  (io/file test-file)
                                     :filename "test-model.xlsx"}]})
                 (resp->status+json)))
          "should validate file for existance in db"))

    (testing "empty body"
      (let [file-id (gen-file-id)]

        (load-test-file! file-id)

        (is (= [400 (hd/new-error 400 "MFP" "No file sent")]
              (-> (req! :post (urlf "/files/%s" file-id))
                  (resp->status+json)))
           "should validate file existance in params")))

    (testing "invalid EventLog"
      (let [file-id (gen-file-id)]

        (load-test-file! file-id)

        (is (= [400 (hd/new-error 400 "MFP" "Missing Columns: [Action]; Extra Columns [Extra column];")]
              (-> (req! :post (urlf "/files/%s" file-id) nil
                        {:multipart [{:name     "file"
                                      :content  (io/file invalid-file)
                                      :filename "test-model.xlsx"}]})
                  (resp->status+json)))
           "should validate file before replace")))

    (testing "normal file replacement"
      (let [file-id (gen-file-id)]

        (load-test-file! file-id)

        (let [old-file-length (-> (req! :get (urlf "/files/%s" file-id))
                                  :headers
                                  :content-length
                                  (Integer/parseInt))]
          (is (= [200 (empty-body 200)]
                 (-> (req! :post (urlf "/files/%s" file-id) nil
                           {:multipart [{:name     "file"
                                         :content  (io/file test-file2)
                                         :filename "new-model.xlsx"}]})
                     (resp->status+body))))

          (let [file-resp (req! :get (urlf "/files/%s" file-id))]
            (is (= 200 (:status file-resp)))
            (is (= "application/octet-stream"
                   (-> file-resp :headers :content-type)))
            (is (= "attachment; filename=new-model.xlsx"
                   (-> file-resp :headers :content-disposition)))
            (is (pos? (-> file-resp :headers :content-length (Integer/parseInt))))

            (is (not= old-file-length
                      (-> file-resp :headers :content-length (Integer/parseInt))))))))))

(deftest file-delete-test
  (with-started-system [system]

(testing "delete not existed file"
      (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                  Integer/MAX_VALUE))]
             (-> (req! :delete (urlf "/files/%s" Integer/MAX_VALUE))
                 resp->status+json))
          "should check file existance before delete"))

    (testing "normal deletion"
      (let [file-id (gen-file-id)]
        (load-test-file! file-id)

        (is (= [200 (empty-body 200)]
               (-> (req! :delete (urlf "/files/%s" file-id))
                   resp->status+body))
            "should successfully delete file")

        (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                    file-id))]
               (-> (req! :get (urlf "/files/%s" file-id))
                   (resp->status+json)))
            "should return 404")))))

(deftest session-create-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "session creating with not uploaded file"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "FNF" (format "File with id '%s' not found"
                                                      Integer/MAX_VALUE))]
                 (->> {:params {:file-id Integer/MAX_VALUE, :event-id ssid}}
                      (json-req! :post (urlf "/events"))
                      resp->status+json))
              "should check file before session creating")

          (let [sessions (-> (req! :get (urlf "/events"))
                             :body json/parse-string (get "data") set)]
            (is (not (contains? sessions ssid))
                "session should be absent"))))

      (testing "normal session creating"
        (let [ssid (gen-session-id)]
          (is (= [200 (empty-body 200)]
                 (->> {:params {:file-id  file-id
                                :event-id ssid}}
                      (json-req! :post (urlf "/events"))
                      resp->status+body))
              "Session created succesfully")

          (let [sessions (-> (req! :get (urlf "/events"))
                             :body json/parse-string (get "data") set)]
            (is (contains? sessions ssid)
                "session should be present"))))

      (testing "session double creating"
        (let [ssid (gen-session-id)]

          (create-test-session! file-id ssid)

          (is (= [400 (hd/new-error 400 "EAC" (format "Event with id '%s' is already created"
                                                      ssid))]
                 (->> {:params {:file-id  file-id
                                :event-id ssid}}
                      (json-req! :post (urlf "/events"))
                      resp->status+json))
              "should fail because session already exists")

          (let [sessions (-> (req! :get (urlf "/events"))
                             :body json/parse-string (get "data") set)]
            (is (contains? sessions ssid)
                "session should be present")))))))

(deftest session-get-event-log-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "get event log from not created session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created"
                                                      ssid))]
                 (-> (req! :get (urlf "/events/%s" ssid))
                     resp->status+json))
              "should fail if session not exists"))

        (testing "normal process"
          (let [ssid (gen-session-id)]

            (create-test-session! file-id ssid)

            (is (= [200 {:status 200 :data []}]
                   (-> (req! :get (urlf "/events/%s/event-log" ssid))
                       (resp->status+json)))
                "should return empty data")))))))

(deftest session-append-event-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]
      (load-test-file! file-id)

      (testing "communicating to not created session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created" ssid))]
                 (-> (req! :post (urlf "/events/%s/event-log/append" ssid))
                     resp->status+json))
              "Session does not exist")))

      (testing "normal log append"
        (let [ssid (gen-session-id)
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
                      "Accidental" "OwnGoal"}]

          (create-test-session! file-id ssid)

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
                 (-> (json-req! :post (urlf "/events/%s/event-log/append" ssid) {:params [event1]})
                     (resp->status+json :keywordize false)))
              "should successfully append log")

          (json-req! :post (urlf "/events/%s/event-log/append" ssid) {:params [event2]})

          (is (= [200 {"status" 200
                       "data"   [{"Accidental" "OwnGoal"
                                  "BodyPart"   "Head"
                                  "EventType"  "Goal"
                                  "GamePart"   "Half1"
                                  "Standart"   "Corner"
                                  "Team"       "Team1"}
                                 {"EventType"  "Goal"
                                  "Team"       "Team2"
                                  "GamePart"   "Half1"
                                  "Standart"   "Corner"
                                  "BodyPart"   "Head"
                                  "Accidental" "OwnGoal"}]}]
                 (-> (req! :get (urlf "/events/%s/event-log" ssid))
                     (resp->status+json :keywordize false)))
              "should return appended log"))))))

(deftest session-set-event-log-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "set logs on non-existed session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created" ssid))]
                 (-> (req! :post (urlf "/events/%s/event-log/set" ssid))
                     resp->status+json))
              "should return error")))

      (testing "normal event log set"
        (let [ssid (gen-session-id)
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
                      "Accidental" "OwnGoal"}]

          (create-test-session! file-id ssid)

          (req! :post
                (urlf "/files/%s/%s/event-log/append" file-id ssid)
                (json/generate-string [event1]))

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
                 (-> (json-req! :post (urlf "/events/%s/event-log/set" ssid) {:params [event2]})
                     (resp->status+json :keywordize false))))
          (is (= [200 {"status" 200
                       "data"   [{"EventType"  "Goal"
                                  "Team"       "Team2"
                                  "GamePart"   "Half1"
                                  "Standart"   "Corner"
                                  "BodyPart"   "Head"
                                  "Accidental" "OwnGoal"}]}]
                 (-> (req! :get (urlf "/events/%s/event-log" ssid))
                     (resp->status+json :keywordize false)))))))))

(deftest session-get-settlements-test
  (with-started-system [system]

    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "with not created session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created" ssid))]
                 (-> (req! :get (urlf "/events/%s/settlements" ssid))
                     resp->status+json))
              "should return error")))

      (testing "normal process"
        (let [ssid (gen-session-id)]

          (create-test-session! file-id ssid)

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
                 (-> (req! :get (urlf "/events/%s/settlements" ssid))
                     (resp->status+json :keywordize false)))))))))

(deftest session-get-workbook-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "with not created session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created" ssid))]
                 (-> (req! :get (urlf "/events/%s" ssid))
                     resp->status+json))
              "should return error")))

      (testing "normal process"
        (let [ssid (gen-session-id)]
          (create-test-session! file-id ssid)

          (let [resp (req! :get (urlf "/events/%s" ssid))]
            (is (= 200 (:status resp)))
            (is (= "application/octet-stream"
                   (-> resp :headers :content-type)))
            (is (= "attachment; filename=test-model.xlsx"
                   (-> resp :headers :content-disposition)))
            (is (pos? (-> resp :headers :content-length (Integer/parseInt))))))))))

(deftest session-finalize-test
  (with-started-system [system]
    (let [file-id (gen-file-id)]

      (load-test-file! file-id)

      (testing "with not created session"
        (let [ssid (gen-session-id)]
          (is (= [404 (hd/new-error 404 "ENF" (format "Event with id '%s' is not created" ssid))]
                 (-> (req! :delete (urlf "/events/%s" ssid))
                     resp->status+json))
              "should return error")))

      (testing "normal process"
        (let [ssid (gen-session-id)]

          (create-test-session! file-id ssid)

          (is (= [200 (empty-body 200)]
                 (-> (req! :delete (urlf "/events/%s" ssid))
                     (resp->status+body)))))))))