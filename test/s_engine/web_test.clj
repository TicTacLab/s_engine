(ns s-engine.web-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [s-engine.web :refer :all]
            [s-engine.config :as c]
            [s-engine.system :as s]
            [s-engine.session :as session]))

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

(deftest session-get-event-log-test
  (is (= [200 {"data" []}]
         (-> (make-url "/files" 1 1 "event-log")
             (http/get)
             (deref)
             (resp->status+body)))))

(deftest session-append-event-test
  (let [{:keys [session-storage storage]} system
        event {:event-type "ev1"
               :min        0
               :sec        0
               :attrs      [{:name  "attr1"
                             :value "1"}]}
        session (session/get-or-create! session-storage storage 1 1)]
    (is (empty? (session/get-events session)))
    (is (= [200 nil]
           (-> (make-url "/files" 1 1 "event-log/append")
               (http/post {:body (json/generate-string {:event event})})
               (deref)
               (resp->status+body))))
    (is (= [event] (session/get-events session)))))

(deftest session-set-event-log-test)

(deftest session-get-settlements-test
  (is (= [200 {"data" [{"market" "Market1", "out" "A"}]}]
         (-> (make-url "/files" 1 1 "settlements")
             (http/get) (deref)
             (resp->status+body)))))

(deftest session-finalize-test
  (let [{:keys [session-storage storage]} system
        session (session/get-or-create! session-storage storage 1 1)]
    (is (= [204 nil]
           (-> (make-url "/files" 1 2)
               (http/delete) (deref)
               (resp->status+body)))
        "should delete non-existing session")
    (is (= [204 nil]
           (-> (make-url "/files" 1 1)
               (http/delete) (deref)
               (resp->status+body))))))