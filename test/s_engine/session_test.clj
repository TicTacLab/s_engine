(ns s-engine.session-test
  (:require [clojure.test :refer :all]
            [s-engine.session :refer :all]
            [com.stuartsierra.component :as component]))

(defn make-storage []
  (-> (new-session-storage)
      (component/start)))

(deftest create-test
  (let [storage (make-storage)
        session (create! storage "model1" "session1")]
    (is (= session
           (get @(:session-table storage) "session1")))))

(deftest get-one-test
  (let [storage (make-storage)
        session (create! storage "model1" "session1")]
    (is (= session
           (get-one storage "session1")))))

(deftest append-event-test
  (let [storage (make-storage)
        session (create! storage "model1" "session1")
        event (->SessionEvent "type1" 0 0 {"attr" 1})]
    (append-event! session event)))

(deftest get-events-test
  (let [storage (make-storage)
        session (create! storage "model1" "session1")
        event1 (->SessionEvent "type1" 0 0 {"attr" 1})
        event2 (->SessionEvent "type1" 0 1 {"attr" 1})]
    (do
      (is (empty? (get-events session)))
      (append-event! session event1)
      (is (= [event1] (get-events session)))
      (append-event! session event2)
      (is (= [event1 event2] (get-events session))))))

(deftest set-events-test)

(deftest get-out-test
  (let [storage (make-storage)
        session (create! storage "model1" "session1")
        event (->SessionEvent "type1" 0 0 {"attr" 1})]
    (do
      (is (= [{:market "Market1", :out "A"}]
             (get-out session)))
      (append-event! session event)
      (is (= [{:market "Market1", :out "B"}]
             (get-out session))))))