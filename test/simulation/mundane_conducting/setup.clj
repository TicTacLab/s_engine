(ns simulation.mundane_conducting.setup
  (:require [simulant.sim :as sim]
            [simulant.util :refer :all]
            [datomic.api :as d]
            [clojure.data.generators :as gen]
            [s-engine.client :as se])
  (:import (java.util UUID)))

(defn create-test [conn model test]
  (require-keys test :db/id :test/duration)
  (-> @(d/transact conn [(assoc test
                           :test/type :test.type/conducting
                           :model/_tests (e model))])
      (tx-ent (e test))))

(defn create-bookmakers
  [conn test]
  (let [model (-> test :model/_tests solo)
        bookmakers (repeatedly (:model/bookmakersCount model)
                               (fn []
                                 {:db/id            (d/tempid :test)
                                  :agent/type       :agent.type/bookmaker
                                  :agent/file-id    (mod (+ (System/nanoTime) (rand-int 100))
                                                         Integer/MAX_VALUE)
                                  :agent/session-id (UUID/randomUUID)
                                  :test/_agents     (e test)}))
        txresult (d/transact conn bookmakers)]
    (tx-entids @txresult (map :db/id bookmakers))))

(defn generate-append-events [test bookmaker at-time]
  {:db/id          (d/tempid :test)
   :action/type    :action.type/appendEvent
   :action/value   (gen/geometric (/ 1 20))
   :action/atTime  at-time
   :agent/_actions (e bookmaker)})

(defn generate-set-events [test bookmaker at-time]
  {:db/id                  (d/tempid :test)
   :action/type            :action.type/setEvents
   :action/values          (repeatedly (gen/geometric (/ 1 6)) #(gen/geometric (/ 1 20)))
   :action/atTime          at-time
   :agent/_actions         (e bookmaker)})

(defn generate-events [test bookmaker at-time]
  (gen/weighted {#(generate-append-events test bookmaker at-time) 9
                 #(generate-set-events test bookmaker at-time)     1}))

(defn generate-bookmaker-work
  [test bookmaker]
  (let [model (-> test :model/_tests first)
        limit (:test/duration test)
        step #(gen/geometric (/ 1 (:model/delayBetweenActions model)))]
    (->> (reductions + (repeatedly step))
         (take-while (fn [t] (< t limit)))
         (map #(generate-events test bookmaker %)))))

(defn generate-bookmakers-work [test bookmakers]
  (map #(generate-bookmaker-work test %) bookmakers))

(defmethod sim/create-test :model.type/conducting
  [conn model test]
  (let [test (create-test conn model test)
        bookmakers (create-bookmakers conn test)]
    (transact-batch conn (generate-bookmakers-work test bookmakers) 1000)
    (d/entity (d/db conn) (e test))))

(defn upload-files! [file-ids]
  (let [file-path "test/resources/Simulant.xlsx"]
    (doseq [fid file-ids]
      (println "Uploading file" fid)
      (se/upload-file fid file-path))))

(defmethod sim/create-sim :test.type/conducting
  [sim-conn test sim]

  (let [agents (:test/agents test)]
    (upload-files! (map :agent/file-id agents))
    (doseq [[fid sid] (map (juxt :agent/file-id :agent/session-id) agents)]
      (se/create-session fid sid)
      (println "Session Created:" sid)))

  (d/create-database (:sim/systemURI sim))
  (-> @(d/transact sim-conn (sim/construct-basic-sim test sim))
      (tx-ent (:db/id sim))))

(defn append-event! [session-id value]
  (let [message {"EventType" "Ping"
                 "Value" value}]
    (se/append-event session-id message)))

(defmethod sim/perform-action :action.type/appendEvent
  [action process]
  (let [sim (-> process :sim/_processes only)
        action-log (getx sim/*services* :simulant.sim/actionLog)
        session-id (-> action :agent/_actions only :agent/session-id)
        value (:action/value action)
        before (System/nanoTime)]

    (append-event! session-id value)

    (action-log [{:actionLog/nsec (- (System/nanoTime) before)
                  :db/id (d/tempid :db.part/user)
                  :actionLog/sim (e sim)
                  :actionLog/action (e action)}])))

(defn set-events! [session-id values]
  (let [messages (map #(hash-map "EventType" "Ping", "Value" %) values)]
    (se/set-events session-id messages)))

(defmethod sim/perform-action :action.type/setEvents
  [action process]
  (let [sim (-> process :sim/_processes only)
        action-log (getx sim/*services* :simulant.sim/actionLog)
        session-id (-> action :agent/_actions only :agent/session-id)
        values (:action/values action)
        before (System/nanoTime)]

    (set-events! session-id values)

    (action-log [{:actionLog/nsec (- (System/nanoTime) before)
                  :db/id (d/tempid :db.part/user)
                  :actionLog/sim (e sim)
                  :actionLog/action (e action)}])))