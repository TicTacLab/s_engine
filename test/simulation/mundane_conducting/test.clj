(ns simulation.mundane_conducting.test
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [simulant.util :refer :all]
            [simulant.sim :as sim]
            [simulation.mundane_conducting.setup]
            [s-engine.test-helper :refer :all]
            [s-engine.system :as s]
            [s-engine.config :as c]
            [s-engine.config :as c]
            [com.stuartsierra.component :as component]
            [s-engine.client :as se]
            [cheshire.core :as json]))

(defn reset-conn
  "Reset connection to a scratch database. Use memory database if no
   URL passed in."
  [uri]
  (d/delete-database uri)
  (d/create-database uri)
  (d/connect uri))

(defn load-schema
  [conn resource]
  (let [m (-> resource io/resource slurp read-string)]
    (doseq [v (vals m)]
      (doseq [tx v]
        @(d/transact conn tx)))))

(def sim-uri (str "datomic:free://localhost:4334/" (d/squuid)))

(def sim-conn (reset-conn sim-uri))

(load-schema sim-conn "simulant/schema.edn")

(load-schema sim-conn "simulation/mundane_conducting/schema.edn")

(defonce system (atom (s/new-system @c/config)))

(try

  (swap! system (comp component/start component/stop))

  (def model-id (d/tempid :model))


  (def conducting-model
    (-> @(d/transact sim-conn [{:db/id                     model-id
                                :model/type                :model.type/conducting
                                :model/bookmakersCount     100
                                :model/delayBetweenActions (* 30 1000)}])
        (tx-ent model-id)))


  (def conducting-test (sim/create-test sim-conn conducting-model
                                        {:db/id         (d/tempid :test)
                                         :test/duration (hours->msec 12)}))

  (def conducting-sim (sim/create-sim sim-conn conducting-test {:db/id            (d/tempid :sim)
                                                                :sim/systemURI    (str "datomic:free://localhost:4334/" (d/squuid))
                                                                :sim/processCount 10}))

  (def action-log
    (sim/create-action-log sim-conn conducting-sim))

  (def sim-clock (sim/create-fixed-clock sim-conn conducting-sim {:clock/multiplier 960}))

  (comment
    (def pruns
      (->> #(sim/run-sim-process sim-uri (:db/id conducting-sim))
           (repeatedly (:sim/processCount conducting-sim))
           (into [])))

    (time
      (mapv (fn [prun] @(:runner prun)) pruns)))

  (def simdb (d/db sim-conn))

  (def ssids
    (d/q '[:find [?ssid ...]
           :where
           [?agent :agent/type :agent.type/bookmaker]
           [?agent :agent/session-id ?ssid]]
         simdb))

  (defn real-result-for-session-id [session-id]
    (-> (se/get-settlements session-id)
        :body
        json/parse-string
        (get-in ["data" 0 "Calc"])))

  (doseq [ssid ssids]

    (let [lastSetEventTime
          (d/q '[:find (max ?time) .
                 :in $ ?ssid
                 :where
                 [?agent :agent/type :agent.type/bookmaker]
                 [?agent :agent/session-id ?ssid]
                 [?agent :agent/actions ?action]
                 [?action :action/type :action.type/setEvents]
                 [?action :action/atTime ?time]]
               simdb
               ssid)

          setEventsValues
          (->> (d/q '[:find (pull ?action [:action/values]) .
                      :in $ ?ssid ?time
                      :where
                      [?agent :agent/type :agent.type/bookmaker]
                      [?agent :agent/session-id ?ssid]
                      [?agent :agent/actions ?action]
                      [?action :action/type :action.type/setEvents]
                      [?action :action/atTime ?time]]
                    simdb
                    ssid
                    lastSetEventTime)
               :action/values)

          appendEventsValues
          (d/q '[:find [?value ...]
                 :in $ ?ssid ?time
                 :with ?action
                 :where
                 [?agent :agent/type :agent.type/bookmaker]
                 [?agent :agent/session-id ?ssid]
                 [?agent :agent/actions ?action]
                 [?action :action/type :action.type/appendEvent]
                 [?action :action/value ?value]
                 [?action :action/atTime ?actionTime]
                 [(> ?actionTime ?time)]]
               simdb
               ssid
               lastSetEventTime)

          expected-sum
          (+ (apply + setEventsValues)
             (apply + appendEventsValues))

          real-sum
          (real-result-for-session-id ssid)]

      (assert (= real-sum (double expected-sum))
              (format "Bookie %s calculates wrong: (not= %f %f)"
                      ssid real-sum (double expected-sum)))))

  (catch Exception _
    (swap! system component/stop)))

