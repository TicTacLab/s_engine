(ns s-engine.conducting
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [simulant.util :refer :all]
            [simulant.sim :as sim]
            [s-engine.conducting-sim]
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

(def sim-uri (str "datomic:free://localhost:4334/" (d/squuid) "_conducting"))

(def sim-conn (reset-conn sim-uri))

(load-schema sim-conn "simulant/schema.edn")

(load-schema sim-conn "s_engine/conducting-sim.edn")

(defonce system (atom (s/new-system @c/config)))

(try

  (swap! system (comp component/start component/stop))

  (def model-id (d/tempid :model))
  (def conducting-model-data
    [{:db/id                     model-id
      :model/type                :model.type/conducting
      :model/bookmakersCount     10
      :model/delayBetweenActions (* 30 1000)}])

  (def conducting-model
    (-> @(d/transact sim-conn conducting-model-data)
        (tx-ent model-id)))


  (def conducting-test (sim/create-test sim-conn conducting-model
                                        {:db/id         (d/tempid :test)
                                         :test/duration (hours->msec 2)}))

  (def conducting-sim (sim/create-sim sim-conn conducting-test {:db/id            (d/tempid :sim)
                                                                :sim/systemURI    (str "datomic:free://localhost:4334/" (d/squuid))
                                                                :sim/processCount 10}))

  (def action-log
    (sim/create-action-log sim-conn conducting-sim))

  (def sim-clock (sim/create-fixed-clock sim-conn conducting-sim {:clock/multiplier 960}))

  (def pruns
    (->> #(sim/run-sim-process sim-uri (:db/id conducting-sim))
         (repeatedly (:sim/processCount conducting-sim))
         (into [])))

  (time
    (mapv (fn [prun] @(:runner prun)) pruns))

  (def simdb (d/db sim-conn))

  (def sums
    (->> (d/q '[:find ?ssid (count ?action) (sum ?value)
                :where
                [?agent :agent/type :agent.type/bookmaker]
                [?agent :agent/session-id ?ssid]
                [?agent :agent/actions ?action]
                [?action :action/value ?value]]
              simdb)
         (map (juxt first #(nth % 2)))
         (into {})))

  (defn real-result-for-session-id [session-id]
    (-> (se/get-settlements session-id)
        :body
        json/parse-string
        (get-in ["data" 0 "Calc"])))

  (doseq [[session-id expected-sum] sums]
    (let [real-sum (real-result-for-session-id session-id)]
      (assert (= real-sum (double expected-sum))
          (format "Bookie %s calculates wrong" session-id))))

  (finally
    (swap! system component/stop)))

