(ns s-engine.conducting
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [simulant.util :refer :all]
            [simulant.sim :as sim]
            [s-engine.conducting-sim]
            [s-engine.system :as s]
            [s-engine.config :as c]
            [s-engine.config :as c]
            [com.stuartsierra.component :as component]
            [clojure.test :refer [is]]))

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
      :model/meanNumberOfEvents  100
      :model/delayBetweenActions (int 30e3)}])

  (def conducting-model
    (-> @(d/transact sim-conn conducting-model-data)
        (tx-ent model-id)))


  (def conducting-test (sim/create-test sim-conn conducting-model
                                        {:db/id         (d/tempid :test)
                                         :test/duration (hours->msec 1)}))

  (def conducting-sim (sim/create-sim sim-conn conducting-test {:db/id            (d/tempid :sim)
                                                                :sim/systemURI    (str "datomic:free://localhost:4334/" (d/squuid))
                                                                :sim/processCount 10}))

  (def action-log
    (sim/create-action-log sim-conn conducting-sim))

  (def sim-clock (sim/create-fixed-clock sim-conn conducting-sim {:clock/multiplier 96}))

  (def pruns
    (->> #(sim/run-sim-process sim-uri (:db/id conducting-sim))
         (repeatedly (:sim/processCount conducting-sim))
         (into [])))

  (time
    (mapv (fn [prun] @(:runner prun)) pruns))

  (def simdb (d/db sim-conn))

  (def bookmakers
    (d/q '[:find [?agent ...]
           :where [?agent :agent/type :agent.type/bookmaker]]
         simdb))

  (def sums
    (>trace (->> (map (fn [bookmaker]
                        [bookmaker
                         (d/q '[:find (sum ?value) .
                                :in $ ?agent
                                :where
                                [?agent :agent/actions ?actions]
                                [?actions :action/value ?value]]
                              simdb
                              bookmaker)])
                      bookmakers)
                 (into {}))))

  (def bookie->file-id
    (->> (map (fn [bookie]
                [bookie
                 (d/q '[:find ?file-id .
                        :in $ ?agent
                        :where
                        [?agent :agent/file-id ?file-id]]
                      simdb bookie)])
              bookmakers)
         (into {})))

  (defn real-result-for-file-id [file-id]
    400)

  #_(doseq [[bookie expected-sum] sums]
    (let [real-sum (real-result-for-file-id (bookie->file-id bookie))]
      (is (= real-sum expected-sum)
          (format "Bookie %s calculates wrong" bookie))))

  (finally
    #_(swap! system component/stop)))

