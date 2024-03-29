(defproject s-engine "1.0.0-SNAPSHOT"
  :description "REST FOR SETTLEMENT EXCEL"
  :dependencies [[org.clojure/clojure "1.8.0-RC3"]
                 [malcolmx "0.1.8"]
                 [com.betinvest/noilly "0.1.4"]
                 [org.clojure/tools.trace "0.7.8"]
                 [org.slf4j/slf4j-api "1.7.12"]
                 [ch.qos.logback/logback-core "1.1.3"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/tools.logging "0.3.1"]
                 [clojurewerkz/cassaforte "2.0.2"]
                 [com.taoensso/nippy "2.9.0"]
                 [com.stuartsierra/component "0.2.3"]
                 [prismatic/schema "1.0.1"]
                 [cheshire "5.5.0"]
                 [javax.servlet/javax.servlet-api "3.1.0"]
                 [compojure "1.4.0"]
                 [ring "1.4.0"]
                 [dire "0.5.3"]
                 [org.clojure/core.cache "0.6.4"]
                 [com.datomic/simulant "0.1.8" :exclude [com.datomic/datomic-free]]
                 [com.datomic/datomic-free "0.9.5344"]]

  :source-paths ["src"]

  :repl-options {:timeout 120000
                 :init-ns user}
  :uberjar-name "s-engine-standalone.jar"
  :jvm-opts ["-Dlogback.configurationFile=logback.xml"]
  :repositories [["snapshots" {:url "s3p://com.betengines.repo/snapshots"
                               :username   "AKIAJULDYNE45TDQBDUQ"
                               :passphrase "xpNOSHkr069mqbY1tbWZjzTQPO0o5H0cmd568c1V"}]
                 ["releases" {:url "s3p://com.betengines.repo/releases"
                              :username   "AKIAJULDYNE45TDQBDUQ"
                              :passphrase "xpNOSHkr069mqbY1tbWZjzTQPO0o5H0cmd568c1V"}]]

  :plugins [[lein-ring "0.8.2"]
            [lein-environ "1.0.0"]
            [s3-wagon-private "1.1.2"]]

  :profiles {:dev  {:source-paths ["dev"]
                    :global-vars  {*warn-on-reflection* false}
                    :dependencies [[ns-tracker "0.3.0"]
                                   [aprint "0.1.3"]
                                   [http-kit.fake "0.2.2"]
                                   [http-kit "2.1.18"]
                                   [criterium "0.4.3"]
                                   [im.chit/vinyasa "0.4.1"]
                                   [org.clojure/tools.trace "0.7.8"]]

                    :injections   [(require '[vinyasa.inject :as inject])
                                   (require 'aprint.core)
                                   (require 'clojure.pprint)
                                   (require 'clojure.tools.trace)
                                   (require 'criterium.core)
                                   (inject/in clojure.core >
                                              [aprint.core aprint]
                                              [clojure.pprint pprint]
                                              [clojure.tools.trace trace]
                                              [criterium.core bench])]}
             :test {:dependencies [[http-kit "2.1.18"]]}}
  :main s-engine.main)
