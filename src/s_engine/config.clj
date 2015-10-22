(ns s-engine.config
  (:require [cheshire.core :as json]))

(def config (atom (json/parse-string (slurp "config.json") true)))
