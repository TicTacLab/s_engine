(ns s-engine.storage.model
  (:import (java.io File)
           (java.nio.file Paths Files))
  (:require [clojurewerkz.cassaforte.cql :as cql]
            [clojurewerkz.cassaforte.query :refer [where columns limit]]))

(defn write-model!
  [storage model-id tempfile filename]
  (let [file-bytes (Files/readAllBytes (Paths/get (.toURI ^File tempfile)))
        {:keys [conn]} storage
        model {:id model-id, :name filename, :file file-bytes}]
    (cql/insert conn "sengine_models" model)))

(defn delete-model! [storage model-id]
  (let [{:keys [conn]} storage]
    (cql/delete conn "sengine_models"
                (where [[= :id model-id]]))))

(defn model-exists? [storage model-id]
  (let [{:keys [conn]} storage]
    (seq (cql/select conn "models"
                     (columns :id)
                     (where [[= :id model-id]])
                     (limit 1)))))

(defn get-model [storage model-id]
  (let [{:keys [conn]} storage]
    (cql/get-one conn "sengine_models"
                 (columns :id :name :file)
                 (where [[= :id model-id]]))))