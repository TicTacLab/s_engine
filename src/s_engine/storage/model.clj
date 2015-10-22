(ns s-engine.model
  (:import (java.io File)
           (java.nio.file Paths Files)))

(defn write-model!
  [storage model-id tempfile filename]
  (let [file-bytes (Files/readAllBytes (Paths/get (.toURI ^File tempfile)))
        {:keys [conn]} storage
        model {:id model-id, :name filename, :file file-bytes}]
    (cql/insert conn "models" model)))

(defn get-model [storage model-id]
  (let [{:keys [conn]} storage]
    (cql/get-one conn "settlement-models"
                 (columns :id :name :file)
                 (where [[= :id model-id]]))))