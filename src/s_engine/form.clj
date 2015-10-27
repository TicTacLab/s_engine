(ns s-engine.form)

(def replace-model-form
  {:fields      [{:name :file
                  :type :file}]
   :validations [[:required [:file]]]
   :enctype     "multipart/form-data"})

(def upload-model-form
  (-> replace-model-form
      (update-in [:fields] conj {:name :id, :type :text, :datatype :int})
      (update-in [:validations] conj [:required [:id]])))
