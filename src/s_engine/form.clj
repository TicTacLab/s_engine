(ns s-engine.form)

(def upload-model-form
  {:fields      [{:name :id
                  :type :text
                  :datatype :int}
                 {:name :file
                  :type :file}]
   :validations [[:required [:id]]
                 [:required [:file]]]
   :enctype     "multipart/form-data"})
