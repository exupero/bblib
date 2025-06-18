(ns mongo
  (:require [babashka.process :as p]
            [cheshire.core :as json]))

(defn fetch-json [db script]
  (-> (p/shell {:out :string} "mongosh" (name db) "--eval"
               (str "JSON.stringify(" script ")"))
      :out
      json/parse-string))
