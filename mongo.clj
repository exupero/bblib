(ns mongo
  (:refer-clojure :exclude [eval find])
  (:require [babashka.process :as p]
            [cheshire.core :as json]))

(defn eval [db script]
  (-> (p/shell {:out :string} "mongosh" (name db) "--eval" script)
      :out))

(defn find [db coll query]
  (-> (eval db (str "JSON.stringify(db." (name coll) ".find(" (json/generate-string query) ").toArray())"))
      json/parse-string))

(defn update-one! [db coll doc]
  (let [id (get doc :_id)
        doc (dissoc doc :_id)]
    (-> (eval db #p (str "JSON.stringify("
                           "db." (name coll) ".updateOne({ _id: ObjectId('" id "') }, { $set: " (json/generate-string doc) " })"
                         ")"))
        json/parse-string)))
