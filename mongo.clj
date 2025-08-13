(ns mongo
  (:refer-clojure :exclude [eval find])
  (:require clojure.walk
            [babashka.process :as p]
            [cheshire.core :as json]))

(defn prepare [value]
  (-> value
      (->> (clojure.walk/postwalk (fn [node]
                                    (if (instance? java.util.regex.Pattern node)
                                      {"$regex" (str node)}
                                      node))))
      json/generate-string))

(defn eval [db script]
  (-> (p/shell {:out :string} "mongosh" (name db) "--eval" script)
      :out))

(defn find
  ([db coll query]
   (-> (eval db (str "JSON.stringify(db['" (name coll) "'].find(" (prepare query) ").toArray())"))
       json/parse-string))
  ([db coll query projection]
   (-> (eval db (str "JSON.stringify(db['" (name coll) "'].find(" (prepare query) ", " (prepare projection) ").toArray())"))
       json/parse-string)))

(defn mutate! [db coll query mutation]
  (let [result (eval db (str "JSON.stringify(db['" (name coll) "'].updateMany(" (prepare query) ", " (prepare mutation) "))"))]
    (try
      (json/parse-string result)
      (catch Exception e
        (throw (ex-info "Failed to parse mutation result" {:result result} e))))))

(defn update-one! [db coll doc]
  (let [id (get doc :_id)
        doc (dissoc doc :_id)]
    (-> (eval db (str "JSON.stringify("
                        "db['" (name coll) "'].updateOne({ _id: ObjectId('" id "') }, { $set: " (prepare doc) " })"
                      ")"))
        json/parse-string)))
