(ns graphql
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            http))

(defn query [config q]
  (-> config
      (assoc :body {:query q})
      http/request!))

(defn ->query [edn-data]
  (letfn [(format-value [v]
            (cond
              (keyword? v) (name v)
              (vector? v) (str "[" (str/join "," (map format-value v)) "]")
              (map? v) (format-object v)
              :else (json/generate-string v)))
          (format-object [obj]
            (str "{"
                 (str/join ","
                   (for [[k v] obj]
                     (str (name k) ":" (format-value v))))
                 "}"))
          (format-arguments [args]
            (when (seq args)
              (str "("
                   (str/join ","
                     (for [[k v] args]
                       (str (name k) ":" (format-value v))))
                   ")")))
          (format-field [field]
            (cond
              (keyword? field) (name field)
              (string? field) field
              (map? field)
              , (let [[field-name field-spec] (first field)]
                  (cond
                    (map? field-spec)
                    , (let [{:keys [args fields]} field-spec
                            args-str (format-arguments args)
                            fields-str (when fields (format-selection-set fields))]
                        (str (name field-name) args-str fields-str))
                    (vector? field-spec)
                    , (str (name field-name) (format-selection-set field-spec))
                    :else
                    , (str (name field-name) "{" (format-field field-spec) "}")))
              :else (str field)))
          (format-selection-set [fields]
            (when (seq fields)
              (str "{"
                   (str/join " " (map format-field fields))
                   "}")))]
    (cond
      (map? edn-data)
      , (let [{:keys [query mutation subscription]} edn-data]
          (cond
            query (str "query" (format-selection-set query))
            mutation (str "mutation" (format-selection-set mutation))
            subscription (str "subscription" (format-selection-set subscription))
            :else (format-selection-set [edn-data])))
      (vector? edn-data)
      , (str "query" (format-selection-set edn-data))
      :else
      , (str "query{" (format-field edn-data) "}"))))

(comment
  (->query [:user :name :email]) ; "query{user name email}"
  (->query [{:user {:args {:id 123}}
             :fields [:name :email]}]) ; "query{user(id:123)}"
  (->query
    {:query [{:users {:args {:first 10
                             :status "ACTIVE"
                             :tags ["important" "priority"]}
                      :fields [:id :name
                               {:posts {:args {:limit 5}
                                        :fields [:title :createdAt]}}]}}]})) ; "query{users(first:10,status:\"ACTIVE\",tags:[\"important\",\"priority\"]){id name posts(limit:5){title createdAt}}}"
