(ns databricks
  (:require http)
  (:import [java.util Base64]))

; API docs: https://docs.databricks.com/api/workspace/introduction

(def host (str (System/getenv "DATABRICKS_HOST") "/api/2.0"))
(def token (System/getenv "DATABRICKS_TOKEN"))

(defn configure-request [{:keys [path] :as params}]
  (-> params
      (dissoc :path)
      (assoc :url (str host path)
             :headers {"Authorization" (str "Bearer " token)
                       "Accept" "application/json"
                       "Content-Type" "application/json"})))

(defn secret [scope k]
  {:path "/secrets/get"
   :method :get
   :query-params {:scope scope :key k}
   :getter #(-> % :body :value
                (->> (.decode (Base64/getDecoder)))
                (String. "UTF-8"))})
