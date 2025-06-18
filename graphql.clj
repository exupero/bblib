(ns graphql
  (:require http))

(defn query [config q]
  (-> config
      (assoc :body {:query q})
      http/request!))
