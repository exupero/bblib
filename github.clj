(ns github
  (:require config
            http))

(def host "https://api.github.com")
(def token (System/getenv "GITHUB_TOKEN"))

(defn configure-request [req]
  (-> req
      (assoc :config {:host host})
      (update :headers merge
              {"Accept" "application/vnd.github+json"
               "Authorization" (str "Bearer " token)
               "X-GitHub-Api-Version" "2022-11-28"})))

(defn pull-request [org repo number]
  (configure-request
    {:path (str "/repos/" (name org) "/" (name repo) "/pulls/" number)
     :method :get}))

(defn search [q]
  (configure-request
    {:path "/search/issues"
     :method :get
     :query-params {:q q}}))
