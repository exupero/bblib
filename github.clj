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

(defn parse-org+repo+number [url]
  (let [[_ org repo number] (re-find #"/([^/]+)/([^/]+)/pull/(\d+)" url)]
    [org repo number]))

(defn pull-request [owner repo number]
  (configure-request
    {:path (str "/repos/" (name owner) "/" (name repo) "/pulls/" number)
     :method :get}))

(defn search [q]
  (configure-request
    {:path "/search/issues"
     :method :get
     :query-params {:q q}}))

(defn add-labels [owner repo number labels]
  (configure-request
    {:path (str "/repos/" (name owner) "/" (name repo) "/issues/" number "/labels")
     :method :post
     :body {:labels labels}}))

(defn new-pull-request [owner repo params]
  (configure-request
    {:path (str "/repos/" (name owner) "/" (name repo) "/pulls")
     :method :post
     :body params}))

(defn request-reviewers [owner repo number reviewers]
  (configure-request
    {:path (str "/repos/" (name owner) "/" (name repo) "/pulls/" number "/requested_reviewers")
     :method :post
     :body reviewers}))

(defn new-review [owner repo number review]
  (configure-request
    {:path (str "/repos/" (name owner) "/" (name repo) "/pulls/" number "/reviews")
     :method :post
     :body review}))
