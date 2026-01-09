(ns github
  (:require [clojure.string :as str]))

(def host "https://api.github.com")
(def token (System/getenv "GITHUB_TOKEN"))

(defn owner+repo [repo]
  (if (keyword? repo)
    [(namespace repo) (name repo)]
    (str/split repo #"/" 2)))

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

(defn search-code [q]
  (configure-request
    {:path "/search/code"
     :method :get
     :query-params {:q q}}))

(defn search-issues [q]
  (configure-request
    {:path "/search/issues"
     :method :get
     :query-params {:q q}}))

(defn search-repos [q]
  (configure-request
    {:path "/search/repositories"
     :method :get
     :query-params {:q q}}))

(defn new-labels
  ([repo number labels]
   (let [[owner repo] (owner+repo repo)]
     (new-labels owner repo number labels)))
  ([owner repo number labels]
   (configure-request
     {:path (str "/repos/" (name owner) "/" (name repo) "/issues/" number "/labels")
      :method :post
      :body {:labels labels}})))

(defn new-pull-request
  ([repo params]
   (let [[owner repo] (owner+repo repo)]
     (new-pull-request owner repo params)))
  ([owner repo params]
   (configure-request
     {:path (str "/repos/" (name owner) "/" (name repo) "/pulls")
      :method :post
      :body params})))

(defn request-reviewers
  ([repo number reviewers]
   (let [[owner repo] (owner+repo repo)]
     (request-reviewers owner repo number reviewers)))
  ([owner repo number reviewers]
   (configure-request
     {:path (str "/repos/" (name owner) "/" (name repo) "/pulls/" number "/requested_reviewers")
      :method :post
      :body reviewers})))

(defn new-review
  ([repo number review]
   (let [[owner repo] (owner+repo repo)]
     (new-review owner repo number review)))
  ([owner repo number review]
   (configure-request
     {:path (str "/repos/" (name owner) "/" (name repo) "/pulls/" number "/reviews")
      :method :post
      :body review})))

(defn new-comment
  ([repo number body]
   (let [[owner repo] (owner+repo repo)]
     (new-comment owner repo number body)))
  ([owner repo number body]
   (configure-request
     {:path (str "/repos/" (name owner) "/" (name repo) "/issues/" number "/comments")
      :method :post
      :body {:body body}})))
