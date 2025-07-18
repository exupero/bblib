(ns jira
  (:require [clojure.string :as str]
            http))

(def host (System/getenv "JIRA_HOST"))
(def username (System/getenv "JIRA_USERNAME"))
(def token (System/getenv "JIRA_TOKEN"))

(def normalize-project-key (comp str/upper-case name))

(defn configure-request [{:keys [path] :as params}]
  (-> params
      (dissoc :path)
      (assoc :url (str host path)
             :basic-auth [username token]
             :headers {"Accept" "application/json"
                       "Content-Type" "application/json"})))

(defn search [query]
  (configure-request
    {:path "/search"
     :method :get
     :query-params {:jql query}}))

(defn ticket
  ([ticket-key]
   (configure-request
     {:path (str "/issue/" (name ticket-key))
      :method :get}))
  ([project n]
   (ticket (str (normalize-project-key project) \- n))))

(defn components
  ([project] (components project {}))
  ([project params]
   (configure-request
     {:path (str "/project/" (normalize-project-key project) "/component")
      :method :get
      :query-params params})))

(defn project-statuses [project]
  (configure-request
    {:path (str "/project/" (normalize-project-key project) "/statuses")
     :method :get}))

(defn ticket-transitions [ticket]
  (configure-request
    {:path (str "/issue/" ticket "/transitions")
     :method :get}))

(defn transition! [ticket status]
  (let [[{:keys [id]}] (-> (ticket-transitions ticket)
                           http/request!
                           :transitions
                           (->> (filter (comp #{status} :name))))]
    (-> (configure-request
          {:path (str "/issue/" ticket "/transitions")
           :method :post
           :body {:transition {:id id}}})
        http/request!)))

(defn ticket-update [ticket-id]
  (configure-request
    {:path (str "/issue/" (normalize-project-key ticket-id))
     :method :put}))

(defn with-assignee [ticket assignee-account-id]
  (assoc-in ticket [:body :fields :assignee :id] assignee-account-id))

(defn with-version [ticket version-id]
  (assoc-in ticket [:body :fields :versions] [{:id version-id}]))

(defn new-ticket [{:keys [project summary description issuetype components] :as fields}]
  (configure-request
    {:path (str "/issue/")
     :method :post
     :body {:fields (merge fields
                           {:project {:key (normalize-project-key project)}
                            :issuetype {:name (str/capitalize (name issuetype))}
                            :summary summary
                            :description description
                            :components (map #(do {:name %}) components)})}}))

(defn link [from-ticket to-ticket type]
  (configure-request
    {:path (str "/issueLink")
     :method :post
     :body {:type {:name type}
            :inwardIssue {:key to-ticket}
            :outwardIssue {:key from-ticket}}}))

(defn linked-keys [{:keys [outwardIssue inwardIssue]}]
  (cond-> []
    outwardIssue (conj (outwardIssue :key))
    inwardIssue (conj (inwardIssue :key))))

(defn versions [project]
  (configure-request
    {:path (str "/project/" (normalize-project-key project) "/versions")
     :method :get}))
