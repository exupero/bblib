(ns slack)

(def host "https://slack.com/api")
(def token (System/getenv "SLACK_TOKEN"))

(defn configure-request [req]
  (-> req
      (assoc :config {:host host})
      (update :headers merge
              {"Accept" "application/json"
               "Authorization" (str "Bearer " token)})))

(defn set-status [text emoji expiration]
  (configure-request
    {:path "/users.profile.set"
     :method :post
     :body {:profile {:status_text text
                      :status_emoji emoji
                      :status_expiration expiration}}}))

(defn conversation-history
  ([channel-id]
   (conversation-history channel-id nil))
  ([channel-id {:keys [oldest]}]
   (configure-request
     {:path "/conversations.history"
      :method :get
      :query-params {:channel channel-id
                     :limit 100
                     :oldest oldest}})))

(defn post-message
  ([channel-id text]
   (post-message channel-id text nil))
  ([channel-id text thread-ts]
   (configure-request
     {:path "/chat.postMessage"
      :method :post
      :body (cond-> {:channel channel-id
                     :text text
                     :thread_ts thread-ts})})))
