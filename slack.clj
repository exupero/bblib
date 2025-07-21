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
