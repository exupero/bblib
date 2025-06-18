(ns http
  (:require [clojure.string :as str]
            babashka.curl
            [cheshire.core :as json]
            [org.httpkit.client :as http-client]
            [time :as t]
            config))

(def requesters
  {:curl babashka.curl/request
   :httpkit #(deref (http-client/request %))})

(def ^:dynamic *config* nil)
(defn config []
  (or *config* (config/read :host)))

(defn restructure-query-params [params]
  (for [[k vs] params
        v (if (coll? vs) vs [vs])]
    [k v]))

(defn with-config [{:keys [path] :as req} {:keys [host creds] :or {creds identity}}]
  (cond-> req
    path
    , (-> (assoc :url (str host path))
        (dissoc :path))
    (req :query-params)
    , (update :query-params restructure-query-params)
    (nil? (get-in req [:headers "Accept"]))
    , (assoc-in [:headers "Accept"] "application/json")
    (nil? (get-in req [:headers "Content-Type"]))
    , (assoc-in [:headers "Content-Type"] "application/json")
    (some-> (req :body) coll?)
    , (update :body json/generate-string)
    true
    , creds))

(defn url-encode [s]
  (-> s
      (java.net.URLEncoder/encode "UTF-8")
      (str/replace "+" "%20")))

(defn assoc-time [[t response]]
  (assoc response :time t))

(defn success? [{:keys [status]}]
  (<= 200 status 299))

(defn json-content-type [content-types]
  (let [regex #"^application/(?:vnd\.[^.]+\.v1\+)?json"]
    (if (string? content-types)
      (re-find regex content-types)
      (some (partial re-find regex) content-types))))

(defn handle-error [{:keys [status error] :as response}]
  (cond
    error         , (throw error)
    (nil? status) , (throw (Exception. "Response has no HTTP status; is host running?"))
    :else         , response))

(defn parse-query-params [s]
  (some-> s
          (str/split #"&")
          (->> (into {} (map #(str/split % #"=" 2))))))

(defn parse-body [{:keys [headers] :as response}]
  (let [content-type (or (headers "content-type")
                         (headers :content-type))]
    (cond-> response
      (and content-type (json-content-type content-type) (response :body))
      , (update :body json/parse-string true))))

(defn get-result [response success? getter failure]
  (if (success? response)
    (let [result (getter response)]
      (if (instance? clojure.lang.IObj result)
        (with-meta result response)
        result))
    (failure response)))

(defn println-to-stderr [s]
  (binding [*out* *err*]
    (println s)))

(defn format-query-params [qs]
  (when (seq qs)
    (transduce
      (comp
        (map (fn [[k v]]
               (str (name k) "=" (url-encode (name v)))))
        (interpose "&"))
      str "?" qs)))

(defn format-as-curl [{:keys [url method headers body basic-auth query-params]}]
  (format "curl -X %s '%s%s' %s %s %s"
          (str/upper-case (name method))
          url
          (or (format-query-params query-params) "")
          (transduce
            (comp
              (map (fn [[k v]]
                     (str "-H '" (name k) ": " v \')))
              (interpose \space))
            str headers)
          (if basic-auth (str/join \: basic-auth) "")
          (if body
            (format "-d'%s'" (str/replace body #"'" "'\\\\''"))
            "")))

(defn request!
  ([spec] (request! spec nil))
  ([spec opts]
   (let [{:keys [config log time lib success getter failure parse-body? as-curl?] :as spec
          :or {log false
               time false
               lib :curl
               success success?
               parse-body? true
               getter :body
               failure identity
               as-curl? false}}
         , (merge spec opts)
         requester (requesters lib)
         log-curl? (= "1" (System/getenv "LOG_CURL"))]
     (-> spec
         (assoc :throw false)
         (with-config (or config (http/config)))
         (cond->
           as-curl?       (format-as-curl)
           (not as-curl?) (cond->
                            log         (doto prn)
                            log-curl?   (doto (-> format-as-curl println-to-stderr))
                            time        (-> requester t/timed assoc-time)
                            (not time)  requester
                            log         (doto prn)
                            true        handle-error
                            true        (dissoc :opts :process)
                            parse-body? parse-body
                            true        (get-result success getter failure)))))))

(defn parse-header [s]
  (let [[k v] (str/split s #":" 2)]
    [k (str/trim v)]))

(defn request!! [req]
  (let [{:keys [status] :as response} (request! req)]
    (if (number? status)
      (if (<= 200 status 299)
        response
        (throw (Exception. (pr-str {:request req :response response}))))
      response)))
