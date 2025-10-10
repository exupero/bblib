(ns browse
  (:require [clojure.java.browse :as browse]
            [clojure.string :as str]
            [org.httpkit.server :as server]))

(defn browse [html static-path]
  (let [port (with-open [sock (java.net.ServerSocket. 0)]
               (.getLocalPort sock))]
    (server/run-server
      (fn [{:keys [uri]}]
        (cond
          (re-find #"^/static/.*\.js$" uri)
          , {:status 200
             :body (slurp (str static-path (str/replace uri #"^/static" "")))
             :headers {"Content-type" "application/javascript"}}
          (= "/" uri)
          , {:status 200 :body html}))
      {:port port})
    (browse/browse-url (str "http://localhost:" port))
    @(promise)))

(defn browse-once [html static-path]
  (let [port (with-open [sock (java.net.ServerSocket. 0)]
               (.getLocalPort sock))
        stop? (promise)]
    (server/run-server
      (fn [{:keys [uri]}]
        (cond
          (re-find #"^/static/.*\.js$" uri)
          , {:status 200
             :body (slurp (str static-path (str/replace uri #"^/static" "")))
             :headers {"Content-type" "application/javascript"}}
          (= "/" uri)
          , (do
              (future
                (Thread/sleep 1000)
                (deliver stop? true))
              {:status 200 :body html})))
      {:port port})
    (browse/browse-url (str "http://localhost:" port))
    @stop?))
