(ns obsidian
  (:require [clojure.string :as str]
            [babashka.process :as p]
            text))

(defn open! [uri]
  (p/shell "open" uri))

(defn uri [kvs]
  (str "obsidian://new?" (text/format-query-params kvs)))

(defn uri! [kvs]
  (open! (uri kvs)))

(defn advanced-uri [kvs]
  (str "obsidian://adv-uri?" (text/format-query-params kvs)))

(defn advanced-uri! [kvs]
  (open! (advanced-uri kvs)))

(defn open-tab [vault path]
  (advanced-uri! {:vault vault :filepath path :openmode "true"}))

(defn sanitize [s]
  (-> s
      (str/replace #"[#^\[\]|:/\\.]+" "-")
      str/trim))

(defn create-and-open! [absolute-path vault vault-path content]
  (spit absolute-path content)
  (Thread/sleep 500) ; wait for file to be written
  (open-tab vault vault-path))
