(ns obsidian
  (:require [babashka.process :as p]
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
