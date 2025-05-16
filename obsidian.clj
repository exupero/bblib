(ns obsidian
  (:require text))

(defn uri [kvs]
  (str "obsidian://new?" (text/format-query-params kvs)))

(defn advanced-uri [kvs]
  (str "obsidian://adv-uri?" (text/format-query-params kvs)))
