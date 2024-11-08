(ns local
  (:require [clojure.java.io :as io]))

(defn home [& path-parts]
  (apply io/file (System/getenv "HOME") path-parts))
