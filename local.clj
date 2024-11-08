(ns local
  (:require [clojure.java.io :as io]))

(defn home [& path-parts]
  (io/file (apply str (System/getenv "HOME") "/" path-parts)))
