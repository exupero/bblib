(ns local
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn file [root & path-parts]
  (io/file (apply str root "/" (str/join "/" path-parts))))

(defn home [& path-parts]
  (apply file (System/getenv "HOME") path-parts))

(defn code [& path-parts]
  (apply file (System/getenv "CODE_PATH") path-parts))
