(ns local
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn home [& path-parts]
  (io/file (apply str (System/getenv "HOME") "/" (str/join "/" path-parts))))
