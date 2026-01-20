(ns local
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn file [root & path-parts]
  (io/file (apply str root "/" (str/join "/" path-parts))))

(defn find-repo-root []
  ; Faster than shelling out to `git rev-parse --show-toplevel`?
  (loop [dir (System/getProperty "user.dir")]
    (if (.exists (io/file (str dir "/.git")))
      (io/file dir)
      (recur (str dir "/..")))))

(defn repo [& path-parts]
  (apply file (find-repo-root) path-parts))

(defn home [& path-parts]
  (apply file (System/getenv "HOME") path-parts))

(defn code [& path-parts]
  (apply file (System/getenv "CODE") path-parts))

(defn dotfiles [& path-parts]
  (apply code "dotfiles" path-parts))
