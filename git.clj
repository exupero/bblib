(ns git
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn run [dir cmd]
  (shell/with-sh-dir dir
    (let [{:keys [out err exit]} (apply shell/sh cmd)]
      (if (pos? exit)
        (throw (Exception. err))
        (str/trim out)))))

(defn root
  ([] (root nil))
  ([dir]
   (run dir ["git" "rev-parse" "--show-toplevel"])))

(defn commit
  ([] (commit nil))
  ([dir]
   (run dir ["git" "rev-parse" "HEAD"])))

(defn branch
  ([] (branch nil))
  ([dir]
   (run dir ["git" "rev-parse" "--abbrev-ref" "HEAD"])))

(defn origin
  ([] (origin nil))
  ([dir]
   (run dir ["git" "config" "--get" "remote.origin.url"])))

(defn path
  ([repo-path] (path nil repo-path))
  ([dir repo-path]
   (str (root dir) "/" repo-path)))

(defn full-path
  ([relative-path] (full-path nil relative-path))
  ([dir relative-path]
   (run dir ["git" "ls-files" "--full-name" relative-path])))
