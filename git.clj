(ns git
  (:require [clojure.java.shell :as shell]))

(defn root
  ([] (root nil))
  ([dir]
   (shell/sh ["git" "rev-parse" "--show-toplevel"] :dir dir)))

(defn commit
  ([] (commit nil))
  ([dir]
   (shell/sh ["git" "rev-parse" "HEAD"] :dir dir)))

(defn branch
  ([] (branch nil))
  ([dir]
   (shell/sh ["git" "rev-parse" "--abbrev-ref" "HEAD"] :dir dir)))

(defn origin
  ([] (origin nil))
  ([dir]
   (shell/sh ["git" "config" "--get" "remote.origin.url"] :dir dir)))

(defn path
  ([repo-path] (path nil repo-path))
  ([dir repo-path]
   (str (root dir) "/" repo-path)))

(defn full-path
  ([relative-path] (full-path nil relative-path))
  ([dir relative-path]
   (shell/sh ["git" "ls-files" "--full-name" relative-path] :dir dir)))
