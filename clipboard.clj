(ns clipboard
  (:refer-clojure :exclude [slurp spit])
  (:require [clojure.java.shell :as shell]))

(defn slurp []
  (:out (shell/sh "pbpaste")))

(defn spit [s]
  (shell/sh "pbcopy" :in s)
  nil)
