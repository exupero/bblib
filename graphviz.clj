(ns graphviz
  (:require [clojure.java.shell :as shell]))

(defn append-attrs [sb attrs]
  (doseq [[k v] attrs
          :when (and (not= k :id) v)]
    (doto sb
      (.append " ")
      (.append (name k))
      (.append "=")
      (.append (if (keyword? v)
                 (name v)
                 (pr-str v))))))

(defn graph [nodes edges]
  (let [sb (StringBuilder.)]
    (doto sb
      (.append "digraph {\n")
      (.append "  graph [dpi=300]\n")
      (.append "  rankdir=\"LR\"\n")
      (.append "  node [shape=box style=filled fillcolor=white]\n"))
    (doseq [{:keys [id attrs]} nodes
            :when id]
      (doto sb
        (.append "  ")
        (.append (pr-str id))
        (.append " [")
        (append-attrs attrs)
        (.append "]\n")))
    (doseq [[from to attrs] edges
            :when (and from to)]
      (doto sb
        (.append "  ")
        (.append (pr-str from))
        (.append " -> ")
        (.append (pr-str to))
        (.append " [")
        (append-attrs attrs)
        (.append "]\n")))
    (doto sb
      (.append "}"))
    (str sb)))

(defn render-as-bytes [graph fmt]
  (let [{:keys [exit out err]}
        , (shell/sh "dot" (str "-T" (name fmt))
                    :in graph
                    :out-enc :bytes)]
    (if (pos? exit)
      (throw (Exception. err))
      out)))

; from Loom https://github.com/aysylu/loom/blob/master/src/loom/io.clj
(defn spit-bytes [file ^bytes data]
  (with-open [w (java.io.FileOutputStream. file)]
    (.write w ^bytes data)))

; from Loom https://github.com/aysylu/loom/blob/master/src/loom/io.clj
(defn open-data [data ext]
  (let [ext (name ext)
        ext (if (= \. (first ext)) ext (str \. ext))
        tmp (java.io.File/createTempFile (subs ext 1) ext)]
    (if (string? data)
      (with-open [w (java.io.FileWriter. tmp)]
        (.write w ^String data))
      (spit-bytes tmp data))
    (.deleteOnExit tmp)
    (shell/sh "open" (str tmp))))

(defn open [graph fmt]
  (open-data (render-as-bytes graph fmt) (name fmt)))
