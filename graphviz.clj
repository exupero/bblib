(ns graphviz
  (:require [clojure.java.shell :as shell]))

(defn append [sb & ss]
  (doseq [s ss]
    (.append sb s)))

(defn append-attrs [sb attrs]
  (doseq [[k v] attrs
          :when (and (not= k :id) v)]
    (append sb
            " " (name k) "="
            (if (keyword? v)
              (name v)
              (pr-str v)))))

(defmulti node (fn [_ {t :type}] t))

(defmethod node :default [sb {:keys [id attrs]}]
  (doto sb
    (append "  " (pr-str id) " [")
    (append-attrs attrs)
    (append "]\n")))

(defmethod node :group [sb {:keys [rank nodes]}]
  (append sb "  {\n")
  (when rank
    (append sb "  rank=" (name rank) "\n"))
  (doseq [n nodes]
    (node sb n))
  (append sb "\n  }"))

(defn graph [{:keys [attrs nodes edges]}]
  (let [sb (StringBuilder.)]
    (append sb "digraph {\n  graph [dpi=300]\n")
    (doseq [[k v] attrs]
      (doto sb
        (append "  " (name k) " [")
        (append-attrs v)
        (append "]\n")))
    (doseq [n nodes]
      (node sb n))
    (doseq [[from to attrs] edges
            :when (and from to)]
      (doto sb
        (append "  " (pr-str from) " -> " (pr-str to) " [")
        (append-attrs attrs)
        (append "]\n")))
    (append sb "}")
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
