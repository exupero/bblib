(ns text
  (:require [clojure.java.shell :as shell]
            clojure.pprint
            [clojure.string :as str]
            [babashka.deps :as deps]
            [cheshire.core :as json]))
(deps/add-deps '{:deps {mvxcvi/puget {:mvn/version "1.3.4"}}})
(require '[puget.printer :as puget])

(defn table [rows]
  (-> (shell/sh "ramda" "-o" "table" "--compact" :in (json/generate-string rows))
      :out))

(defn print-table [rows]
  (println (table rows)))

(defn tty? []
  (-> (shell/sh "test" "-t" "1") :exit zero?))

(defn pprint [o]
  (if (tty?)
    (puget/cprint o)
    (clojure.pprint/pprint o)))

; copied from Loom https://github.com/aysylu/loom/blob/master/src/loom/io.clj
(defn spit-bytes [file ^bytes data]
  (with-open [w (java.io.FileOutputStream. file)]
    (.write w ^bytes data)))

(defn print-lines [lines]
  (doseq [line lines]
    (println line)))

(defn indent [ind s]
  (->> s
       str/split-lines
       (map #(str ind %))
       (str/join "\n")))

; from https://stackoverflow.com/a/20747249
(defn truncate [s n]
  (when (and s (not (neg? n)))
    (subs s 0 (min (count s) n))))

(defn update-file [path f & args]
  (let [content (slurp path)]
    (some->> (apply f content args)
             (spit path))))

(defn url-encode [s]
  (-> s
    (java.net.URLEncoder/encode "UTF-8")
    (str/replace "+" "%20")))

(defn format-query-params [kvs]
  (transduce
    (comp
      (map (fn [[k v]]
             (if (true? v)
               (name k)
               (str (name k) "=" (url-encode (name v))))))
      (interpose "&"))
    str kvs))
