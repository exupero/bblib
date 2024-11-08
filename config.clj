(ns config
  (:refer-clojure :exclude [read])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            clojure.pprint
            [clojure.string :as str]))

(def roots (map io/file (str/split (System/getenv "CLJ_CONFIGS_PATH") #":")))

(defn find-file [nm]
  (let [nm (name nm)]
    (some (fn [root]
            (let [f (io/file root nm)]
              (when (.exists f) f)))
          roots)))

(defn content [nm]
  (some-> (name nm) find-file  slurp))

(defn path [nm]
  (str (first roots) "/" (name nm) ".edn"))

(defn read [nm]
  (some-> (str (name nm) ".edn") find-file slurp read-string))

(defn read-as [nm fmt]
  (some->> (str (name nm) ".edn")
           find-file
           slurp
           (format fmt)
           (edn/read-string {:default tagged-literal})))

(defn read-list [nm]
  (read-as nm "[%s]"))

(defn read-map [nm]
  (read-as nm "{%s}"))

(defn write! [nm value]
  (->> value
       clojure.pprint/pprint
       with-out-str
       (spit (path nm))))
