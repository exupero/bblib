(ns config
  (:refer-clojure :exclude [read])
  (:require [clojure.edn :as edn]
            clojure.pprint))

(def root (str (System/getenv "CLJ_CONFIGS_PATH") "/"))

(defn content [nm]
  (slurp (str root (name nm))))

(defn path [nm]
  (str root (name nm) ".edn"))

(defn read [nm]
  (-> nm path slurp read-string))

(defn read-as [nm fmt]
  (->> nm path slurp
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
