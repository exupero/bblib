(ns config
  (:refer-clojure :exclude [read])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            clojure.pprint
            [clojure.string :as str]))

(def roots (map io/file (str/split (System/getenv "CLJ_CONFIGS_PATH") #":")))

(defn edn-name [nm]
  (str (name nm) ".edn"))

(defn find-file [nm]
  (let [nm (name nm)]
    (some (fn [root]
            (let [f (io/file root nm)]
              (when (.exists f) f)))
          roots)))

(defn content [nm]
  (some-> (name nm) find-file slurp))

(defn path [nm]
  (edn-name (str (first roots) "/" (name nm))))

(defn read [nm]
  (some-> (edn-name nm) find-file slurp read-string))

(defn read-as [nm fmt]
  (some->> (edn-name nm)
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

(defn write-map! [nm m]
  (spit (find-file (edn-name nm))
        (with-out-str
          (doseq [[k v] m]
            (pr-str k v)))))

(defn update-map! [nm f & args]
  (let [file (find-file (edn-name nm))
        m (edn/read-string {:default tagged-literal}
                           (format "{%s}" (slurp file)))
        m (apply f m args)]
    (spit file (with-out-str
                 (doseq [[k v] m]
                   (prn k v))))))
