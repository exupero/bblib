(ns re
  (:require [clojure.string :as str]))

(defn re-map [re f s]
  (if (re-matches re s)
    [(f s)]
    (remove #{"" ::padding}
      (interleave
        (str/split s re)
        (concat (map f (re-seq re s)) [::padding])))))
