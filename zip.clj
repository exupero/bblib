(ns zip
  (:refer-clojure :exclude [find iterate take-while drop-while])
  (:require [clojure.zip :as z]))

(defn inserts-left [loc inserts]
  (reduce z/insert-left loc inserts))

(defn go [loc direction pred]
  (loop [loc loc]
    (if (and loc (not (z/end? loc)))
      (if (pred (z/node loc))
        loc
        (recur (direction loc)))
      loc)))

(defn go-find [loc direction & clauses]
  (loop [loc loc]
    (if (and loc (not (z/end? loc)))
      (let [f (some (fn [[pred f]]
                      (when (pred (z/node loc)) f))
                    (partition 2 clauses))]
        (if f
          (recur (direction (f loc)))
          (recur (direction loc))))
      loc)))

(defn iterate [loc direction]
  (->> loc
       (clojure.core/iterate direction)
       (clojure.core/take-while (complement z/end?))))

(defn find [loc direction pred]
  (-> loc
      (iterate direction)
      (->> (some #(when (pred %) %)))))

(defn take-while [loc direction pred]
  (when (and (not (z/end? loc)) (pred (z/node loc)))
    (lazy-seq
      (cons loc (take-while (direction loc) direction pred)))))

(defn drop-while [loc direction pred]
  (if (and (not (z/end? loc)) (pred (z/node loc)))
    (drop-while (direction (z/remove loc)) direction pred)
    loc))
