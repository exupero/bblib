(ns diff
  (:require [clojure.string :as str]
            xforms))

(defn files [diff]
  (sequence
    (comp
      (xforms/split-by (partial re-find #"^diff"))
      (map (partial str/join "\n")))
    (str/split-lines diff)))

(defn parse-file [diff]
  (let [[header index a b & lines] (str/split-lines diff)]
    {:header [header index a b]
     :chunks (sequence
               (comp
                 (xforms/split-by (partial re-find #"^@@"))
                 (map (fn [[header & lines]]
                        {:header header
                         :lines lines})))
               lines)}))

(defn unparse-file [{:keys [header chunks]}]
  (str (str/join "\n" header) "\n"
       (str/join "\n" (map (fn [{:keys [header lines]}]
                             (str header "\n" (str/join "\n" lines)))
                           chunks))))

(defn original [lines]
  (->> lines
       (filter #(re-find #"^[ -]" %))
       (map #(subs % 1))))

(defn revised [lines]
  (->> lines
       (filter #(re-find #"^[ +]" %))
       (map #(subs % 1))))
