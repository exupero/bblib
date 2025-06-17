(ns tree-sitter
  (:require clojure.data.xml
            [clojure.string :as str]
            clojure.walk
            [babashka.process :as p]
            util
            xforms)
  (:import [java.io File]))

(defn parse [extension code]
  (let [tempfile (File/createTempFile "tree-sitter-" (str \. (name extension)))]
    (spit tempfile code)
    (->> (p/shell {:out :string :err :string} "tree-sitter parse" (.getAbsolutePath tempfile) "--xml")
         :out
         clojure.data.xml/parse-str
         (clojure.walk/postwalk
           (fn [n]
             (cond
               (string? n)
               , (str/trim n)
               (and (map? n) (n :content))
               , (update n :content (partial remove #{""}))
               :else
               , n))))))

(defn parse-results [s]
  (sequence
    (comp
      (xforms/split-by (partial re-find #"^[^ ]"))
      (filter second) ; don't output files that don't have matches
      (mapcat (fn [[path & lines]]
                (sequence
                  (comp
                    (xforms/split-by (partial re-find #"pattern:"))
                    (map (fn [[pattern & captures]]
                           {:path path
                            :pattern (some->> pattern (re-find #"\d+") Integer/parseInt)
                            :captures (map (fn [capture]
                                             (let [[_ idx nm start-row start-col _ _ text]
                                                   , (re-find #"capture: (?:(\d+) - )?([^ ]+), start: \((\d+), (\d+)\), end: \((\d+), (\d+)\)(?:, text: `(.*)`)?" capture)]
                                               {:capture (some-> idx Integer/parseInt)
                                                :name nm
                                                :line (some-> start-row Integer/parseInt inc)
                                                :column (some-> start-col Integer/parseInt inc)
                                                :text text}))
                                           captures)})))
                  lines))))
    (str/split-lines s)))

(defn query [query paths]
  (let [tempfile (File/createTempFile "tree-sitter-query-" ".scm")
        _ (spit tempfile query)]
    (-> (apply p/shell {:out :string :err :string} "tree-sitter query" (.getAbsolutePath tempfile) paths)
        :out
        parse-results)))

(defn unparse [tree]
  (->> tree
       (tree-seq (comp seq :content) :content)
       (filter string?)
       (apply str)))
