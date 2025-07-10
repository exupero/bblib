(ns markdown
  (:require [clojure.string :as str]
            [clj-yaml.core :as yaml]
            text))

(defn frontmatter [s]
  (when (= "---" (subs s 0 3))
    (yaml/parse-string
      (transduce
        (comp
          (drop 1)
          (take-while (complement #{"---"}))
          (interpose "\n"))
        str (str/split-lines s)))))

(defn frontmatter+content [s]
  (let [[head & tail] (-> s str/trim str/split-lines)]
    (if (= head "---")
      (let [[front back] (split-with (complement #{"---"}) tail)]
        [(yaml/parse-string (str/join "\n" front))
         (str/join "\n" (drop 1 back))])
      [{} (str/join "\n" tail)])))

(defn remove-frontmatter [s]
  (if (= "---" (subs s 0 3))
    (transduce
      (comp
        (drop 1)
        (drop-while (complement #{"---"}))
        (drop 1)
        (interpose "\n"))
      str (str/split-lines s))
    s))

(defn update-frontmatter [file f & args]
  (let [[fm content] (frontmatter+content (slurp file))]
    (spit file
          (str "---\n"
               (yaml/generate-string (apply f fm args) :dumper-options {:flow-style :block})
               "---\n"
               content))))
