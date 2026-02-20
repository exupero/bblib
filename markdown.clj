(ns markdown
  (:require [clojure.string :as str]
            [clj-yaml.core :as yaml]
            text))

(defn frontmatter-text [s]
  (when (= "---" (subs s 0 3))
    (transduce
      (comp
        (drop 1)
        (take-while (complement #{"---"}))
        (interpose "\n"))
      str (str/split-lines s))))

(defn frontmatter [s]
  (when (and (pos? (count s))
             (= "---" (subs s 0 3)))
    (yaml/parse-string (frontmatter-text s))))

(defn frontmatter-text+content [s]
  (let [[head & tail] (-> s str/trim str/split-lines)]
    (if (= head "---")
      (let [[front back] (split-with (complement #{"---"}) tail)]
        [(str/join "\n" front)
         (str/join "\n" (drop 1 back))])
      ["" (str/join "\n" (cons head tail))])))

(defn frontmatter+content [s]
  (let [[head & tail] (-> s str/trim str/split-lines)]
    (if (= head "---")
      (let [[fm-text content] (frontmatter-text+content s)]
        [(yaml/parse-string fm-text) content])
      [{} (str/join "\n" (cons head tail))])))

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

(defn content [file]
  (remove-frontmatter (slurp file)))

(defn update-frontmatter-text [file f & args]
  (text/update-file file
    (fn [content]
      (let [[fm-text content] (frontmatter-text+content content)]
        (str "---\n"
             (apply f fm-text args)
             "\n---\n"
             content)))))

(defn update-frontmatter [file f & args]
  (text/update-file file
    (fn [content]
      (let [[fm content] (frontmatter+content content)]
        (str "---\n"
             (yaml/generate-string (apply f fm args) :dumper-options {:flow-style :block})
             "---\n"
             content)))))
