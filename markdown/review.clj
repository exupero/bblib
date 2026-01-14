(ns markdown.review
  (:require [clojure.string :as str]
            [babashka.deps :as deps]
            [babashka.process :as p]
            [cheshire.core :as json]
            github
            xforms))
(deps/add-deps '{:deps {net.cgrand/xforms {:mvn/version "0.19.6"}}})
(require '[net.cgrand.xforms :as xf])

(defn join-lines [lines]
  (str/trim (str/join "\n" lines)))

(defn parse-code-comment [lines]
  (let [[[header & code] [_ & cmt]]
        , (split-with (complement #{"```"}) lines)
        [_ path start-line] (re-find #"^```diff ([^ ]+) -\d+ \+(\d+)" header)
        start-line (parse-long start-line)
        right-side-lines (filter (partial re-find #"^$|^[^-]") code)
        end-line (+ start-line (dec (count right-side-lines)))]
    (cond-> {:body (join-lines cmt)
             :path path
             :side :RIGHT}
      (= start-line end-line)    (assoc :line start-line)
      (not= start-line end-line) (assoc :line end-line :start_line start-line))))

(defn parse-section-lines [lines]
  (if (re-find #"^```diff " (first lines))
    [:code (parse-code-comment lines)]
    [:main (join-lines lines)]))

(defn parse-review [content]
  (into {}
        (comp
          (xforms/split-by #(re-find #"^---" %))
          (map (fn [section]
                 (->> section
                      (drop-while #{"---" ""})
                      parse-section-lines)))
          (xf/by-key (xf/into [])))
        (str/split-lines content)))

(defn replace-mentions [s alias->username]
  (str/replace s #"@([A-Za-z0-9_]+)"
               (fn [[_ nm]]
                 (str "@" (alias->username nm nm)))))

(defn fmt [{:keys [main code]} alias->username]
  {:body (replace-mentions (str/join "\n\n" main) alias->username)
   :comments (map #(update % :body replace-mentions alias->username) code)})

(defn fetch-pr-info []
  (let [{{owner :login} :headRepositoryOwner {repo :name} :headRepository :keys [number]}
        , (-> @(p/process {:out :string} "gh pr view --json headRepositoryOwner,headRepository,number")
              :out
              (json/parse-string true))]
    {:owner owner
     :repo repo
     :number number}))

(defn new-review [owner repo number content alias->username]
  (let [review (fmt (parse-review content) alias->username)]
    (github/new-review owner repo number review)))
