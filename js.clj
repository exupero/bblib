(ns js
  (:require [babashka.process :as p]
            cheshire.core))

(def pretty-printer
  (cheshire.core/create-pretty-printer
    (assoc cheshire.core/default-pretty-print-options
           :indent-arrays? true
           :indentation "  "
           :object-field-value-separator ": ")))

(defn generate-formatted-string [o]
  (cheshire.core/generate-string o {:pretty pretty-printer}))

(defn generate-pretty-string [o]
  (-> (p/shell {:in (cheshire.core/generate-string o)
                :out :string}
               "jq -C")
      :out))

(defn tty? []
  (-> @(p/process {:inherit :out :env {}} "test -t 1") :exit zero?))

(defn pprint [o]
  (let [f (if (tty?)
            generate-pretty-string
            generate-formatted-string)]
    (println (f o))))
