(ns cli
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            babashka.cli))

(def types
  {(type "") :string
   (type 0) :long
   (type []) []})

(defn long? [n]
  (= (type 0) (type n)))

(defn opt-name? [s]
  (and (symbol? s)
       (= \- (first (name s)))))

(defn opt-name->keyword [s]
  (-> s name (str/replace #"^--?" "") keyword))

(defn opt->spec [opt]
  (let [[als nm tail] (match opt
                        [(als :guard opt-name?) (nm :guard opt-name?) & tail]
                        , [(opt-name->keyword als) (opt-name->keyword nm) tail]
                        [(nm :guard opt-name?) & tail]
                        , [nil (opt-name->keyword nm) tail])]
    {nm (merge (cond-> {} als (assoc :alias als))
               (match (vec tail)
                 [(coerce :guard keyword?) (desc :guard string?)]
                 , {:desc desc
                    :coerce coerce}
                 [default (desc :guard string?)]
                 , {:desc desc
                    :default default
                    :coerce (types (type default))}
                 [(desc :guard string?)]
                 , {:desc desc
                    :coerce :boolean}
                 [default]
                 , {:default default
                    :coerce (types (type default))}
                 []
                 , {:coerce :boolean}
                 :else
                 , (throw (Exception. (str "Unknown CLI spec: " (pr-str opt))))))}))

(comment
  (for [opt '[[-d --dry-run]
              [-p --port 5000]
              [-p --port 5000 "HTTP port to serve on"]
              [--paths [cat]]]]
    (opt->spec opt)))

(defn opts->spec [opts]
  (apply merge (map opt->spec opts)))

(defmacro bind-opts [[opts args] & body]
  (let [spec (or (opts->spec opts) {})
        [args->opts vararg]
        , (if (= '& (second (reverse args)))
            [`(concat
                ~(into []
                       (comp
                         (take-while (complement '#{&}))
                         (map keyword))
                       args)
                (repeat 100 ~(keyword (peek args))))
             (keyword (peek args))]
            [(mapv keyword args) nil])
        nms (->> spec keys
                 (map symbol)
                 (concat (sequence
                           (comp
                             (distinct)
                             (remove '#{&})
                             (map symbol))
                           args)))
        opts (cond-> {:spec spec
                      :args->opts args->opts}
               vararg (assoc :coerce {vararg []}))]
    `(let [{:keys [~@nms]} (babashka.cli/parse-opts *command-line-args* ~opts)]
       ~@body)))

(defmacro with-opts [[opts args] & body]
  `(when (= *file* (System/getProperty "babashka.file"))
     (bind-opts [~opts ~args] ~@body)))

(comment
  (macroexpand-1 '(bind-opts [[[-d --dry-run]
                               [-p --preview]]
                              [url urls urls]]
                    (println dry-run preview url urls)))
  (macroexpand-1 '(bind-opts [[[-d --dry-run]
                               [-p --preview]]
                              [& urls]]
                    (println dry-run preview url urls)))
  (macroexpand-1 '(bind-opts [[]
                              [& urls]]
                    (println urls)))

  (binding [*command-line-args* ["-d" "--no-preview" "yes" "no"]]
    (bind-opts [[[-d --dry-run]
                 [-p --preview]
                 [-P --no-preview]]
                [url & urls]]
      (println dry-run preview no-preview url urls)))

  (binding [*command-line-args* ["a" "b"]]
    (let [opts (babashka.cli/parse-opts *command-line-args* {:spec {}, :coerce {:urls []} :args->opts [:urls :urls]})]
      (prn opts)))

  (binding [*command-line-args* ["a" "b"]]
    (bind-opts [[]
                [& urls]]
      (println urls)))

  (macroexpand-1 '(bind-opts [[[-i --input :string ""]]
                              []]
                    input)))
