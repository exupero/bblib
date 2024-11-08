(ns python
  {:clj-kondo/ignore [:unresolved-symbol]}
  (:require [clojure.string :as str]
            [babashka.deps :as deps]
            [babashka.process :as p]
            [cheshire.core :as json]))
(deps/add-deps '{:deps {meander/epsilon {:mvn/version "0.0.650"}}})
(require '[meander.epsilon :as m]
         '[meander.strategy.epsilon :as m*])

(defn dot-dash? [sym]
  (and (symbol? sym)
       (str/starts-with? (name sym) ".-")))

(defn un-dot-dash [sym]
  (-> sym name (subs 2) symbol))

(defn dot? [sym]
  (and (symbol? sym)
       (str/starts-with? (name sym) ".")))

(defn un-dot [sym]
  (-> sym name (subs 1) symbol))

(def special
  (m*/rewrite
    ('.- ?o ?a)
    , (py/attr (m/app special ?o) ?a)
    ((m/and ?a (m/pred dot-dash?)) ?o)
    , (py/attr ?o (m/app un-dot-dash ?a))
    ('. ?o ?m . !args ...)
    , (m/app special ((py/attr ?o ?m) . !args ...))
    ((m/and ?m (m/pred dot?)) ?o . !args ...)
    , (m/app special ((py/attr ?o (m/app un-dot ?m)) . !args ...))
    ('.. ?o (?m . !args ...))
    , (m/app special ((py/attr ?o ?m) . !args ...))
    ('.. ?o (?m . !args ...) . !more ...)
    , (m/app special ('.. (py/invoke (py/attr ?o ?m) [!args ...]) . !more ...))
    (?f . !args ... . (m/and ?kwargs (m/pred (comp :** meta))))
    , (py/invoke ?f [!args ...] ?kwargs)
    (?f . !args ...)
    , (py/invoke ?f [!args ...] {})
    ?else
    , ?else))

(comment
  (special '(.- a b))
  (special '(.- (.- a b) c))
  (special '(. a b c d))
  (special '(.. a (b c d)))
  (special '(.. a (b c d) (e f g)))
  (special '(.. a (b c d) (e f g) (h i j)))
  (special '(f a ^:** {:b 1}))
  (special '(f a))
  (special '(. a b c ^:** {:d e}))
  (special '(.-a b))
  (special '(.a b c d ^:** {:e f}))
  nil)

(def ctx-load {:type :Load})

(defn constant [v]
  {:type :Constant :value v})

(def ast
  (m*/match
    nil
    , (constant nil)
    true
    , (constant true)
    false
    , (constant false)
    (m/and ?s (m/pred string?))
    , (constant ?s)
    (m/and ?n (m/pred number?))
    , (constant ?n)
    (m/and ?k (m/pred keyword?))
    , (constant (name ?k))
    (m/and ?s (m/pred symbol?))
    , {:type :Name :id (name ?s) :ctx ctx-load}
    (py/list ?vs)
    , {:type :List :elts (map ast ?vs) :ctx ctx-load}
    (py/dict ?kvs)
    , {:type :Dict :keys (map ast (keys ?kvs)) :values (map ast (vals ?kvs))}
    (py/attr ?obj ?attr)
    , {:type :Attribute :value (ast ?obj) :attr (name ?attr) :ctx ctx-load}
    (py/invoke ?f ?args)
    , {:type :Call :func (ast ?f) :args (map ast ?args) :keywords []}
    (py/invoke ?f ?args ?kwargs)
    , {:type :Call
       :func (ast ?f)
       :args (map ast ?args)
       :keywords (map (fn [[k v]]
                        {:type :keyword :arg (name k) :value (ast v)})
                      ?kwargs)}))

(comment
  (ast '(py/attr (py/list [1]) b))
  (ast '(py/list [1 2 3]))
  (ast '(py/dict {:a 1 :b 2}))
  (ast '(py/invoke a [b]))
  (ast '(py/invoke a [b c]))
  (ast '(py/invoke a [b c (py/dict {:d e})]))
  (ast '(py/invoke a [b c] {:d e}))
  nil)

(defn clj->py-ast [form]
  (-> form
      special
      ast))

(comment
  (clj->py-ast '(.- (.- a b) c))
  (clj->py-ast '(. a b c d ^:** {:e 1}))
  (clj->py-ast '(.. a (b c d ^:** {:e 1})))
  nil)

(defn clj->python [form]
  (let [{:keys [err exit out]}
        , (p/shell {:in (json/generate-string (clj->py-ast form))
                    :out :string
                    :err :string}
                   "ast2py")]
    (if (pos? exit)
      (throw (Exception. err))
      out)))

(comment
  (clj->python '(.- a b))
  (clj->python '(.- (.- a b) c))
  (clj->python '(. a b c d))
  (clj->python '(.. a (b c d) (e f g) (h i j)))
  (clj->python '(. a b c d ^:** {:e f}))
  (clj->python '(.-a b))
  (clj->python '(.a b c d ^:** {:e f}))
  nil)
