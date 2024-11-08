(ns pull
  (:require clojure.test
            [babashka.deps :as deps]))
(deps/add-deps '{:deps {io.github.matthewdowney/rich-comment-tests {:mvn/version "v1.0.3"}}})
(require '[com.mjdowney.rich-comment-tests :as rct])

(defn sub [field]
  (cond
    (and (keyword? field)
         (not (namespace field)))
    , {:key field}
    (map? field)
    , (let [[k vs] (first field)]
        (if (vector? k)
          {:fetch k :sub (map sub vs)}
          {:key k :sub (map sub vs)}))
    :else
    , nil))

(defn spec [q]
  (let [[query fields] (first q)]
    (cond-> {:fetch query}
      (and (seq fields)
           (not= '[*] fields))
      , (assoc :sub (map sub fields)))))

^:rct/test
(comment
  (spec {[:a ""] []})
  ; =>
  {:fetch [:a ""]}

  (spec {[:a ""] ['*]})
  ; =>
  {:fetch [:a ""]}

  (spec {[:a ""] [:name]})
  ; =>
  {:fetch [:a ""]
   :sub [{:key :name}]}

  (spec {[:a ""] [:name {:friends [:name]}]})
  ; =>
  {:fetch [:a ""]
   :sub [{:key :name}
         {:key :friends
          :sub [{:key :name}]}]}

  (spec {[:a ""] [{[:friends] [:name]}]})
  ; =>
  {:fetch [:a ""]
   :sub [{:fetch [:friends]
          :sub [{:key :name}]}]}

  (spec {[:a ""] [{[:friends] [:name {[:status] [:mood]}]}]})
  ; =>
  {:fetch [:a ""]
   :sub [{:fetch [:friends]
          :sub [{:key :name}
                {:fetch [:status]
                 :sub [{:key :mood}]}]}]})

(defn get-subs [o sub f]
  (cond
    (map? o)
    , (into {}
            (comp
              (map (fn [s]
                     (let [{k :key :keys [fetch sub]} s]
                       (cond
                         k
                         , [k (get o k)]
                         fetch
                         , (let [[k & args] fetch
                                 o (apply f k o args)]
                             [(first fetch) (get-subs o sub f)]))))))
            sub)
    (sequential? o)
    , (map #(get-subs % sub f) o)
    :else
    , (throw (Exception. (str "don't know how to get subs for " (pr-str o))))))

(defn pull [q f]
  (let [{:keys [fetch sub]} (spec q)]
    (get-subs (apply f fetch) sub f)))

^:rct/test
(comment
  (pull {[:person "alice"] [:name {[:friends] [:name {[:status] [:mood]}]}]}
        (fn [& args]
          (condp = args
            [:person "alice"]
            , {:id "alice" :name "Alice"}
            [:friends {:id "alice" :name "Alice"}]
            , [{:id "bob" :name "Bob"}
               {:id "carl" :name "Carl"}]
            [:status {:id "bob" :name "Bob"}]
            , {:mood :dour}
            [:status {:id "carl" :name "Carl"}]
            , {:mood :happy})))
  ; =>
  {:name "Alice"
   :friends [{:name "Bob" :status {:mood :dour}}
             {:name "Carl" :status {:mood :happy}}]})

(comment
  (binding [clojure.test/*test-out* *out*]
    (rct/run-ns-tests! *ns*)))
