(ns peg
  (:refer-clojure :exclude [not range sequence set some take])
  (:require [clojure.string :as str]))

; inspired by https://janet-lang.org/docs/peg.html
; state structure inspired by https://github.com/ericnormand/squarepeg
; :i = input remaining
; :r = return value
; :c = captures

; Constants

(defn success []
  (fn [input]
    [nil input]))

(defn fail []
  (constantly nil))

; Parsers

(defn literal [s]
  (fn [input]
    (when (str/starts-with? input s)
      {:i (subs input (count s))
       :r s})))

^:rct/test
(comment
  ((literal "s") "s") ;=> {:i "" :r "s"}
  nil)

(defn range [cs]
  (let [[low high] (map int cs)]
    (fn [input]
      (when (<= low (int (first input)) high)
        {:i (subs input 1)
         :r (str (first input))}))))

^:rct/test
(comment
  ((range "an") "k") ;=> {:i "" :r "k"}
  ((range "ae") "g") ;=> nil
  nil)

(defn regex [pattern]
  (fn [input]
    (when-let [r (re-find (re-pattern (str "^" pattern)) input)]
      {:i (subs input (count r))
       :r r})))

(defn set [cs]
  (fn [input]
    (when-let [c ((clojure.core/set cs) (first input))]
      {:i (subs input 1)
       :r (str c)})))

^:rct/test
(comment
  ((set "abc") "a") ;=> {:i "" :r "a"}
  ((set "abc") "b") ;=> {:i "" :r "b"}
  ((set "abc") "c") ;=> {:i "" :r "c"}
  ((set "abc") "d") ;=> nil
  nil)

(defn take [n]
  (fn [input]
    (cond
      (zero? n)
      , {:i input}
      (pos? n)
      , (when (<= n (count input))
          {:i (subs input n)
           :r (subs input 0 n)})
      (neg? n)
      , (when (< (count input) (Math/abs n))
          {:i input}))))

^:rct/test
(comment
  ((take 0) "abc") ;=> {:i "abc"}
  ((take 2) "abc") ;=> {:i "c" :r "ab"}
  ((take -1) "abc") ;=> nil
  ((take -1) "") ;=> {:i ""}
  nil)

^:rct/test
(comment
  ((regex #"\d+") "54g") ;=> {:i "g" :r "54"}
  nil)

; Combinators

(defn not [parser]
  (fn [input]
    (when-not (parser input)
      {:i input})))

^:rct/test
(comment
  ((not (literal "a")) "b") ;=> {:i "b"}
  ((not (literal "a")) "a") ;=> nil
  nil)

(defn between [low high parser]
  (fn [input]
    (loop [input input
           cnt 0
           result []]
      (if (< cnt high)
        (let [{input' :i :keys [r]} (parser input)]
          (if r
            (recur input' (inc cnt) (conj result r))
            (when (<= low cnt high)
              {:i input
               :r result})))
        {:i input
         :r result}))))

^:rct/test
(comment
  ((between 3 4 (literal "a")) "aa") ;=> nil
  ((between 3 4 (literal "a")) "aaa") ;=> {:i "" :r ["a" "a" "a"]}
  ((between 3 4 (literal "a")) "aaaa") ;=> {:i "" :r ["a" "a" "a" "a"]}
  ((between 3 4 (literal "a")) "aaaaa") ;=> {:i "a" :r ["a" "a" "a" "a"]}
  nil)

(defn at-least [n parser]
  (between n Long/MAX_VALUE parser))

(defn at-most [n parser]
  (between 0 n parser))

(defn opt [parser]
  (at-most 1 parser))

(defn any [parser]
  (at-least 0 parser))

(defn some [parser]
  (at-least 1 parser))

(defn repeat [n parser]
  (between n n parser))

(defn sequence [& parsers]
  (fn [input]
    (loop [parsers parsers
           o {:i input}]
      (if (empty? parsers)
        o
        (let [[parser & parsers] parsers]
          (when-let [o' (parser (o :i))]
            (recur parsers (assoc o' :c (merge (o :c) (o' :c))))))))))

^:rct/test
(comment
  ((sequence (literal "a") (literal "b")) "abc") ;=> {:i "c" :r "b"}
  nil)

(defn choice [& parsers]
  (fn [input]
    (clojure.core/some #(% input) parsers)))

^:rct/test
(comment
  ((choice (literal "a") (regex #"ba")) "bac") ;=> {:i "c" :r "ba"}
  nil)

(defn look
  ([offset]
   (look 0 (take offset)))
  ([offset parser]
   (fn [input]
     (when (parser (subs input offset))
       {:i input}))))

^:rct/test
(comment
  ((look 2) "a") ;=> nil
  ((look 2) "abc") ;=> {:i "abc"}
  ((look 2 (literal "cd")) "abcd") ;=> {:i "abcd"}
  ((look 2 (literal "ef")) "abcd") ;=> nil
  nil)

(defn given [pred parser]
  (fn [input]
    (when (pred input)
      (parser input))))

^:rct/test
(comment
  ((given (literal "a") (literal "abc")) "abcd") ;=> {:i "d" :r "abc"}
  ((given (literal "b") (literal "abc")) "abcd") ;=> nil
  nil)

(defn given-not [pred parser]
  (given (not pred) parser))

^:rct/test
(comment
  ((given-not (literal "a") (literal "abc")) "abcd") ;=> nil
  ((given-not (literal "b") (literal "abc")) "abcd") ;=> {:i "d" :r "abc"}
  nil)

(defn to [parser]
  (fn [input]
    (cond
      (nil? (first input)) nil
      (parser input) {:i input}
      :else (recur (subs input 1)))))

^:rct/test
(comment
  ((to (literal "d")) "abcd") ;=> {:i "d"}
  ((to (literal "p")) "abcd") ;=> nil
  nil)

(defn thru [parser]
  (fn [input]
    (when-not (nil? (first input))
      (if-let [{input' :i} (parser input)]
        {:i input'}
        (recur (subs input 1))))))

^:rct/test
(comment
  ((thru (literal "d")) "abcde") ;=> {:i "e"}
  ((thru (literal "p")) "abcde") ;=> nil
  nil)

; TODO `sub`

; Captures

(defn capture [parser tag]
  (fn [input]
    (let [{:keys [r] :as o} (parser input)]
      (assoc-in o [:c tag] r))))

^:rct/test
(comment
  ((capture (literal "a") :x) "abc") ;=> {:i "bc" :r "a" :c {:x "a"}}
  ((sequence (literal "a") (capture (literal "b") :x) (literal "c")) "abc") ;=> {:i "" :r "c" :c {:x "b"}}
  nil)

; Matcher

(defn match [parser input]
  (let [{:keys [c r]} (parser input)]
    (or c r)))

^:rct/test
(comment
  (match (literal "s") "s") ;=> "s"
  nil)
