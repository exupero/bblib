(ns peg
  (:refer-clojure :exclude [not range replace sequence set some take])
  (:require [clojure.string :as str]))

; inspired by https://janet-lang.org/docs/peg.html
; state structure inspired by https://github.com/ericnormand/squarepeg
; :i = input remaining
; :r = return value
; :c = captures
; :p = position in input

; Constants

(defn success []
  (fn [input _ position]
    {:i input :p position}))

(defn fail []
  (constantly nil))

; Parsers

(defn literal [s]
  (fn [input _ position]
    (when (str/starts-with? input s)
      (let [c (count s)]
        {:i (subs input c)
         :r s
         :p (+ position c)}))))

^:rct/test
(comment
  ((literal "s") "s" nil 0) ;=> {:i "" :r "s" :p 1}
  nil)

(defn range [cs]
  (let [[low high] (map int cs)]
    (fn [input _ _]
      (when (<= low (int (first input)) high)
        {:i (subs input 1)
         :r (str (first input))
         :p 1}))))

^:rct/test
(comment
  ((range "an") "k" nil 0) ;=> {:i "" :r "k" :p 1}
  ((range "ae") "g" nil 0) ;=> nil
  nil)

(defn regex [pattern]
  (fn [input _ _]
    (when-let [r (re-find (re-pattern (str "^" pattern)) input)]
      (let [c (count r)]
        {:i (subs input c)
         :r r
         :p c}))))

^:rct/test
(comment
  ((regex #"\d+") "54g" nil 0) ;=> {:i "g" :r "54" :p 2}
  nil)

(defn set [cs]
  (fn [input _ _]
    (when-let [c ((clojure.core/set cs) (first input))]
      {:i (subs input 1)
       :r (str c)
       :p 1})))

^:rct/test
(comment
  ((set "abc") "a" nil 0) ;=> {:i "" :r "a" :p 1}
  ((set "abc") "b" nil 0) ;=> {:i "" :r "b" :p 1}
  ((set "abc") "c" nil 0) ;=> {:i "" :r "c" :p 1}
  ((set "abc") "d" nil 0) ;=> nil
  nil)

(defn take [n]
  (fn [input _ _]
    (cond
      (zero? n)
      , {:i input :p 0}
      (pos? n)
      , (when (<= n (count input))
          {:i (subs input n)
           :r (subs input 0 n)
           :p n})
      (neg? n)
      , (let [c (count input)]
          (when (< c (Math/abs n))
            {:i input
             :p c})))))

^:rct/test
(comment
  ((take 0) "abc" nil 0) ;=> {:i "abc" :p 0}
  ((take 2) "abc" nil 0) ;=> {:i "c" :r "ab" :p 2}
  ((take -1) "abc" nil 0) ;=> nil
  ((take -1) "" nil 0) ;=> {:i "" :p 0}
  nil)

; Combinators

(defn not [parser]
  (fn [input args position]
    (when-not (parser input args position)
      {:i input
       :p 0})))

^:rct/test
(comment
  ((not (literal "a")) "b" nil 0) ;=> {:i "b" :p 0}
  ((not (literal "a")) "a" nil 0) ;=> nil
  nil)

(defn between [low high parser]
  (fn [input args position]
    (loop [o {:i input :p position}
           cnt 0]
      (if (< cnt high)
        (if-let [o' (parser (o :i) args position)]
          (recur (-> o
                     (assoc :i (o' :i))
                     (update :r str (o' :r))
                     (update :p + (o' :p)))
                 (inc cnt))
          (when (<= low cnt high)
            o))
        o))))

^:rct/test
(comment
  ((between 3 4 (literal "a")) "aa" nil 0) ;=> nil
  ((between 3 4 (literal "a")) "aaa" nil 0) ;=> {:i "" :r "aaa" :p 3}
  ((between 3 4 (literal "a")) "aaaa" nil 0) ;=> {:i "" :r "aaaa" :p 4}
  ((between 3 4 (literal "a")) "aaaaa" nil 0) ;=> {:i "a" :r "aaaa" :p 4}
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
  (fn [input args position]
    (loop [parsers parsers
           o {:i input :p position}]
      (if (empty? parsers)
        o
        (let [[parser & parsers] parsers]
          (when-let [o' (parser (o :i) args (o :p))]
            (recur parsers
                   {:i (o' :i)
                    :r (str (o :r) (o' :r))
                    :c (concat (o :c) (o' :c))
                    :p (o' :p)})))))))

^:rct/test
(comment
  ((sequence (literal "a") (literal "b")) "abc" nil 0) ;=> {:i "c" :r "ab" :c () :p 2}
  ((sequence (literal "a") (literal "b")) "abc" nil 5) ;=> {:i "c" :r "ab" :c () :p 7}
  nil)

(defn choice [& parsers]
  (fn [input args position]
    (clojure.core/some #(% input args position) parsers)))

^:rct/test
(comment
  ((choice (literal "a") (regex #"ba")) "bac" nil 0) ;=> {:i "c" :r "ba" :p 2}
  nil)

(defn look
  ([offset]
   (look 0 (take offset)))
  ([offset parser]
   (fn [input args position]
     (when (parser (subs input offset) args position)
       {:i input :p 0}))))

^:rct/test
(comment
  ((look 2) "a" nil 0) ;=> nil
  ((look 2) "abc" nil 0) ;=> {:i "abc" :p 0}
  ((look 2 (literal "cd")) "abcd" nil 0) ;=> {:i "abcd" :p 0}
  ((look 2 (literal "ef")) "abcd" nil 0) ;=> nil
  nil)

(defn given [pred parser]
  (fn [input args position]
    (when (pred input args position)
      (parser input args position))))

^:rct/test
(comment
  ((given (literal "a") (literal "abc")) "abcd" nil 0) ;=> {:i "d" :r "abc" :p 3}
  ((given (literal "b") (literal "abc")) "abcd" nil 0) ;=> nil
  nil)

(defn given-not [pred parser]
  (given (not pred) parser))

^:rct/test
(comment
  ((given-not (literal "a") (literal "abc")) "abcd" nil 0) ;=> nil
  ((given-not (literal "b") (literal "abc")) "abcd" nil 0) ;=> {:i "d" :r "abc" :p 3}
  nil)

(defn to [parser]
  (fn [input args position]
    (loop [input input
           result ""
           pos position]
      (cond
        (nil? (first input)) nil
        (parser input args position) {:i input :r result :p pos}
        :else (recur (subs input 1)
                     (str result (first input))
                     (inc pos))))))

^:rct/test
(comment
  ((to (literal "d")) "abcd" nil 0) ;=> {:i "d" :r "abc" :p 3}
  ((to (literal "p")) "abcd" nil 0) ;=> nil
  nil)

(defn thru [parser]
  (fn [input args position]
    (loop [o {:i input :p position}]
      (when-not (nil? (first (o :i)))
        (if-let [o' (parser (o :i) args (o :p))]
          (assoc o' :r (o :r))
          (recur {:i (subs (o :i) 1)
                  :r (str (o :r) (first (o :i)))
                  :p (inc (o :p))}))))))

^:rct/test
(comment
  ((thru (literal "d")) "abcde" nil 0) ;=> {:i "e" :r "abc" :p 4}
  ((thru (literal "p")) "abcde" nil 0) ;=> nil
  nil)

(defn sub [window-parser parser]
  (fn [input args position]
    (when-let [o (window-parser input args position)]
      (let [o' (parser (o :r) args position)]
        {:i (o :i)
         :r (o' :r)
         :p (o :p)}))))

^:rct/test
(comment
  ((sub (to (literal ";")) (any (choice (literal "a") (literal ";")))) "aaa;aaa" nil 0) ;=> {:i ";aaa" :r "aaa" :p 3}
  nil)

; Captures

(defn capture
  ([parser]
   (fn [input args position]
     (let [o (parser input args position)]
       (update o :c (fnil conj []) (o :r)))))
  ([parser tag]
   (fn [input args position]
     (let [o (parser input args position)]
       (update o :c (fnil conj []) {tag (o :r)})))))

^:rct/test
(comment
  ((capture (literal "a")) "abc" nil 0) ;=> {:i "bc" :r "a" :c ["a"] :p 1}
  ((capture (literal "a") :x) "abc" nil 0) ;=> {:i "bc" :r "a" :c [{:x "a"}] :p 1}
  ((sequence (literal "a") (capture (literal "b") :x) (literal "c")) "abc" nil 0) ;=> {:i "" :r "abc" :c [{:x "b"}] :p 3}
  ((capture (sequence (literal "a") (literal "b"))) "abc" nil 0) ;=> {:i "c" :r "ab" :c ["ab"] :p 2}
  nil)

(defn replace
  ([parser f]
   (fn [input args position]
     (update (parser input args position) :c #(do [(apply f %)]))))
  ([parser f tag]
   (fn [input args position]
     (update (parser input args position) :c #(do [{tag (apply f %)}])))))

^:rct/test
(comment
  ((replace (capture (regex "\\d+")) parse-long) "25a" nil 0) ;=> {:i "a" :r "25" :c [25] :p 2}
  nil)

(defn constant
  ([c]
   (fn [input _ _]
     {:i input :r c :c [c] :p 0}))
  ([c tag]
   (fn [input _ _]
     {:i input :r c :c [{tag c}] :p 0})))

^:rct/test
(comment
  ((constant "c") "b" nil 0) ;=> {:i "b" :r "c" :c ["c"] :p 0}
  ((constant "c" :x) "b" nil 0) ;=> {:i "b" :r "c" :c [{:x "c"}] :p 0}
  nil)

(defn argument
  ([n]
   (fn [input args _]
     (let [arg (args n)]
       {:i input :r arg :c [arg] :p 0})))
  ([n tag]
   (fn [input args _]
     (let [arg (args n)]
       {:i input :r arg :c [{tag arg}] :p 0}))))

^:rct/test
(comment
  ((argument 1) "abc" [:a :b :c] 0) ;=> {:i "abc" :r :b :c [:b] :p 0}
  ((argument 1 :x) "abc" [:a :b :c] 0) ;=> {:i "abc" :r :b :c [{:x :b}] :p 0}
  nil)

(defn position
  ([]
   (fn [input _ position]
     {:i input :c [position] :p position}))
  ([tag]
   (fn [input _ position]
     {:i input :c [{tag position}] :p position})))

^:rct/test
(comment
  ((sequence (literal "a") (position)) "abc" nil 0) ;=> {:i "bc" :r "a" :c [1] :p 1}
  ((sequence (literal "a") (position :pos)) "abc" nil 0) ;=> {:i "bc" :r "a" :c [{:pos 1}] :p 1}
  nil)

; Matcher

(defn match [parser input & args]
  (let [{:keys [c r]} (parser input args 0)]
    (or c r)))

^:rct/test
(comment
  (match (literal "s") "s") ;=> "s"
  (match (capture (literal "s")) "s") ;=> ["s"]
  nil)
