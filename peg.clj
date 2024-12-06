(ns peg
  (:refer-clojure :exclude [not range repeat replace sequence set some take])
  (:require [clojure.string :as str]))

; inspired by https://janet-lang.org/docs/peg.html
; state structure inspired by https://github.com/ericnormand/squarepeg
; :i = input remaining
; :r = return value
; :c = captures
; :p = position in input

(defn try-match
  ([parser input] (try-match parser input nil))
  ([parser input args] (try-match parser input args 0))
  ([parser input args position]
   (try-match parser input args position 0 0))
  ([parser input args position line column]
   (parser input args position line column)))

(defn lc [s]
  (let [lines (str/split s #"\n" -1)]
    [(dec (count lines))
     (count (last lines))]))

^:rct/test
(comment
  (lc "") ;=> [0 0]
  (lc "\n\n") ;=> [2 0]
  (lc "ab\nde") ;=> [1 2]
  nil)

(defn update-lc [[start-line start-column] o]
  (if-let [s (o :r)]
    (let [[l c] (lc s)]
      (assoc o
             :pl (+ start-line l)
             :pc (if (zero? l)
                   (+ start-column c)
                   c)))
    (assoc o :pl start-line :pc start-column)))

^:rct/test
(comment
  (update-lc [5 4] {:r "abc"}) ;=> {:r "abc" :pl 5 :pc 7}
  (update-lc [5 4] {:r "\n\nabc"}) ;=> {:r "\n\nabc" :pl 7 :pc 3}
  nil)

; Constants

(defn success []
  (fn [input _ position line column]
    {:i input :p position :pl line :pc column}))

(defn fail []
  (constantly nil))

; Parsers

(defn literal [s]
  (fn [input _ position line column]
    (when (str/starts-with? input s)
      (let [c (count s)]
        (update-lc [line column]
                   {:i (subs input c)
                    :r s
                    :p (+ position c)})))))

^:rct/test
(comment
  (try-match (literal "s") "s") ;=> {:i "" :r "s" :p 1 :pl 0 :pc 1}
  (try-match (literal "a\nbcd\nef") "a\nbcd\nefg") ;=> {:i "g" :r "a\nbcd\nef" :p 8 :pl 2 :pc 2}
  nil)

(defn range [cs]
  (let [[low high] (map int cs)]
    (fn [input _ position line column]
      (when (<= low (int (first input)) high)
        (update-lc [line column]
                   {:i (subs input 1)
                    :r (str (first input))
                    :p (inc position)})))))

^:rct/test
(comment
  (try-match (range "an") "k") ;=> {:i "" :r "k" :p 1 :pl 0 :pc 1}
  (try-match (range "ae") "g") ;=> nil
  nil)

(defn regex [pattern]
  (fn [input _ position line column]
    (when-let [r (re-find (re-pattern (str "^" pattern)) input)]
      (let [c (count r)]
        (update-lc [line column]
                   {:i (subs input c)
                    :r r
                    :p (+ position c)})))))

^:rct/test
(comment
  (try-match (regex #"\d+") "54g") ;=> {:i "g" :r "54" :p 2 :pl 0 :pc 2}
  nil)

(defn set [cs]
  (fn [input _ position line column]
    (when-let [c ((clojure.core/set cs) (first input))]
      (update-lc [line column]
                 {:i (subs input 1)
                  :r (str c)
                  :p (inc position)}))))

^:rct/test
(comment
  (try-match (set "abc") "a") ;=> {:i "" :r "a" :p 1 :pl 0 :pc 1}
  (try-match (set "abc") "b") ;=> {:i "" :r "b" :p 1 :pl 0 :pc 1}
  (try-match (set "abc") "c") ;=> {:i "" :r "c" :p 1 :pl 0 :pc 1}
  (try-match (set "abc") "d") ;=> nil
  nil)

(defn take [n]
  (fn [input _ position line column]
    (cond
      (zero? n)
      , {:i input :p position}
      (pos? n)
      , (when (<= n (count input))
          (update-lc [line column]
                     {:i (subs input n)
                      :r (subs input 0 n)
                      :p (+ position n)}))
      (neg? n)
      , (let [c (count input)]
          (when (< c (Math/abs n))
            {:i input
             :p (+ position c)})))))

^:rct/test
(comment
  (try-match (take 0) "abc") ;=> {:i "abc" :p 0}
  (try-match (take 2) "abc") ;=> {:i "c" :r "ab" :p 2 :pl 0 :pc 2}
  (try-match (take -1) "abc") ;=> nil
  (try-match (take -1) "") ;=> {:i "" :p 0}
  nil)

; Combinators

(defn not [parser]
  (fn [input args position line column]
    (when-not (parser input args position line column)
      {:i input
       :p 0})))

^:rct/test
(comment
  (try-match (not (literal "a")) "b") ;=> {:i "b" :p 0}
  (try-match (not (literal "a")) "a") ;=> nil
  nil)

(defn between [low high parser]
  (fn [input args position line column]
    (loop [o {:i input :p position}
           cnt 0]
      (if (< cnt high)
        (if-let [o' (parser (o :i) args position line column)]
          (recur (-> o
                     (assoc :i (o' :i))
                     (update :r str (o' :r))
                     (update :p + (o' :p)))
                 (inc cnt))
          (when (<= low cnt high)
            (update-lc [line column] o)))
        (update-lc [line column] o)))))

^:rct/test
(comment
  (try-match (between 3 4 (literal "a")) "aa") ;=> nil
  (try-match (between 3 4 (literal "a")) "aaa") ;=> {:i "" :r "aaa" :p 3 :pl 0 :pc 3}
  (try-match (between 3 4 (literal "a")) "aaaa") ;=> {:i "" :r "aaaa" :p 4 :pl 0 :pc 4}
  (try-match (between 3 4 (literal "a")) "aaaaa") ;=> {:i "a" :r "aaaa" :p 4 :pl 0 :pc 4}
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
  (fn [input args position line column]
    (loop [parsers parsers
           o {:i input :p position}]
      (if (empty? parsers)
        (update-lc [line column] o)
        (let [[parser & parsers] parsers]
          (when-let [o' (parser (o :i) args (o :p) (o :pl 0) (o :pc 0))]
            (recur parsers
                   {:i (o' :i)
                    :r (str (o :r) (o' :r))
                    :c (concat (o :c) (o' :c))
                    :p (o' :p)})))))))

^:rct/test
(comment
  (try-match (sequence (literal "a") (literal "b")) "abc") ;=> {:i "c" :r "ab" :c () :p 2 :pl 0 :pc 2}
  (try-match (sequence (literal "a") (literal "b")) "abc" nil 5) ;=> {:i "c" :r "ab" :c () :p 7 :pl 0 :pc 2}
  nil)

(defn choice [& parsers]
  (fn [input args position line column]
    (clojure.core/some #(% input args position line column) parsers)))

^:rct/test
(comment
  (try-match (choice (literal "a") (regex #"ba")) "bac") ;=> {:i "c" :r "ba" :p 2 :pl 0 :pc 2}
  nil)

(defn look
  ([offset]
   (look 0 (take offset)))
  ([offset parser]
   (fn [input args position line column]
     (when (parser (subs input offset) args position line column)
       {:i input :p 0}))))

^:rct/test
(comment
  (try-match (look 2) "a") ;=> nil
  (try-match (look 2) "abc") ;=> {:i "abc" :p 0}
  (try-match (look 2 (literal "cd")) "abcd") ;=> {:i "abcd" :p 0}
  (try-match (look 2 (literal "ef")) "abcd") ;=> nil
  nil)

(defn given [pred parser]
  (fn [input args position line column]
    (when (pred input args position line column)
      (parser input args position line column))))

^:rct/test
(comment
  (try-match (given (literal "a") (literal "abc")) "abcd") ;=> {:i "d" :r "abc" :p 3 :pl 0 :pc 3}
  (try-match (given (literal "b") (literal "abc")) "abcd") ;=> nil
  nil)

(defn given-not [pred parser]
  (given (not pred) parser))

^:rct/test
(comment
  (try-match (given-not (literal "a") (literal "abc")) "abcd") ;=> nil
  (try-match (given-not (literal "b") (literal "abc")) "abcd") ;=> {:i "d" :r "abc" :p 3 :pl 0 :pc 3}
  nil)

(defn to [parser]
  (fn [input args position line column]
    (loop [input input
           result ""
           pos position]
      (cond
        (nil? (first input))
        , nil
        (parser input args position line column)
        , (update-lc [line column]
                     {:i input :r result :p pos})
        :else
        , (recur (subs input 1)
                 (str result (first input))
                 (inc pos))))))

^:rct/test
(comment
  (try-match (to (literal "d")) "abcd") ;=> {:i "d" :r "abc" :p 3 :pl 0 :pc 3}
  (try-match (to (literal "p")) "abcd") ;=> nil
  nil)

(defn thru [parser]
  (fn [input args position line column]
    (loop [o {:i input :p position}]
      (when-not (nil? (first (o :i)))
        (if-let [o' (parser (o :i) args (o :p) (o :pl 0) (o :pc 0))]
          (update-lc [line column]
                     (assoc o' :r (str (o :r) (o' :r))))
          (recur {:i (subs (o :i) 1)
                  :r (str (o :r) (first (o :i)))
                  :p (inc (o :p))}))))))

^:rct/test
(comment
  (try-match (thru (literal "d")) "abcde") ;=> {:i "e" :r "abcd" :p 4 :pl 0 :pc 4}
  (try-match (thru (literal "p")) "abcde") ;=> nil
  nil)

(defn sub [window-parser parser]
  (fn [input args position line column]
    (when-let [o (window-parser input args position line column)]
      (let [o' (parser (o :r) args position line column)]
        (update-lc [line column]
                   {:i (o :i)
                    :r (o' :r)
                    :p (o :p)})))))

^:rct/test
(comment
  (try-match (sub (to (literal ";")) (any (choice (literal "a") (literal ";")))) "aaa;aaa") ;=> {:i ";aaa" :r "aaa" :p 3 :pl 0 :pc 3}
  nil)

; Captures

(defn capture
  ([parser]
   (fn [input args position line column]
     (let [o (parser input args position line column)]
       (update o :c (fnil conj []) (o :r)))))
  ([parser tag]
   (fn [input args position line column]
     (let [o (parser input args position line column)]
       (update o :c (fnil conj []) {tag (o :r)})))))

^:rct/test
(comment
  (try-match (capture (literal "a")) "abc") ;=> {:i "bc" :r "a" :c ["a"] :p 1 :pl 0 :pc 1}
  (try-match (capture (literal "a") :x) "abc") ;=> {:i "bc" :r "a" :c [{:x "a"}] :p 1 :pl 0 :pc 1}
  (try-match (sequence (literal "a") (capture (literal "b") :x) (literal "c")) "abc") ;=> {:i "" :r "abc" :c [{:x "b"}] :p 3 :pl 0 :pc 3}
  (try-match (capture (sequence (literal "a") (literal "b"))) "abc") ;=> {:i "c" :r "ab" :c ["ab"] :p 2 :pl 0 :pc 2}
  nil)

(defn replace
  ([parser f]
   (fn [input args position line column]
     (update (parser input args position line column) :c #(do [(apply f %)]))))
  ([parser f tag]
   (fn [input args position line column]
     (update (parser input args position line column) :c #(do [{tag (apply f %)}])))))

^:rct/test
(comment
  (try-match (replace (capture (regex "\\d+")) parse-long) "25a") ;=> {:i "a" :r "25" :c [25] :p 2 :pl 0 :pc 2}
  nil)

(defn constant
  ([c]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [c] :p position})))
  ([c tag]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [{tag c}] :p position}))))

^:rct/test
(comment
  (try-match (constant "c") "b") ;=> {:i "b" :c ["c"] :p 0 :pl 0 :pc 0}
  (try-match (constant "c" :x) "b") ;=> {:i "b" :c [{:x "c"}] :p 0 :pl 0 :pc 0}
  nil)

(defn argument
  ([n]
   (fn [input args _ line column]
     (let [arg (args n)]
       (update-lc [line column]
                  {:i input :c [arg] :p 0}))))
  ([n tag]
   (fn [input args _ line column]
     (let [arg (args n)]
       (update-lc [line column]
                  {:i input :c [{tag arg}] :p 0})))))

^:rct/test
(comment
  (try-match (argument 1) "abc" [:a :b :c] 0) ;=> {:i "abc" :c [:b] :p 0 :pl 0 :pc 0}
  (try-match (argument 1 :x) "abc" [:a :b :c] 0) ;=> {:i "abc" :c [{:x :b}] :p 0 :pl 0 :pc 0}
  nil)

(defn position
  ([]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [position] :p position})))
  ([tag]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [{tag position}] :p position}))))

^:rct/test
(comment
  (try-match (sequence (literal "a") (position)) "abc") ;=> {:i "bc" :r "a" :c [1] :p 1 :pl 0 :pc 1}
  (try-match (sequence (literal "a") (position :pos)) "abc") ;=> {:i "bc" :r "a" :c [{:pos 1}] :p 1 :pl 0 :pc 1}
  nil)

(defn line
  ([]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [line] :p position})))
  ([tag]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [{tag line}] :p position}))))

^:rct/test
(comment
  (try-match (line) "a" nil 0 4 2) ;=> {:i "a" :c [4] :p 0 :pl 4 :pc 2}
  (try-match (line :l) "a" nil 0 4 2) ;=> {:i "a" :c [{:l 4}] :p 0 :pl 4 :pc 2}
  nil)

(defn column
  ([]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [column] :p position})))
  ([tag]
   (fn [input _ position line column]
     (update-lc [line column]
                {:i input :c [{tag column}] :p position}))))

^:rct/test
(comment
  (try-match (column) "a" nil 0 4 2) ;=> {:i "a" :c [2] :p 0 :pl 4 :pc 2}
  (try-match (column :c) "a" nil 0 4 2) ;=> {:i "a" :c [{:c 2}] :p 0 :pl 4 :pc 2}
  nil)

; Matcher

(defn match [parser input & args]
  (let [{:keys [c r]} (parser input args 0 0 0)]
    (or c r)))

^:rct/test
(comment
  (match (literal "s") "s") ;=> "s"
  (match (capture (literal "s")) "s") ;=> ["s"]
  nil)
