(ns pprint)

(defn clojure [text]
  (let [out (StringBuilder.)]
    (loop [cs (seq text)
           column 0
           indents [0]]
      (if-let [[c & cs] cs]
        (do
          (.append out c)
          (cond
            (= c \,)
            , (do
                (doto out
                  (.append \newline)
                  (.append (apply str (repeat (peek indents) " "))))
                (recur cs (peek indents) indents))
            (#{\( \[ \{} c)
            , (recur cs (inc column) (conj indents (inc column)))
            (#{\) \] \}} c)
            , (recur cs (peek (pop indents)) (pop indents))
            :else
            , (recur cs (inc column) indents)))
        (str out)))))

(comment
  (print (clojure "[a,b={c=d,e=f(g,h),i},j,k]")))

(def closing
  {\( \)
   \[ \]
   \{ \}})

(defn c [text]
  (let [out (StringBuilder.)]
    (loop [cs (seq text)
           indents [0]]
      (if-let [[c & cs] cs]
        (cond
          (= c \,)
          , (let [[d & ds] cs
                  cs (if (= \space d) ds cs)]
              (doto out
                (.append c)
                (.append \newline)
                (.append (apply str (repeat (peek indents) " "))))
              (recur cs indents))
          (#{\( \[ \{} c)
          , (let [new-indent (+ 2 (peek indents))
                  [d & ds] cs]
              (if (= d (closing c))
                (do
                  (doto out
                    (.append c)
                    (.append d))
                  (recur ds indents))
                (do
                  (doto out
                    (.append c)
                    (.append \newline)
                    (.append (apply str (repeat new-indent " "))))
                  (recur cs (conj indents new-indent)))))
          (#{\) \] \}} c)
          , (let [new-indent (peek (pop indents))]
              (doto out
                (.append \newline)
                (.append (apply str (repeat new-indent " ")))
                (.append c))
              (recur cs (pop indents)))
          :else
          , (do
              (.append out c)
              (recur cs indents)))
        (str out)))))

(comment
  (print (c "[a,b={c=d,e=f(g,h),i},j,k]"))
  (print (c "[{a,b}, {c,d}]")))

(defn graphql [text]
  (let [out (StringBuilder.)]
    (loop [cs (seq text)
           indents [0]]
      (if-let [[c & cs] cs]
        (cond
          (= c \,)
          , (let [[d & ds] cs
                  cs (if (= \space d) ds cs)]
              (doto out
                (.append c)
                (.append \newline)
                (.append (apply str (repeat (peek indents) " "))))
              (recur cs indents))
          (= c \:)
          , (do
              (doto out
                (.append c)
                (.append \space))
              (recur cs indents))
          (#{\{} c)
          , (let [new-indent (+ 2 (peek indents))
                  [d & ds] cs]
              (.append out \space)
              (if (= d (closing c))
                (do
                  (doto out
                    (.append c)
                    (.append d))
                  (recur ds indents))
                (do
                  (doto out
                    (.append c)
                    (.append \newline)
                    (.append (apply str (repeat new-indent " "))))
                  (recur cs (conj indents new-indent)))))
          (#{\( \[} c)
          , (let [new-indent (+ 2 (peek indents))
                  [d & ds] cs]
              (if (= d (closing c))
                (do
                  (doto out
                    (.append c)
                    (.append d))
                  (recur ds indents))
                (do
                  (doto out
                    (.append c)
                    (.append \newline)
                    (.append (apply str (repeat new-indent " "))))
                  (recur cs (conj indents new-indent)))))
          (#{\) \] \}} c)
          , (let [new-indent (peek (pop indents))]
              (doto out
                (.append \newline)
                (.append (apply str (repeat new-indent " ")))
                (.append c))
              (recur cs (pop indents)))
          :else
          , (do
              (.append out c)
              (recur cs indents)))
        (str out)))))

(comment
  (print (graphql "a{b(c:d){e f, g}}")))
