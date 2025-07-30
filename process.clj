(ns process
  (:refer-clojure :exclude [send!])
  (:require [clojure.java.io :as io]
            [babashka.process :as p]))

(defn process [cmd]
  (let [{:keys [in out err]} (p/process {:in :stream :out :stream :err :stream} cmd)]
    {:in (io/writer in)
     :out (io/reader out)
     :err (io/reader err)
     :process process}))

(defn send! [{:keys [in]} lines]
  (doseq [line lines]
    (.write in line)
    (.write in "\n"))
  (.flush in))

(defn read-all! [{:keys [out]}]
  (loop [res []]
    (if (.ready out)
      (if-let [line (.readLine out)]
        (recur (conj res line))
        res)
      res)))

(comment
  (def proc (process "python -iq"))

  (send! proc
    ["print('Hello from Clojure!')"
     "3+5"])

  (read-all! proc))
