(ns python.exec
  (:require [clojure.java.io :as io]
            [babashka.process :as p]))

; Has to be done at namespace level, not in function, otherwise *file* refers
; to the script rather than this file
(def dir (.getParent (io/file *file*)))

(defn run-chunks [opts chunks]
  (let [opts (merge {:in :stream :out :stream :err :stream} opts)
        {:keys [in out err] :as proc} (p/process opts "python" (io/file dir "exec.py"))
        in (io/writer in)
        out (io/reader out)
        err (io/reader err)]
    (doseq [code chunks]
      (.write in (pr-str code))
      (.write in "\n")
      (.flush in))
    (.close in)
    (let [{:keys [exit]} @proc]
      {:out (line-seq out)
       :err (line-seq err)
       :exit exit})))

(defn run [opts code]
  (run-chunks opts [code]))

(comment
  (run {} "print('hello')\nprint(1+2)")
  (run {} "6")

  (run-chunks {} ["print('chunk1')" "print('chunk2')"])

  (time (run-chunks {}
                    (lazy-cat
                      ["print('hello')"]
                      [(do
                         (Thread/sleep 2000)
                         "print('bye')")]))))
