(ns python.exec
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.process :as p]))

; Has to be done at namespace level, not in function, otherwise *file* refers
; to the script rather than this file
(def dir (.getParent (io/file *file*)))

(defprotocol IRunner
  (submit [this script]))

(defn wrap-try [script marker]
  (str "import sys\n"
       "try:\n"
       "    " (str/replace script "\n" "\n    ") "\n"
       "finally:\n"
       "    print('" marker "')\n"
       "    print('" marker "', file=sys.stderr)\n"))

(defrecord Runner [in out err]
  IRunner
  (submit [_ script]
    (let [id (java.util.UUID/randomUUID)
          marker (str "__DONE__" id "__")]
      (async/put! in (wrap-try script marker))
      (let [content (atom {})]
        (loop []
          (async/alt!!
            out ([line]
                 (if (= line marker)
                   (if (@content :err-done?)
                     (let [{:keys [out err]} @content]
                       (reset! content {})
                       {:out out :err err})
                     (do
                       (swap! content assoc :out-done? true)
                       (recur)))
                   (do
                     (swap! content update :out conj line)
                     (recur))))
            err ([line]
                 (if (= line marker)
                   (if (@content :out-done?)
                     (let [{:keys [out err]} @content]
                       (reset! content {})
                       {:out out :err err})
                     (do
                       (swap! content assoc :err-done? true)
                       (recur)))
                   (do
                     (swap! content update :err conj line)
                     (recur))))))))))

(defn runner
  ([opts]
   (let [opts (merge {:in :stream :out :stream :err :stream} opts)
         {:keys [in out err] :as proc} (p/process opts "python" (io/file dir "exec.py"))
         in (io/writer in)
         out (java.io.BufferedReader. (io/reader out))
         err (java.io.BufferedReader. (io/reader err))
         in-queue (async/chan)
         out-queue (async/chan)
         err-queue (async/chan)]
     ; async/go-loop doesn't seem to work here
     (async/thread
       (loop []
         (if-let [code (async/<!! in-queue)]
           (do
             (.write in (pr-str code))
             (.write in "\n")
             (.flush in)
             (recur))
           (do
             (.close in)
             (async/close! out-queue)
             (async/close! in-queue)))))
     ; Don't use async/go-loop because the .readLine blocks and monopolizes the thread
     (async/thread
       (loop []
         (when-let [line (.readLine out)]
           (async/put! out-queue line)
           (recur))))
     ; Don't use async/go-loop because the .readLine blocks and monopolizes the thread
     (async/thread
       (loop []
         (when-let [line (.readLine err)]
           (async/put! err-queue line)
           (recur))))
     (->Runner in-queue out-queue err-queue)))
  ([opts script]
   (let [r (runner opts)]
     (submit r script)
     r)))

(defrecord SingleUseRunner [script-promise result-future]
  IRunner
  (submit [_ script]
    (deliver script-promise script)
    @result-future))

(defn run-chunks [opts chunks])

; This single-use runner is useful for scripts that can call `sys.exit()`, which breaks the reusable runner's
; reader/writer/channel usage; TBD why.
(defn single-use-runner [opts preload]
  (let [script (promise)
        result (future
                 (let [opts (merge {:in :stream :out :stream :err :stream} opts)
                       {:keys [in out err] :as proc} (p/process opts "python" (io/file dir "exec.py"))
                       in (io/writer in)
                       out (io/reader out)
                       err (io/reader err)]
                   (.write in (pr-str preload))
                   (.write in "\n")
                   (.flush in)
                   (.write in (pr-str @script))
                   (.write in "\n")
                   (.flush in)
                   (.close in)
                   (let [{:keys [exit]} @proc]
                     {:out (line-seq out)
                      :err (line-seq err)
                      :exit exit})))]
    (->SingleUseRunner script result)))

(comment
  (run {} "print('hello')\nprint(1+2)")
  (run {} "6")

  (run-chunks {} ["print('chunk1')" "print('chunk2')"])

  (time (run-chunks {}
                    (lazy-cat
                      ["print('hello')"]
                      [(do
                         (Thread/sleep 2000)
                         "print('bye')")])))

  (let [r (runner {})]
    [(submit r "print('hello')")
     (submit r "print('bye')")
     (submit r "print('cheerio'); print('tootles', file=sys.stderr)")]))
