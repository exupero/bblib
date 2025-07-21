(ns time
  (:require [clojure.string :as str]
            [babashka.deps :as deps]))
(deps/add-deps '{:deps {com.widdindustries/cljc.java-time {:mvn/version "0.1.21"}}})
(require '[cljc.java-time.format.date-time-formatter :as dtf]
         '[cljc.java-time.local-date-time :as ldt]
         '[cljc.java-time.offset-date-time :as odt]
         '[cljc.java-time.local-date :as ld]
         '[cljc.java-time.zone-id :as jz]
         '[cljc.java-time.instant :as instant]
         '[cljc.java-time.duration :as d])

(def parse odt/parse)

(def now odt/now)
(def plus-days odt/plus-days)
(def minus-days odt/minus-days)
(def plus-minutes odt/plus-minutes)
(def minus-minutes odt/minus-minutes)

(defn format-as [t pattern]
  (.format (dtf/of-pattern pattern) t))

(defn isoformat-today []
  (format-as (now) "yyyy-MM-dd"))

(defn isoformat-datetime [t]
  (.format t dtf/iso-offset-date-time))

(defn isoformat-date [d]
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSSXXX") d))

(defn datetime? [v]
  (instance? java.time.OffsetDateTime v))

(defmacro timed [form]
  `(let [start# (System/nanoTime)
         res# ~form
         end# (System/nanoTime)]
     [(- end# start#) res#]))

(defn start-of-day [t]
  (-> t
      (.withHour 0)
      (.withMinute 0)
      (.withSecond 0)
      (.withNano 0)))

(defn parse-date [d]
  (.atStartOfDay (ld/parse d)
                 (java.time.ZoneId/systemDefault)))

(defn parse-time [hhmm]
  (let [[_ h m] (re-matches #"(\d{1,2})(\d{2})" hhmm)
        h (Integer/parseInt h)
        m (Integer/parseInt m)]
    (-> (now)
        (.withHour h)
        (.withMinute m)
        (.withSecond 0)
        (.withNano 0))))

(defn nanos->iso [nanos]
  (let [formatter (-> (dtf/of-pattern "yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSSZ")
                      (dtf/with-zone (jz/of "UTC")))
        inst (instant/of-epoch-second (/ nanos 1000000000) (mod nanos 1000000000))]
    (-> (.format formatter inst)
        (str/replace #"\+0000$" "Z"))))

(defn as-nanos [t]
  (let [inst (.toInstant t)]
    (+ (* (.getEpochSecond inst) 1000000000)
       (.getNano inst))))

(defn iso->nanos [iso]
  (as-nanos (parse iso)))

(defn seconds->days [s]
  (/ s 60 60 24))

(defn date-extent [dates]
  (let [instants (->> dates (keep #(some-> % .toInstant)) sort)]
    [(first instants) (last instants)]))

(defn span [dates]
  (->> dates sort date-extent (apply d/between)))

(defn time-to-nearest [t p]
  (let [h (odt/get-hour t)
        m (-> t
              odt/get-minute
              (/ p)
              float
              Math/round
              (* p))
        [h m] (if (< 59 m)
                [(inc h) (mod m 60)]
                [h m])]
    (-> t
        (odt/with-hour h)
        (odt/with-minute m))))

(defn ->epoch-seconds [t]
  (.getEpochSecond (.toInstant t)))
