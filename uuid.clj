(ns uuid)

(def uuid-pattern #"[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}")

(defn uuid? [s]
  (or (clojure.core/uuid? s)
      (and (string? s)
           (re-matches uuid-pattern s))))

(defn find-uuid [s]
  (re-find uuid-pattern s))

(defn find-uuids [s]
  (re-seq uuid-pattern s))
