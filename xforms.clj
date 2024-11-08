(ns xforms)

(defn split-by [pred]
  (fn [rf]
    (let [buffer (volatile! nil)]
      (fn
        ([] (rf))
        ([res] (rf (rf res @buffer)))
        ([res item]
         (if-let [buffer' @buffer]
           (if (pred item)
             (do (vreset! buffer [item]) (rf res buffer'))
             (do (vswap! buffer conj item) res))
           (do (vreset! buffer [item]) res)))))))

