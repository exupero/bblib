(ns pull)

(defn pluck [o fields fetch]
  (cond
    (map? o)
    , (if (= '[*] fields)
        o
        (into {}
              (map (fn [field]
                     (cond
                       (or (keyword? field)
                           (string? field))
                       , [field (get o field)]
                       (map? field)
                       , (let [[k fields] (first field)]
                           (cond
                             (keyword? k)
                             , (let [o' (if (namespace k)
                                          (fetch [k o])
                                          (get o k))]
                                 [k (pluck o' fields fetch)])
                             (vector? k)
                             , (let [[q & params] k
                                     filt (into [q o] params)]
                                 [q (pluck (fetch filt) fields fetch)])))
                       :else
                       , (throw (Exception. (str "don't know how to get field " (pr-str field)))))))
              fields))
    (sequential? o)
    , (map #(pluck % fields fetch) o)
    :else
    , (throw (Exception. (str "don't know how to pluck fields from " (pr-str o))))))

(defn pull [q fetch]
  (let [[filt fields] (first q)]
    (pluck (fetch filt) fields fetch)))

(comment
  (defmulti fetch first)
  (defmethod fetch :item/id [_]
    {:id "a4340bef-ec27-49a5-a362-32f9cd699381"
     :name "Alice"
     :friends [{:id "fe788a57-9af1-4402-aca8-0dee3362d92d" :name "Bob"}
               {:id "057c4a19-0101-4fcc-a87f-574ebd8d13cc" :name "Carl"}]})
  (defmethod fetch :friend/status [[_ {:keys [id name]}]]
    {:id id :name name :status :sad})

  (pull '{[:item/id "..."] [:name
                            {:friends [:name
                                       {:friend/status [:id :status]}]}]}
        fetch))
