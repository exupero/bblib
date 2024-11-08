(ns geojson)

(defn point [lon lat]
  {:type "Feature",
   :geometry {:coordinates [lon lat],
              :type "Point"}})

(defn line-string [coords]
  {:type "Feature"
   :geometry {:type "LineString"
              :coordinates coords}})

(defn feature-collection [features]
  {:type "FeatureCollection"
   :features features})

(defn points [locs]
  (feature-collection
    (map #(apply point %) locs)))
