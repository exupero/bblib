(ns geojson)

(defn point [lon lat]
  {:type "Feature",
   :geometry {:coordinates [lon lat],
              :type "Point"}})

(defn line-string [coords]
  {:type "Feature"
   :properties {}
   :geometry {:type "LineString"
              :coordinates coords}})

(defn feature-collection [features]
  {:type "FeatureCollection"
   :features features})

(defn points [locs]
  (feature-collection
    (map #(apply point %) locs)))

(defn parse-coord [s]
  (let [coords (->> (re-seq #"(?:(\d+\.?\d*)°?|(\d+°\d+['′]\d+[\"″]))\s*([NSEW])" s)
                    (into {} (map (fn [[_ decimal degrees direction]]
                                    (if decimal
                                      [direction (Float/parseFloat decimal)]
                                      [direction (let [[d m s] (re-seq #"\d+" degrees)]
                                                   (+ (Float/parseFloat d)
                                                      (/ (Float/parseFloat m) 60)
                                                      (/ (Float/parseFloat s) 3600)))])))))]
    {:latitude (or (coords "N") (- (coords "S")))
     :longitude (or (coords "E") (- (coords "W")))}))

(comment
  (parse-coord "46.32423 N  118.234 W")
  (parse-coord "46.32423°N  118.234°W")
  (parse-coord "46°3′10″ N  118°26′29″ W"))
