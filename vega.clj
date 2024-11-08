(ns vega
  (:require [cheshire.core :as json]))

(defn vega-html [spec]
 (format "<!DOCTYPE html><html>
  <head>
    <meta charset=utf-8 />
    <script src='https://cdn.jsdelivr.net/npm/vega@5'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-lite@5'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-embed@6'></script>
  </head>
  <body>
    <div id='vis'></div>
    <script type='text/javascript'>vegaEmbed('#vis', %s);</script>
  </body>
</html>" (json/generate-string spec {:pretty true})))

(defn boxplot
  ([values] (boxplot values {:width 500 :height 500}))
  ([values {:keys [width height title x-title y-title c-title scale]}]
   (let [xs (distinct (map :x values))]
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :title title
      :width (or width (* 50 (count xs)))
      :height height
      :data {:values values}
      :mark {:type "boxplot" :extent "min-max"}
      :encoding (cond-> {:x {:field "x"
                             :type "ordinal"
                             :title x-title
                             :axis {:titleFontSize 16
                                    :labelFontSize 16}}
                         :y (cond-> {:field "y"
                                     :type "quantitative"
                                     :title y-title
                                     :axis {:titleFontSize 16
                                            :labelFontSize 16}}
                              scale (assoc :scale scale))
                         :size {:value 30}}
                  c-title (assoc :color {:field "c"
                                         :type "nominal"
                                         :title c-title}))})))

(defn scatter
  ([values] (scatter values {:width 500 :height 500}))
  ([values {:keys [font-size height width x-title y-title]}]
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :width width
    :height height
    :data {:values (map (partial zipmap [:x :y]) values)}
    :mark "point"
    :encoding {:x {:field "x"
                   :type "quantitative"
                   :axis {:title x-title
                          :titleFontSize font-size
                          :labelFontSize font-size}}
               :y {:field "y"
                   :type "quantitative"
                   :axis {:title y-title
                          :titleFontSize font-size
                          :labelFontSize font-size}}}}))

(defn hconcat [& graphs]
  {:hconcat graphs})

(defn layer [& graphs]
  {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
   :title (-> graphs first :title)
   :layer graphs})

(defn boxplot-layers [opts & layer-values]
  (apply layer (map #(boxplot % opts) layer-values)))

(defn color-layers-by [spec k]
  (-> spec
      (update :layer (fn [layers]
                       (map #(assoc-in % [:encoding :color] {:field k :type :nominal})
                            layers)))))
