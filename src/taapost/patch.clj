(ns taapost.patch)

;;temporary monkeypatches for roz.
(in-ns 'oz.headless)
(defn render [from to & {:keys [pre-raster] :or {pre-raster identity}}]
  (let [;;data  (str "JSON.parse(JSON.stringify("  (json/encode (->> from :data :values )) ")")
        ;;from (assoc-in from [:data :values] "REPLACE-ME")
        spec (if (map? from) (json/encode from) (oz/load from))
        ;;spec (clojure.string/replace spec "\"REPLACE-ME\"" data)
        ]
    (->  spec
         str
         darkstar/vega-lite-spec->svg
         pre-raster
         b/parse-svg-string
         (b/render-svg-document to))))

(defn svg [from to & {:keys [pre-raster] :or {pre-raster identity}}]
  (let [spec (if (map? from) (json/encode from) (oz/load from))]
    (->  spec
         str
         darkstar/vega-lite-spec->svg
         pre-raster)))
(in-ns 'taapost.patch)
