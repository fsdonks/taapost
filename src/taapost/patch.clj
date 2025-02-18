(ns taapost.patch)

;;temporary monkeypatches for roz.
(in-ns 'oz.headless)
(defn render [from to & {:keys [pre-raster] :or {pre-raster identity}}]
  (let [spec (if (map? from) (json/encode from) (oz/load from))]
    (->  spec
         str
         darkstar/vega-lite-spec->svg
         pre-raster
         b/parse-svg-string
         (b/render-svg-document to))))
(in-ns 'taapost.patch)
