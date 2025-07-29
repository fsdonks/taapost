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

(defn svg [from to & {:keys [pre-raster] :or {pre-raster identity}}]
  (let [spec (if (map? from) (json/encode from) (oz/load from))]
    (->  spec
         str
         darkstar/vega-lite-spec->svg
         pre-raster)))

(in-ns 'applied-science.darkstar)
;;this just points us to "our" version of vega instead of the one bundled with
;;darkstar, which is old and prone to svg errors.
(def engine
  (let [engine (.getEngineByName (javax.script.ScriptEngineManager.) "graal.js")
        bindings (.getBindings engine javax.script.ScriptContext/ENGINE_SCOPE)]
    (.put bindings "polyglot.js.allowAllAccess" true)
    (doto engine
      ;; XXX minimal polyfill for part of the fetch and fs APIs, brittle af
      (.eval "
async function fetch(path, options) {
  var body = Java.type('clojure.core$slurp').invokeStatic(path,null);
  return {'ok' : true,
          'body' : body,
          'text' : (function() {return body;}),
          'json' : (function() {return JSON.parse(body);})};
}
function readFile(path, callback) {
  try {
    var data = Java.type('applied_science.darkstar$read_file').invokeStatic(path);
    callback(null, data);
  } catch (err) {
    printErr(err);
  }
}
var fs = {'readFile':readFile};
")
      (.eval (slurp (.getResource (class oz.headless/render ) "/oz/public/js/vega.js")))
      (.eval (slurp (.getResource (class oz.headless/render ) "/oz/public/js/vega-lite.js"))))))

;;have to redef these with the patched engine
(defn make-js-fn [js-text]
  (let [^java.util.function.Function f (.eval engine js-text)]
    (fn [& args] (.apply f (to-array args)))))

(def vega-lite->vega
  "Converts a VegaLite spec into a Vega spec."
  (make-js-fn "function(vlSpec) { return JSON.stringify(vegaLite.compile(JSON.parse(vlSpec)).spec);}"))

(def vega-spec->view
  "Converts a Vega spec into a Vega view object, finalizing all resources."
  (make-js-fn "function(spec) { return new vega.View(vega.parse(JSON.parse(spec)), {renderer:'svg'}).finalize();}"))

(def view->svg
  "Converts a Vega view object into an SVG."
  (make-js-fn "function (view) {
    var promise = Java.type('clojure.core$promise').invokeStatic();
    view.toSVG(1.0).then(function(svg) {
        Java.type('clojure.core$deliver').invokeStatic(promise,svg);
    }).catch(function(err) {
        Java.type('clojure.core$deliver').invokeStatic(promise,'<svg><text>error</text></svg>');
    });
    return promise;
}"))
(in-ns 'taapost.patch)
