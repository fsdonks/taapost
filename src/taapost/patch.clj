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
      (.eval (slurp (clojure.java.io/file "resources/vega.js")))
      (.eval (slurp (clojure.java.io/file "resources/vega-lite.js"))))))
(in-ns 'taapost.patch)
