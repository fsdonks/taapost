(ns taapost.util
  (:require [spork.cljgui.components [swing :as gui]]
            [spork.util [io :as io] [table :as tbl]]
            [spork.util.excel.core :as xl]
            [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [clojure.walk :as w]
            [clojure.data.json :as json]))

(defn as-dataset [obj]
  (cond (tc/dataset? obj) obj
        (string? obj) (-> obj
                          io/file-path
                          (tc/dataset {:separator "\t" :key-fn keyword}))
        :else (throw (ex-info "don't know how to coerce to dataset?!" {:in obj}))))

;;backwards compatibility with spork to make it easy to dump xlsx...
(extend-protocol tbl/ITabular
  tech.v3.dataset.impl.dataset.Dataset
  (table-fields  [t] (tc/column-names t))
  (table-columns [t] (vec (vals t))))

;;a little helper ported from spork.util.table to tablecloth.
;;useful for looking at intermediate tables...
(defn visualize   [obj & {:keys [title sorted] :or {title "some data" sorted true}}]
  (gui/->scrollable-view
   (gui/->swing-table (tc/column-names obj)
                      (tc/rows obj) :sorted sorted)))

(defn records
  "Convenience fn to project the dataset onto seq-of-maps.
   Identical to tech.ml.dataset seq-of-maps function."
  [ds]
  (tc/rows ds :as-maps))

;;helpful for messing with vega specs.
;;https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn deep-merge
  "Like clojure.core/merge, but recursively merges maps."
  [& maps]
  (apply merge-with (fn [& args]
                      (if (every? map? args)
                        (apply deep-merge args)
                        (last args)))
         maps))

;;not currently used.
(defn collapse [xs]
  (case (-> xs meta :datatype)
    :string (first xs)
    (dfn/mean xs)))

(defn map-columns*
  "Helper function. Acts like tablecloth.api/map-columns, except we can define
   multiple column mappings inline, meaning we can map many columns. As with
   clojure.core/let, we can define an imperative set of transforms, e.g. later
   column mappings can refer to earlier columns that were defined.

   Expects one or more specs for column mapping, where each spec is
   a triple of column-name, column-selector, and mapping-function.

   (-> the-dataset
       (map-columns* :total [:x :y] +
                     :product [:x :y] *))"
  [ds & specs]
  (->> specs
       (partition 3)
       (reduce (fn [acc [colname sel fn]]
                 (tc/map-columns acc colname sel fn))
               ds)))

(defn aggregate-columns*
  "Helper function. Acts like tablecloth.api/aggregate-columns, except we can define
   multiple column aggregations in a single map.  Combines the convenience of
   tablecloth.api/aggregate and aggregate-columns.

   (-> the-dataset
       (aggregate-columns* {:total    +
                            :product dfn/mean}))"
  [ds fns]
  (tc/aggregate-columns ds (vec (keys fns)) (vec (vals fns))))


(defn unjson [in]
  (w/postwalk
   (fn [frm]
     (if (map? frm)
       (zipmap (map keyword (keys frm)) (vals frm))
       frm)) in))
