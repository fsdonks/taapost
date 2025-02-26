(ns taapost.util
  (:require [spork.cljgui.components [swing :as gui]]
            [tablecloth.api :as tc]))

;;a little helper ported from spork.util.table to tablecloth.
;;useful for looking at intermediate tables...
(defn visualize   [obj & {:keys [title sorted] :or {title "some data" sorted true}}]
  (gui/->scrollable-view
   (gui/->swing-table (tc/column-names obj)
                      (tc/rows obj) :sorted sorted)))
