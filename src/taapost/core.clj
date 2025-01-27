(ns taapost.core
  (:require [cljplot.render :as r]
            [cljplot.build :as b]
            [cljplot.common :as common]
            [fastmath.interpolation :as in]
            [fastmath.interpolation.step :as sin]
            [fastmath.interpolation.monotone :as min]
            [fastmath.stats :as stats]
            [clojure2d.color :as c]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [cljplot.core :refer [save show]]
            [java-time :as dt]
            [fastmath.random :as rnd]))
;;trying to reproduce some of the janky
;;shave chart and n-list processing here.
;;So we have a portable way to run stuff
;;with the R dependency.

;;the gist is..
;;we need to dice up performance data
;;by branch, then SRC.

;;There's some normalization by str that
;;happens (tbd).

;;All values are normalized to demand.


;;pulled from cljplot examples/vega/stacked-bar-weather

(defn sift [ks xs]
  (reduce (fn [acc r]
            (reduce (fn [inner k]
                      (update inner k conj (get r k)))
                    acc ks))
          (zipmap ks (repeat [])) xs))

(defn demo []
  (let [raw   {"SRC0" [1 1  1 1  1]
               "SRC1" [2 10 1 1  1]}
        data  raw #_(->> raw
                   (sift [:RA :RA :Unmet :RCU :RCUD]))
        legend [[:rect "RA" {:color :green}]
                [:rect "RC" {:color :darkblue}]
                [:rect "Unmet Demand" {:color :yellow}]
                [:rect "RC Unavailable Demand" {:color :lightyellow}]
                [:rect "RC Unavailable" {:color :white}]]]
    (-> (b/series
         [:grid nil {:x nil}]
         [:stack-vertical [:sbar data {:palette [:green :darkblue :yellow :lightyellow :white]}]])
        (b/preprocess-series)
        (b/update-scale :x :scale [:bands {:padding-in 0.0 :padding-out 0.2}])
        (b/update-scale :y :ticks 10)
        (b/update-scale :y :fmt "%,.0f")
        (b/add-axes :left)
        (b/add-axes :bottom)
        (b/add-label :bottom "SRC")
        (b/add-label :left "Quantity")
        (b/add-legend "Bar type" legend)
        (r/render-lattice {:width 500 :height 300})
        #_(save "results/vega/stacked-bar-weather.jpg")
        (show))))
