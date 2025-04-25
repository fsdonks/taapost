;;Another plot resurrected from Craig.
;;This is from a side-job where we have various options
;;to implement reduction targets.  The response is going
;;to be change in fill by nominal sampling period, rendered
;;by [branch [phase [option *]]]
;;as a normalized stacked bar chart (where option is the categorical x axis,
;;and the trends are colored assessments of [green redd black],
;;which map to filled, new-risk, unfilled), faceted by branch and phase
;;We expect filled/new-risk to vary by option, typically showing an
;;intuitive increase in new-risk (e.g. worsening fill).
(ns taapost.fillfacet
  (:require [clojure.walk :as w]
            [oz [core :as oz] [headless :as h]]
            [scicloj.tableplot.v1.hanami :as hanami]
            [aerial.hanami.templates :as ht]
            [hiccup.core :as hc]
            [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.libs.fastexcel]
            [clojure.string :as s]
            [taapost.patch]
            [taapost.util :as u]
            [spork.util.io :as io]
            [clojure.data.json :as json]))


(comment
  (def base-fill
    {"Aviation" {"Competition" 0.90
                 "Conflict"    0.90
                 "RTC"         0.90}
     "Donkeys" {"Competition" 0.70
                "Conflict"    0.70
                "RTC"         0.70}
     "Hobbits" {"Competition" 0.60
                "Conflict"    0.60
                "RTC"         0.60}})
  (def impact {:light 0.05
               :medium 0.10
               :heavy  0.2
               :nuts 0.3})
  (def branches (->> (range 20) (mapv #(str "Branch " %))))
  (def branch->base (zipmap branches (cycle (keys base-fill))))

  (def notional
    (-> (tc/dataset (for [branch branches
                          phase  ["Competition" "Conflict" "RTC"]
                          option [:light :medium :heavy :nuts]]
                      (let [base     (get-in base-fill [(branch->base branch) phase])
                            unfilled (- 1.0 base)
                            delta    (impact option)
                            fill     (- base delta)
                            new-risk delta]
                        {:branch branch :phase phase :option option
                         :unfilled unfilled
                         :fill fill
                         :new-risk new-risk})))
        (tc/pivot->longer #{:unfilled :fill :new-risk}
                          {:target-columns :trend :value-column-name :value})))
)

(def colors {:forest-green "#238823"
             :cardinal "#D2222D"})
(def bar-spec
  {;; :$schema "https://vega.github.io/schema/vega-lite/v6.json",
   :width 400
   :background "lightgrey"
   :height 50
   :data nil
   :mark "bar",
   :encoding
   {
    :column {:field "phase"
             :header {:title "Phase"}
             :spacing 1},
    :row    {:field "branch"
             :header {:title "Branch" :labelAngle 0 :labelAlign "left"}
             :spacing 1}
    :x     {:field "value", :type "quantitative", :aggregate "sum"
            :axis {#_ #_:labels false :title "Percent Fill" :format "%"}} ,
    :y     {:field "option", :type "nominal"
            :axis {#_ #_:labels false :title nil}},
    :color {:field "trend", :type "nominal"
            :scale {:domain [:fill :new-risk :unfilled]
                    :range  ["green" (colors :cardinal)  "black"]
                            #_[(colors :forest-green) (colors :cardinal)  "black"]}}
    }})

(defn bar-plot [d]
  [:vega  (assoc-in bar-spec
                 [:data :values]
                 (u/records d))])
