;;shave chart templates
(ns taapost.shave
  (:require [clojure.walk :as w]
            [oz [core :as oz] [headless :as h]]
            [tablecloth.api :as tc]))

(defn unjson [in]
  (w/postwalk
   (fn [frm]
     (if (map? frm)
       (zipmap (map keyword (keys frm)) (vals frm))
       frm)) in))

(def stacked-vl
  {:data {:url "https://raw.githubusercontent.com/vega/vega/refs/heads/main/docs/data/barley.json"},
   :title {:text  "Aviation-Aggregated modeling Results as Percentages of Demand"
           :subtitle "Conflict-Phase 3 Most Stressful Scenario"}
   :config {:background "lightgrey"}
   :width 800
   :encoding
   {:x {:type "nominal", :field "variety"
        :axis {:labels false :title nil}}
    :y {:type "quantitative", :aggregate "sum", :field "yield", :stack "zero" }},
   :layer
   [{:mark {:type "bar" :binSpacing 20 :width 20},
     :encoding {:color {:type "nominal", :field "site"
                        :legend {:direction "horizontal"
                                 :orient "bottom"}}}}
    #_
    {:mark {:type "text", :color "black", :dy -15, :dx 0},
     :encoding
     {:detail {:type "nominal", :field "site"},
      :text
      {:type "quantitative", :aggregate "sum", :field "yield", :format ".1f"}}}
    ;;title
    {:mark {:type "text", :color "black", :dy 10, :dx 0 :angle -90 :align "left"},
     :encoding
     {:detail {:type "nominal", :field "variety"},
      :text    {:type "nominal", :field "variety"}
      :y  {:datum 0}}}
    ;;str
    {:mark {:type "text", :color "black", :dy 20, :dx 0 :angle -90 :align "left"},
     :encoding
     {;:detail {:type "nominal", :field "variety"},
      :text    {:type "nominal", :field "variety"}
      :y  {:datum 0}}}
    ;;rule.
    {:mark "rule"
     :encoding {:x nil ;;this works but I'm not happy.
                :y {:datum 100}
                :color {:value "red"}}}
    ]})


;;Shave charts are produced for 2 subviews:
;;Campaigning, Phase3.
;;Only for a single level of supply (statically at least).


;;There are 2 classes of charts:
;;SRCs By Branch


;;Aggregate branch.

;;data format...
;;select supply where ra = programmed.
;;group-by [src phase], for phase in [campaignining, phase3]

;;aggregate by rep seed.
;;compute mean by campaigning.

;;categories:
;;RA Supply, RC Supply, RC Unavailable as Portion of Unmet,
;;Unmet Demand, RC Unavailable Leftover from Unmet

;;rep-seed	SRC	phase	AC-fill	NG-fill	RC-fill	AC-overlap	NG-overlap	RC-overlap	total-quantity	AC-deployable	NG-deployable	RC-deployable	AC-not-ready	NG-not-ready	RC-not-ready	AC-total	NG-total	RC-total	AC	NG	RC

;;I think we assume max ac is the programmed supply for now.
;;We can also specify that?
;;Maybe by the time we get here, we should assume we have our inputs pared down to a single supply per src.


;;[src [ac ng rc] phase] -> reps.

;;can we just accumulate a max or programmed supply?
;;tbd - do this using tmd stuff.
(defn get-maxes [ds]
  (->> (tc/rows ds :as-maps)
       (reduce (fn [acc {:keys [ AC NG RC]}]
                 (-> acc
                     (update :AC max AC)
                     (update :NG max NG)
                     (update :RC max RC)))
               {:AC 0 :NG 0 :RC 0})))

;;we just get 1 supply....do we know what the programmed force supply is?
(defn bar-charts [data]
  (for [[{:keys [SRC]} src-data]
          (tc/group-by data [:SRC] {:result-type :as-map})]
    (let [{ac-max :AC rc-max :RC ng-max :NG} (get-maxes src-data)
          supply (tc/select-rows src-data (fn [{:keys [AC RC NG]}]
                                            (and (= AC ac-max)
                                                 (= RC rc-max)
                                                 (= NG ng-max))))]
      [SRC supply])))

;;Generate bar charts by phases....
;;just get data for one phase.
#_
(def d (tc/dataset "../make-one-to-n/resources/results_4_SRCs.csv" {:key-fn keyword}))

;;given a subdataset for a max supply, we want to grab the phases of interest.
;;generate a bar chart for each phase.

;;assume we have a phase...
;;we have multiple SRCs per branch.
;;for each src, we want to extract
;;[AC RC Unmet-Demand Demand-Met]

;;where AC, RC are directly from the supply.
;;Unmet-Demand 
(defn bar-chart [d phase]
  (let [subdata (tc/select-rows d #(-> % :phase (= phase)))]
    subdata))
