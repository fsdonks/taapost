;;shave chart templates
(ns taapost.shave
  (:require [clojure.walk :as w]
            [oz [core :as oz] [headless :as h]]
            [scicloj.tableplot.v1.hanami :as hanami]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]))

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
;;[AC RC Unmet-Demand Demand-Met ]

;;where AC, RC are directly from the supply.
;;Unmet-Demand

;;demand-met = sum ACFill RCFill + AC-deployable + NG-deployable +  RC-deployable / total-quantity

;; summarise(Demand =mean(Demand)/phase_length_1, SupplyRA = (mean(RAFill) + mean(RAExcess))/phase_length_1,
;;                  SupplyRC = (mean(RCFill)+mean(RCExcess))/phase_length_1, RCTotal = mean(RCTotal+NGTotal)/phase_length_1)

;;for each src, we want to collate the reps down into means.
(defn by-phase [d phase] (tc/select-rows d #(-> % :phase (= phase))))

;;There's the raw fill data for the fm barcharts (the "performance data"),
;;and then the data for plotting shavecharts.
;; #shave chart data = (average daily supply ra + average daily supply rc) / average daily demand 
;; #reduces to average supply ra + average supply rc / average demand reduces to 
;; #sum(supply ra) + sum(supply rc) / sum(demand)
;; #bar chart data mean((RCFill+RCExcess)/Demand)

(defn collapse [xs]
  (case (-> xs meta :datatype)
    :string (first xs)
    (dfn/mean xs)))

(defn map-columns* [ds & specs]
  (->> specs
       (partition 3)
       (reduce (fn [acc [colname sel fn]]
                 (tc/map-columns acc colname sel fn))
               ds)))
;;may not need this so much.
(defn rename [in]
  (let [cols (->> in tc/column-names
                  (map (fn [x]
                         (case x
                           :total-quantity :Demand
                           (-> x name (clojure.string/replace "-" "") keyword)))))]
  (tc/rename-columns in)))
;;SRC	phase	AC-fill	NG-fill	RC-fill	AC-overlap	NG-overlap	RC-overlap	total-quantity	AC-deployable	NG-deployable	RC-deployable	AC-not-ready	NG-not-ready	RC-not-ready	AC-total	NG-total	RC-total	AC	NG	RC


;;why is phase length important?
;;We need to normalize the measures, which are all in demand days, to get them
;;into units of demand/uics etc.  We do this by dividing by phase length.

;;So if we factor our phase length from all the measures we get them into
;;some normal form.
;;note: we can infer phase length trivially from AC-total/AC or
;;any of the total fields....Since it will just be a time-weighted
;;sum of the inventory over the phase-length.  That can simplify our
;;processing a bit.
(defn bar-chart [d phase phase-length]
  (let [normalized-d (fn [f e d] (/ (+ f e) d))
        normalized-t (fn [&  xs] (/ (apply + xs) phase-length))
        subdata  (-> d
                     (by-phase phase)
                     (map-columns* :RAFill [:AC-fill] identity
                                   :RCFill [:RC-fill :NG-fill] dfn/+
                                   ;;in baseline they divide this by phaselength?
                                   :Demand [:total-quantity] identity
                                   :RAExcess [:AC-deployable] identity
                                   :RCExcess  [:NG-deployable :RC-deployable] dfn/+
                                   :RASupply  [:RAFill :RAExcess] normalized-t
                                   :RCSupply  [:RCFill :RCExcess] normalized-t
                                   :UnmetDemand [:RAFill :RCFill :Demand] normalized-d))]
    (tc/aggregate-columns subdata  (disj (set (tc/column-names subdata)) :rep-seed) collapse)))

;;we want to transform into normalized values.
;;For the bar chart, (RAFill + RAExcess)/Demand -> RASupply, RCFill + RCExcees -> RCSupply

(def dt
  (tc/dataset "../make-one-to-n/resources/results_4_SRCs.csv"
              {:key-fn keyword :separator \tab}))

#_
(bar-chart (-> s1 second second) "phase3" 981)

#_
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
