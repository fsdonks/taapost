;;shave chart templates
(ns taapost.shave
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
            [spork.util.io :as io]
            [clojure.data.json :as json]))

(defn unjson [in]
  (w/postwalk
   (fn [frm]
     (if (map? frm)
       (zipmap (map keyword (keys frm)) (vals frm))
       frm)) in))

(defn records [ds] (tc/rows ds :as-maps))


;;helpful for messing with vega specs.
;;https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? map? args)
                        (apply deep-merge args)
                        (last args)))
         maps))

(defn col-mean [k] #(-> % (get k) dfn/mean))
;;aspect ratio.
(defn ar [pw ph h]
  [(* h (/ pw ph) 1.0) h])

;;preclude divide by zero errors.
(defn safe-div [x y] (if (zero? y) 0 (dfn// x y)))

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

(defn max-inventory [ds]
  (->> (for [[{:keys [SRC]} src-data]
             (tc/group-by ds [:SRC] {:result-type :as-map})]
         [SRC  (get-maxes src-data)])
       (into {})))

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

;;SRC	phase
;;AC-fill	NG-fill	RC-fill	AC-overlap NG-overlap	RC-overlap
;;total-quantity
;;AC-deployable	NG-deployable	RC-deployable
;;AC-not-ready	NG-not-ready	RC-not-ready
;;AC-total	NG-total	RC-total
;;AC	NG	RC


;;why is phase length important?
;;We need to normalize the measures, which are all in demand days, to get them
;;into units of demand/uics etc.  We do this by dividing by phase length.

;;So if we factor our phase length from all the measures we get them into
;;some normal form.
;;note: we can infer phase length trivially from AC-total/AC or
;;any of the total fields....Since it will just be a time-weighted
;;sum of the inventory over the phase-length.  That can simplify our
;;processing a bit.

;;depending on the input data, it's possible we have no supply
;;for any phase.  So we get a divide by zero error.
(defn derive-length [AC-total NG-total RC-total AC NG RC]
    (cond (pos? AC) (/ AC-total AC)
          (pos? RC) (/ RC-total RC)
          (pos? NG) (/ NG-total NG)
          :else 0))

;;conform the col names and add computed fields for stats stuff.
;;maybe we only call this 1x up higher in the chain.
(defn stylize [d]
  (let [normalized (fn [f e d] (double (/ (+ f e)
                                          (if (pos? d) d 1))))
        lengths (->> (tc/rows d :as-maps)
                     (reduce (fn [acc {:keys [phase AC-total NG-total RC-total AC NG RC]}]
                               (update acc phase
                                       #(max (or % 0)
                                             (derive-length AC-total NG-total RC-total AC NG RC))))
                             {}))]
    (-> d
        (tc/map-columns  :phase-length [:phase] lengths)
        (map-columns* :RAFill    [:AC-fill] identity
                      :RCFill    [:RC-fill :NG-fill] dfn/+
                      :Demand    [:total-quantity]   identity
                      :RAExcess  [:AC-deployable]    identity
                      :RCExcess  [:NG-deployable :RC-deployable] dfn/+
                      :UnmetDemand [:RAFill :RCFill :Demand] normalized))))

;;ported from the goof shave chart stuff...
(defn adjust-demand [{:keys [UnmetPercent  RCunavailpercent]}]
  (let [overlap (if (<= UnmetPercent RCunavailpercent)
                  UnmetPercent
                  RCunavailpercent)]
    {:UnmetOverlapPercent       overlap
     :UnmetPercent     (- UnmetPercent overlap)
     :RCunavailpercent (- RCunavailpercent overlap)}))

;;how different is this to the BCD scripts?
;;fat is supposed to refer to bar width, which was controlled
;;somehow by str IIRC.  Some of this  is just junk now.
;;we just derive phase-length now. pretty trivial we just add phase-length
;;column.
;;we want to transform into normalized values.
;;For the bar chart, (RAFill + RAExcess)/Demand -> SupplyRA, RCFill + RCExcees -> SupplyRC
(defn fat-shave-data [d phase]
  (let [phased (by-phase d phase)
        pl     (-> (phased :phase-length) first)
        mean-t (fn [d k] (/ (dfn/mean (d k)) pl))]
    (-> phased
        (tc/group-by [:SRC :AC :NG :RC :phase])
        ;;aggregate automatically ungroups.
        (tc/aggregate {:Demand    (fn [{:keys [Demand]}] (/ (dfn/mean Demand) pl))
                       :SupplyRA  (fn [{:keys [RAFill RAExcess]}]
                                    (/ (+ (dfn/mean RAFill) (dfn/mean RAExcess)) pl))
                       :SupplyRC  (fn [{:keys [RCFill RCExcess]}]
                                    (/ (+ (dfn/mean RCFill) (dfn/mean RCExcess)) pl))
                       ;;I don't think we actually need means here....constant values by phase.
                       :RCTotal   (fn [{:keys [RC-total NG-total]}]
                                    (/ (+ (dfn/mean RC-total) (dfn/mean NG-total)) pl))
                       :phase-length (fn [_] pl)})
        (map-columns* :TotalSupply [:SupplyRA :SupplyRC] dfn/+)
        (tc/group-by [:SRC]) ;;it's possible this v fails if we dupes.
        (tc/select-rows (fn [{:keys [AC maxac]}] (= AC maxac))
                        {:pre {:maxac (fn [d] (apply dfn/max (d :AC)))}})
        (tc/ungroup)
        (map-columns* :UnmetDemand      [:Demand :TotalSupply] (fn [dem s] (max (- dem s) 0))
                      :RAUnavailable    [:AC :SupplyRA] (fn [ac supplyRA]  (max (- ac supplyRA) 0))
                      :RCUnavailable    [:NG :RC :SupplyRC]    (fn [ng rc supplyrc] (max (- (+ ng rc) supplyrc) 0))
                      :RApercent        [:SupplyRA :Demand]    safe-div
                      :RCpercent        [:SupplyRC :Demand]    safe-div
                      :UnmetPercent     [:UnmetDemand :Demand] safe-div
                      :RAunavailpercent [:RAUnavailable :Demand] safe-div
                      :RCunavailpercent [:RCUnavailable :Demand] safe-div
                      :Totalpercent     [:RApercent :RCpercent]   dfn/+)
        ;;tack on cols for UnmetOverlap Unmetpercent RCunavailpercent
        (tc/map-rows adjust-demand))))

;;visualization helper.  we compute the labels from the data and use
;;that for our text layer for pax.  We provide 2 arities: one allows
;;use to pass in STR value to multiply each compo inventory by.
;;The other assumes we are passing in precomputed compo-strenght
;;values, and assumes 1 for str to allow us to pass through totals
;;unaltered.  The former we use for pax-labels in the by-src plots,
;;and the latter is for aggregating to cross-branch totals.
(defn pax-label
  ([AC NG RC STR]
   (let [[ac ng rc] (mapv #(let [n (/ (* % STR) 1000.0)]
                             (cond (zero? %) "0K"
                                   (zero? (long n)) (format "%.1fK" n)
                                   :else (format "%dK" (long n)))) [AC NG RC])]
     (str "(" (s/join ", " [ac ng rc]) ")")))
  ([ACSTR NGSTR RCSTR] (pax-label ACSTR NGSTR RCSTR 1)))

;;combines the data from the fatshavechart and the unit-detail workbook.
;;we expect to provide an inner join on SRC, and bring in BRANCH, TITLE, and STR
(defn join-and-clean [fat unit-detail]
  (->> (-> (tc/inner-join fat unit-detail [:SRC])
           (tc/map-columns :PaxLabel [:AC :NG :RC :STR] pax-label)
           (tc/rows :as-maps))
       tc/dataset))

;;helper function to pull in our worksheet/table for SRC TITLE STR BRANCH info.
(defn read-unit-detail [path]
  (-> (tc/dataset path {:key-fn keyword})
      (tc/select-columns [:SRC :TITLE :STR :BRANCH])))

;;it's way better if the data is reshaped...
;;we want src, trend, qty
;;where trend
;;{"RA Supply" "RC Supply" "RC Unavailable As Portion of UnMet"
;; "Unmet Demand" "RC Unavailable Leftover From Unmet"}

;;generate our aggregated, detailed table of data by phase.
(defn phase-data [results unit-detail phase]
  (join-and-clean (fat-shave-data (stylize results) phase) unit-detail))

;;this helps us define a map that we can use to define a column value that
;;can provide the ordering for our color channel for our trends.
(def trend-order (-> [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                     (zipmap (range))))

;;reshapes our phase-data derived from marathon results into a dataset with a trend/value
;;column pair.  rolls percentages into trend/value.
(defn pivot-trend [ds]
  (-> (tc/pivot->longer ds [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                        {:target-columns :trend :value-column-name :value})
      (tc/drop-columns [:RApercent :RCpercent :UnmetPercent :RCunavailpercent :UnmetOverlapPercent])
      (tc/order-by [:SRC :phase :trend])
      (map-columns* :color-order [:trend] trend-order
                    :DemandMet   [:Totalpercent] (fn [e] (str "Demand Met: "
                                                              (format "%.0f" (* e 100)) "%")))))

;;We can move these elsewhere...
;;per https://groups.google.com/g/vega-js/c/_3JwxvraWCQ/m/WGERWhqfBgAJ
;;we can get fill patterns in.

;;These are hiccp-encoded SVG patterns that define "colors" we can reference when
;;using the vega SVG renderer.  It allows us to implement arbitrary pattern fills
;;for any color we plot (specifically the crosshatch fill from the legacy plot).
;;We can add new patterns here and they will be available from within our
;;vega plots as  "url(#some-id)".  For example, to use the pattern with the
;;id "yellow-crosshatch", we would refer to it from a vega spec as the
;;string  "url(#yellow-crosshatch)" .  SVG allows an additional layer of
;;customization over our aesthetics if we desire (in addition to custom patterns).
(def pats
  [:svg {:height "0" :width "0"}
   [:defs [:pattern {:id "circles-1" :patternUnits "userSpaceOnUse" :width "10" :height "10"}
           [:image {:href "data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSJ3aGl0ZSIgLz4KICA8Y2lyY2xlIGN4PSIxIiBjeT0iMSIgcj0iMSIgZmlsbD0iYmxhY2siLz4KPC9zdmc+"
                    :x "0" :y "0" :width "10" :height "10"}]]
    [:pattern {:id "diagonal-stripe-2" :patternUnits "userSpaceOnUse" :width "10" :height "10"}
     [:image {:href "data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSd3aGl0ZScvPgogIDxwYXRoIGQ9J00tMSwxIGwyLC0yCiAgICAgICAgICAgTTAsMTAgbDEwLC0xMAogICAgICAgICAgIE05LDExIGwyLC0yJyBzdHJva2U9J2JsYWNrJyBzdHJva2Utd2lkdGg9JzInLz4KPC9zdmc+"
              :x "0" :y "0" :width "10" :height "10"}]]
    [:pattern {:id "yellow-crosshatch"  :patternUnits "userSpaceOnUse" :width "8" :height "8"
               :patternTransform "rotate(45 0 0) scale(1.5,1.5)" }
     [:svg {:width "8" :height "8" }
      [:rect {:width "8" :height "8" :fill "#ffffb2" :stroke "#000000" :stroke-opacity "0.3"}
       [:path {:d "M0 0L8 8ZM8 0L0 8Z" :stroke-width "0.5"  :stroke "#000000"}]]]]]])

;;We can define custom labels in vega using a (json) map applied as a function looking
;;up datum.label as a key (via indexed lookup in js).  We can just use clojure
;;maps to to this, and build the desired json expression programmatically..

;;In a vega JSON spec, we might see custom labels defined by a json map with
;;an indexed lookup on datum encoding like:

;; {"RApercent":"RA Supply",
;;  "RCpercent":"RC Supply",
;;  "UnmetOverlapPercent":"RC Unavailable As Portion of Unmet",
;;  "UnmetPercent":"Unmet Demand",
;;  "RCunavailpercent":"RC Unavailable Leftover From Unmet"}[datum.label]

;;label-expr allows us to pass in clojure maps to derive a label expression
;;we can use to easily adjust the label aesthetics without having to
;;splice strings or munge json directly.

(defn label-expr
  "Given a map of {field label}, returns a json expr for vega - as a string
   - that will recompute/relabel the fields with the corresponding user
   defined labels.  This can then be used to define custom labels inside of
  encoding -> _ -> legend -> labelExpr.
  (-> {:a \"Trend A\" :b \"Trend B\"} label-expr)"
  [m]
  (-> m json/json-str (str  "[datum.label]")))

;;Our common label expression for showing our internal columns
;;as legacy legend entries.
(def labelexpr
 (-> {:RApercent "RA Supply"
      :RCpercent "RC Supply"
      :UnmetOverlapPercent "RC Unavailable As Portion of Unmet"
      :UnmetPercent "Unmet Demand"
      :RCunavailpercent  "RC Unavailable Leftover From Unmet"}
     json/json-str
     (str  "[datum.label]")))

;;This is our vega specification for the legacy shave chart implementation.
;;It is fundamentally a layered plot, in the Graphics of Grammar parlance.
;;It is a combination of a stacked bar chart, 2 text layers, and a rule layer,
;;all with a common x/y encoding (e.g. a shared access).
;;We use parameters via :params to allow quick changing of the specification.
;;This lets us reference the parameter values inside the specification elsewhere
;;as if they were just local variables.  We can then change aesthetic features
;;programmatically (like bard width and spacing) just using clojure data structure
;;transformations (maps and vectors).
(def shave-pat
  {:data {},
   :title {:text  "Aviation-Aggregated modeling Results as Percentages of Demand"
           :fontSize 28
           :subtitle "Conflict-Phase 3 Most Stressful Scenario"
           :subtitleFontSize 20}
   :config {:background "lightgrey"}
   :height 700
   :width 1500
   :autosize {:type "fit"
              :contains "padding"}
   ;;compute variable widths....
   :params [{:name "barwidth"   :value 20} ;;translate to 10 halfwidth...
            {:name "txt1offset" :value 15}
            {:name "txt2offset" :value 25}]
   :encoding
   {:x {:type "nominal", :field "TITLE" #_"SRC"
        :axis {:labels false :title nil}}
    :y {:type "quantitative", :aggregate "sum", :field "value", :stack "zero"
        :title "%Demand"
        :scale {:domain  [0.0 2.5]}
        :axis {:format ".0%" :labelFontSize 16
               :titleFontSize 16
               :values (vec (range 0.0 2.6 0.25))}}},
   :layer
   [{:mark {:type "bar" :binSpacing 22  :width {:expr "barwidth"}
            :clip true :stroke "black"},
     :encoding {:color {:type "nominal", :field "trend"
                        :legend {:direction "horizontal" :orient "bottom"
                                 :labelExpr labelexpr
                                 :labelLimit 0
                                 :title ""}
                        :scale {:domain [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                                :range  ["#bdd7ee" "#c6e0b4"  "url(#yellow-crosshatch)" "#ffffb2" "white"]}}
                :order {:field :color-order}}}
    ;;title
    {:mark {:type "text", :color "black", :dy {:expr "txt1offset"}, :dx 0 :angle -90 :align "left"},
     :encoding
     {;;:detail {:type "nominal", :field "TITLE"},
      :text    {:type "nominal", :field "TITLE"}
      :y  {:datum 0}}}
    ;;str
    {:mark {:type "text", :color "black", :dy {:expr "txt2offset"} , :dx 0 :angle -90 :align "left"},
     :encoding
     {;:detail {:type "nominal", :field "variety"},
      :text    {:type "nominal", :field "PaxLabel"}
      :y  {:datum 0}}}
    ;;demand met
    {:mark {:type "text", :color "black", :dy {:expr "txt1offset"} , :dx 0 :angle -90 :align "left"},
     :encoding
     {;;:detail {:type "nominal", :field "TITLE"},
      :text    {:type "nominal", :field "DemandMet"}
      :y  {:datum 1.5}}}
    ;;rule.
    {:mark "rule"
     :encoding {:x nil ;;this works but I'm not happy.
                :y {:datum 1.0}
                :color {:value "red"}}}
    ]})

;;We define a helper function to handle customizing the vega spec
;;for a specific set of categories, primarily controlling bar width and
;;spacing aesthetics, as well as title and subtitle.
(defn customize-spec [spec data & {:keys [title subtitle width keyfn]
                                   :or {title "The Title"
                                        subtitle "The SubTitle"
                                        width 1050
                                        keyfn :SRC}}]
  (let [[barwidth txt1offset txt2offset] (spec :params)
        width (or (spec :width) width)
        n (->> data (map keyfn) distinct count)
        k (/ width  (+ n 2)) ;;this is how we can divide the space.
        bar-width (- k 20) ;;need at least 10 for the labels.
        hw        (/ bar-width 2.0)
        txt1      (+ hw 7)
        txt2      (+ txt1 9)
        push (fn [m v] (assoc m :value v))
        params [(push barwidth (long bar-width))
                (push txt1offset (long txt1))
                (push txt2offset (long txt2))]]
    (-> spec
        (assoc-in [:data :values] data)
        (deep-merge {:title {:text title :subtitle subtitle}
                    :width width
                    :params [(push barwidth   (long bar-width))
                             (push txt1offset (long  txt1))
                             (push txt2offset (long txt2))]}))))

;;An interactive help function; given a sequence of maps corresponding
;;to our trend data, we can pop an interactive view of the shavechart.
(defn render-bars [data & {:keys [title subtitle]
                           :or {title "The Title"
                                subtitle "The SubTitle"}}]
  (let [spec (-> shave-pat
                 (customize-spec data :title title :subtitle subtitle)
                 (assoc-in [:data :values] data)
                 (merge {:title {:text title :subtitle subtitle}}))]
    (oz/view! [:div pats
               [:vega-lite  spec  { :renderer :svg}]])))


;;Within Branch SRC Shave Charts
;;==============================

;;Defines an Oz-compatible webpage that we can view.  We can
;;treat this as a hiccup component that wraps our chart and
;;renders it as an svg (with the requisite preamble to allow our
;;svg to have custom patterns defined).
(defn src-bar-chart [data & {:keys [title subtitle]
                            :or {title "The Title"
                                 subtitle "The SubTitle"}}]
  (let [spec (-> shave-pat
                 (customize-spec data :title title :subtitle subtitle))]
    [:div {:style {:width "100%"}}
     [:vega-lite  spec  { :renderer :svg}]]))

;;Defines a visual component that lays out multiple plots for rendering, one for
;;each branch.  Within the branch, we show a shavechart for each SRC.
(defn branch-charts
  ([data {:keys [title subtitle] :as opts}]
  (let [data (tc/map-columns data :SRC2 [:SRC] (fn [SRC] (subs SRC 0 2)))]
    [:div
     pats
     (for [[{:keys [BRANCH]}  src-data] (->> (tc/group-by data [:BRANCH #_:SRC2] {:result-type :as-map})
                                           (sort-by (comp :SRC2 first)))]
        (src-bar-chart (->   src-data pivot-trend records vec)
                       :title (str BRANCH "-" title )
                       :subtitle subtitle))]))
  ([data] (branch-charts data {:title "Aggregated Modeling Results as Percentages of Demand"
                               :subtitle "Conflict-Phase 3 Most Stressful Scenario"})))

;;Aggregated Branch Shave Charts
;;==============================

;;We need to slighty munge the data prior to a visualizing it.
;;Compared to the src-level view, we now want to compute total
;;strength and fill stats across a branch.
(defn agg-branch-data [ds]
  (-> ds
      (map-columns* :SRC2 [:SRC] (fn [SRC] (subs SRC 0 2))
                    :ACSTR [:AC :STR] dfn/*
                    :RCSTR [:RC :STR] dfn/*
                    :NGSTR [:NG :STR] dfn/*)
       (tc/group-by [:BRANCH :phase])
       (aggregate-columns*  {:RApercent dfn/mean
                             :RCpercent dfn/mean
                             :RAunavailpercent dfn/mean
                             :RCunavailpercent dfn/mean
                             :Totalpercent dfn/mean
                             :UnmetPercent dfn/mean
                             :UnmetOverlapPercent dfn/mean
                             :ACSTR dfn/sum
                             :RCSTR dfn/sum
                             :NGSTR dfn/sum})
       (tc/map-columns :PaxLabel [:ACSTR :RCSTR :NGSTR] (fn [ac rc ng] (pax-label ac rc ng)))))

;;Since we use a clojure map for our spec, we can trivially derive
;;new specs by messing with the map.
(def br-spec (-> shave-pat
                 (assoc-in [:encoding :x :field] "BRANCH")
                 (assoc-in [:layer 1 :encoding :text :field] "BRANCH")))

;;Another chart component that we can embed for viewing an svg plot in Oz
;;or elsewhere.  This one aggregates our data by branch and produces the
;;aggregate branch view.  It's very similar to the basic shave chart, but
;;the underlying basis for aggregation is BRANCH.
(defn branch-bar-chart [data & {:keys [title subtitle]
                                :or {title "The Title"
                                     subtitle "The SubTitle"}}]
  (let [spec (-> br-spec
                 (customize-spec data :title title :subtitle subtitle :keyfn :BRANCH))]
    [:div {:style {:width "100%"}}
     [:vega-lite  spec  { :renderer :svg}]]))

;;assuming we already have max results from our barchart data....
(defn agg-branch-charts
  ([data {:keys [title subtitle] :as opts}]
   (let [branches     (agg-branch-data data)]
     [:div
      pats
      (for [[{:keys [phase]} br-data] (tc/group-by branches  [:phase] {:result-type :as-map})]
        (branch-bar-chart (->   br-data pivot-trend records vec)
                          :title (str "All Branches" "-" title )
                          :subtitle (case phase
                                      "comp1" "Campaigning"
                                      subtitle)))]))
  ([data] (agg-branch-charts data {:title "Aggregated Modeling Results as Percentages of Demand"
                                   :subtitle ""})))

;;Emitting raster images
;;=====================
;;allows us to emit custom shapes/colors/patterns into stand-alone svg files.
;;necessary to allow us to use svg patterns for colors.

;;move to resources or inline.
(def pre "<svg class=\"marks\" width=\"1874\" height=\"799\" viewBox=\"0 0 1874 799\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n")

(defn inject-patterns [svg pats]
  (let [head (re-find #"<svg.+/xlink.+>" pre)]
    (clojure.string/replace svg head (str head (hc/html pats)))))

;;we'll try to include our hatched thing here..
(defn emit-bars [data & {:keys [title subtitle]
                          :or {title "The Title"
                               subtitle "The SubTitle"}}]
  (let [spec (-> shave-pat
                 (assoc-in [:data :values] data)
                 (merge {:title {:text title :subtitle subtitle}}))]
    (oz.headless/render spec "bars.png" :pre-raster (fn [svg] (inject-patterns svg pats)))))


;;campaigning/comp and branch views.


;;legacy bar chart data conversion
;;================================

;;if we have barchart data, that is equivalent to what we get out of phase-data.
;;we need to adjust the fields though.
(def bcd->shave {:dmetRA :RApercent
                 :dmetRC :RCpercent
                 :ACunavailable :RAunavailpercent
                 :RCunavailable :RCunavailpercent})

;;probably doesn't matter?
(def shave->bcd (into {} (map (fn [[k v]] [v k]) bcd->shave)))

;;this lets us adapt old bcd format into shavechart data. helpful for
;;transitioning. Note: we have cases where TotalPercent is 0. In these cases, we
;;want unmet precent to also be 0. This indicates no fill and no demand.
;;given a barchart dataset, and a unit-detail dataset, we can produce
;;the input necessary for our barchart plots.
(defn barchart->phasedata [ds detail]
  (let [minv (max-inventory ds)]
    (-> ds
        (tc/select-rows (fn [{:keys [SRC AC RC NG]}]
                          (let [r (minv SRC)]
                            (and (= (r :AC) AC)
                                 (= (r :RC) RC)
                                 (= (r :NG) NG)))))
        (tc/rename-columns  bcd->shave)
        (map-columns*  :Totalpercent [:RApercent :RCpercent] +
                       :UnmetPercent [:Totalpercent] (fn [x] (if (zero? x) x
                                                                 (max (- 1.0 x) 0))))
        (tc/map-rows adjust-demand)
        (join-and-clean detail))))

;;total percent RAPercent + RCPercent.  We filter scenario by max.
;;We have some cases where total percent is zero.
(defn max-by-scenario [ds]
  (let [flds (tc/column-names ds)]
    (-> ds
        (tc/group-by  [:SRC :AC :NG :RC :phase])
        (tc/order-by :Totalpercent :asc)
        (tc/select-rows [0])
        (tc/ungroup)
        (tc/select-columns flds)
        (tc/drop-columns [:Scenario]))))

(defn barchart->src-charts [ds detail]
  (-> ds
      (barchart->phasedata detail)
      max-by-scenario))

(comment
  
  ;;sample data
  (def dt
    (tc/dataset "../make-one-to-n/resources/results_no_truncation.txt"
                {:key-fn keyword :separator \tab}))

  (def unit-detail (read-unit-detail (io/file-path "~/SRC_STR_BRANCH.xlsx") #_"../make-one-to-n/resources/SRC_BASELINE.xlsx"))

  (def ph3 (phase-data dt unit-detail "phase3"))

  (render-bars  (->  ph3 pivot-trend records vec)
                :title "Aviation-Aggregated modeling Results as Percentages of Demand"
                :subtitle "Conflict-Phase 3 Most Stressful Scenario")

  (oz/view! (->  ph3 branch-charts))

  ;;actual rendering.
  ;;basic pipeline for munging legacy barchart data and turning it into shave charts.
  (def bcd (tc/dataset (io/file-path "~/bcd.txt") {:separator "\t" :key-fn keyword}))

  (def bcd2 (barchart->src-charts bcd unit-detail))
  (oz/view! (-> bcd2
                (tc/select-rows (fn [{:keys [phase]}] (= phase "phase3")))
                (branch-charts {:title "Aggregated Modeling Results as Percentages of Demand"
                                :subtitle "Conflict-Phase 3 Most Stressful Scenario"})))
  (oz/view! (-> bcd2
                (tc/select-rows (fn [{:keys [phase]}] (= phase "comp1")))
                (branch-charts {:title "Aggregated Modeling Results as Percentages of Demand"
                                :subtitle "Campaigning"})))

  (oz/view! (-> bcd2
                (agg-branch-charts {:title "Aggregated Modeling Results as Percentages of Demand"
                                    :subtitle "Most Stressful Scenario By Branch"})))
  )

;;possible convenience macros.
;; (mapping UnmetDemand   (max (- ?Demand ?TotalSupply) 0)
;;          RCUnavailable (max (- (+ ?NG ?RC) ?SupplyRC))
;;          RApercent         safe-div
;;          RCpercent         safe-div
;;          UnmetPercent      safe-div
;;          RCunavailpercent  safe-div
;;          Totalpercent      dfn/+

;; (mapping [RCUnavailable  (max (- (+ ?NG ?RC) ?SupplyRC))
;;           SupplyRA       (/ (+ (dfn/mean ?RAFill) (dfn/mean ?RAExcess)) pl)
;;           SupplyRC       (/ (+ (dfn/mean ?RCFill) (dfn/mean ?RCExcess)) pl)
;;           ;;I don't think we actually need means here....constant values by phase.
;;           RCTotal        (/ (+ (dfn/mean ?RC-total) (dfn/mean ?NG-total)) pl)
;;           phase-length    pl])

;; (defmacro deriving [bindings]
;;   {:RCUnavailable  (max (- (+ ?NG ?RC) ?SupplyRC))})
