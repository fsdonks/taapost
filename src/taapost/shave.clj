;;shave chart templates
(ns taapost.shave
  (:require [clojure.walk :as w]
            [oz [core :as oz] [headless :as h]]
            [scicloj.tableplot.v1.hanami :as hanami]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.libs.fastexcel]
            [clojure.string :as s]))

(defn unjson [in]
  (w/postwalk
   (fn [frm]
     (if (map? frm)
       (zipmap (map keyword (keys frm)) (vals frm))
       frm)) in))

(defn records [ds] (tc/rows ds :as-maps))

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

;;we just derive phase-length now. pretty trivial we just add phase-length
;;column.

;;preclude divide by zero errors.
(defn safe-div [x y] (if (zero? y) 0 (dfn// x y)))
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
                      :RCUnavailable    [:NG :RC :SupplyRC]    (fn [ng rc supplyrc] (max (- (+ ng rc) supplyrc)))
                      :RApercent        [:SupplyRA :Demand]    safe-div
                      :RCpercent        [:SupplyRC :Demand]    safe-div
                      :UnmetPercent     [:UnmetDemand :Demand] safe-div
                      :RCunavailpercent [:RCUnavailable :Demand] safe-div
                      :Totalpercent     [:RApercent :RCpercent]   dfn/+)
        ;;tack on cols for UnmetOverlap Unmetpercent RCunavailpercent
        (tc/map-rows adjust-demand))))

(defn round-prod [l r]  (dfn/round (dfn/* l r)))
;;why do we need to mult by str here?
;;TODO - snuff this out...
(def str-flds [:Demand :SupplyRA :SupplyRC :TotalSupply :UnmetDemand
               :RCTotal :RCUnavailable])

(defn pax-label  [AC NG RC STR]
  (let [[ac ng rc] (mapv #(let [n (/ (* % STR) 1000.0)]
                            (cond (zero? %) "0K"
                                  (zero? (long n)) (format "%.1fK" n)
                                  :else (format "%dK" (long n)))) [AC NG RC])]
    (str "(" (s/join ", " [ac ng rc]) ")")))

(defn join-and-clean [fat unit-detail]
  (->> (-> (tc/inner-join fat unit-detail [:SRC])
           (tc/map-columns :PaxLabel [:AC :NG :RC :STR] pax-label)
           (tc/rows :as-maps))
       (map (fn [{:keys [STR] :as r}]
              (reduce (fn [acc k] (update acc k round-prod STR)) r str-flds))) ;;may not need this!
       tc/dataset))

;;we want to transform into normalized values.
;;For the bar chart, (RAFill + RAExcess)/Demand -> RASupply, RCFill + RCExcees -> RCSupply
(defn read-unit-detail [path]
  (-> (tc/dataset path {:key-fn keyword})
      (tc/select-columns [:SRC :TITLE :STR])))

;;it's way better if the data is reshaped...
;;we want src, trend, qty
;;where trend
;;{"RA Supply" "RC Supply" "RC Unavailable As Portion of UnMet"
;; "Unmet Demand" "RC Unavailable Leftover From Unmet"}

(defn phase-data [results unit-detail phase]
  (join-and-clean (fat-shave-data (stylize results) phase) unit-detail))

(def trend-order (-> [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                     (zipmap (range))))

(defn pivot-trend [ds]
  (-> (tc/pivot->longer ds [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                        {:target-columns :trend :value-column-name :value})
      (tc/drop-columns [:RApercent :RCpercent :UnmetPercent :RCunavailpercent :UnmetOverlapPercent])
      (tc/order-by [:SRC :phase :trend])
      (map-columns* :color-order [:trend] trend-order
                    :DemandMet   [:Totalpercent] (fn [e] (str "Demand Met: "
                                                              (format "%.0f" (* e 100)) "%")))))

;;Where does max demand factor in?  I know it matters for 1-n.
;;I think we pick the max for bar charts too.

(def shave-base
  {:data {},
   :title {:text  "Aviation-Aggregated modeling Results as Percentages of Demand"
           :subtitle "Conflict-Phase 3 Most Stressful Scenario"}
   :config {:background "lightgrey"}
   :height 700
   :width 1800
   :encoding
   {:x {:type "nominal", :field "SRC"
        :axis {:labels false :title nil}}
    :y {:type "quantitative", :aggregate "sum", :field "value", :stack "zero"
        :title "%Demand"
        :scale {:domain  [0.0 2.5]}
        :axis {:format ".0%"}}},
   :layer
   [{:mark {:type "bar" :binSpacing 20 :width 20 :clip true :stroke "black"},
     :encoding {:color {:type "nominal", :field "trend"
                        :legend {:direction "horizontal"
                                 :orient "bottom"}
                        :scale {:domain [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                                :range  ["#bdd7ee" "#c6e0b4" "#ffffef" #_"#99996a" "#ffffb2" "white"]}}
                :order {:field :color-order}}}
    ;;title
    {:mark {:type "text", :color "black", :dy 15, :dx 0 :angle -90 :align "left"},
     :encoding
     {;;:detail {:type "nominal", :field "TITLE"},
      :text    {:type "nominal", :field "TITLE"}
      :y  {:datum 0}}}
    ;;str
    {:mark {:type "text", :color "black", :dy 25, :dx 0 :angle -90 :align "left"},
     :encoding
     {;:detail {:type "nominal", :field "variety"},
      :text    {:type "nominal", :field "PaxLabel"}
      :y  {:datum 0}}}
    ;;demand met
    {:mark {:type "text", :color "black", :dy 15, :dx 0 :angle -90 :align "left"},
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

(defn render-bars [data & {:keys [title subtitle]
                           :or {title "The Title"
                                subtitle "The SubTitle"}}]
  (let [spec (-> shave-base
                 (assoc-in [:data :values] data)
                 (merge {:title {:text title :subtitle subtitle}}))]
    (oz/view! [:vega-lite spec])))

;;campaigning/comp and branch views.

(comment
  ;;sample data
  (def dt
    (tc/dataset "../make-one-to-n/resources/results_no_truncation.txt"
                {:key-fn keyword :separator \tab}))

  (def unit-detail (read-unit-detail "../make-one-to-n/resources/SRC_BASELINE.xlsx"))

  (def ph3 (phase-data dt unit-detail "phase3"))

  (render-bars  (->  ph3 pivot-trend records vec)
      :title "Aviation-Aggregated modeling Results as Percentages of Demand"
      :subtitle "Conflict-Phase 3 Most Stressful Scenario")

  (render-bars2  (->  ph3 pivot-trend records vec)
      :title "Aviation-Aggregated modeling Results as Percentages of Demand"
      :subtitle "Conflict-Phase 3 Most Stressful Scenario"))

;;per https://groups.google.com/g/vega-js/c/_3JwxvraWCQ/m/WGERWhqfBgAJ
;;we can get fill patterns in.
(def pats
  [:svg {:height "0" :width "0"}
   [:defs [:pattern {:id "circles-1" :patternUnits "userSpaceOnUse" :width "10" :height "10"}
           [:image {:href "data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSJ3aGl0ZSIgLz4KICA8Y2lyY2xlIGN4PSIxIiBjeT0iMSIgcj0iMSIgZmlsbD0iYmxhY2siLz4KPC9zdmc+"
                    :x "0" :y "0" :width "10" :height "10"}]]
    [:pattern {:id "diagonal-stripe-2" :patternUnits "userSpaceOnUse" :width "10" :height "10"}
     [:image {:href "data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSd3aGl0ZScvPgogIDxwYXRoIGQ9J00tMSwxIGwyLC0yCiAgICAgICAgICAgTTAsMTAgbDEwLC0xMAogICAgICAgICAgIE05LDExIGwyLC0yJyBzdHJva2U9J2JsYWNrJyBzdHJva2Utd2lkdGg9JzInLz4KPC9zdmc+"
              :x "0" :y "0" :width "10" :height "10"}]]
    [:pattern {:id "yellow-crosshatch"  :patternUnits "userSpaceOnUse" :width "8" :height "8"}
     [:svg {:width "8" :height "8"}
      [:rect {:width "8" :height "8" :fill "#ffffb2" :stroke "#000000" }
       [:path {:d "M0 0L8 8ZM8 0L0 8Z" :stroke-width "0.5"  :stroke "#000000"}]]]]]])

(def shave-pat
  {:data {},
   :title {:text  "Aviation-Aggregated modeling Results as Percentages of Demand"
           :subtitle "Conflict-Phase 3 Most Stressful Scenario"}
   :config {:background "lightgrey"}
   :height 700
   :width 1800
   :encoding
   {:x {:type "nominal", :field "SRC"
        :axis {:labels false :title nil}}
    :y {:type "quantitative", :aggregate "sum", :field "value", :stack "zero"
        :title "%Demand"
        :scale {:domain  [0.0 2.5]}
        :axis {:format ".0%"}}},
   :layer
   [{:mark {:type "bar" :binSpacing 20 :width 20 :clip true :stroke "black"},
     :encoding {:color {:type "nominal", :field "trend"
                        :legend {:direction "horizontal" :orient "bottom"}
                        :scale {:domain [:RApercent :RCpercent :UnmetOverlapPercent :UnmetPercent :RCunavailpercent]
                                :range  ["#bdd7ee" "#c6e0b4"  "url(#yellow-crosshatch)" "#ffffb2" "white"]}}
                :order {:field :color-order}}}
    ;;title
    {:mark {:type "text", :color "black", :dy 15, :dx 0 :angle -90 :align "left"},
     :encoding
     {;;:detail {:type "nominal", :field "TITLE"},
      :text    {:type "nominal", :field "TITLE"}
      :y  {:datum 0}}}
    ;;str
    {:mark {:type "text", :color "black", :dy 25, :dx 0 :angle -90 :align "left"},
     :encoding
     {;:detail {:type "nominal", :field "variety"},
      :text    {:type "nominal", :field "PaxLabel"}
      :y  {:datum 0}}}
    ;;demand met
    {:mark {:type "text", :color "black", :dy 15, :dx 0 :angle -90 :align "left"},
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

(defn render-bars2 [data & {:keys [title subtitle]
                           :or {title "The Title"
                                subtitle "The SubTitle"}}]
  (let [spec (-> shave-pat
                 (assoc-in [:data :values] data)
                 (merge {:title {:text title :subtitle subtitle}}))]
    (oz/view! [:div pats
               [:vega-lite  spec  { :renderer :svg}]])))

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

;;campaign availability will be entirely a
;;function of RC readiness....
;;if we count excess demand...assume uniform distribution
(comment
  (defn init-supply [ac rc]
    {:ac (vec (repeatedly ac #(rand-int 960)))
     :rc (vec (repeatedly rc #(rand-int 2010)))})

  (defn advance [ct clength t]
    (mod (+ ct t) clength))

  (defn supply-at [{:keys [ac rc]} t]
    {:ac (->> ac (mapv (fn [ct] (advance ct 960 t))))
     :rc (->> rc (mapv (fn [ct] (advance ct 2010 t))))}))

;;transform

