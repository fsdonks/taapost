;;This is a port of the legacy n-list functionality from pandas to tablecloth.
;;We substantially change the computation by decoupling the generation of output
;;and not trying to retain the merged cell behavior from multi-indexed
;;dataframes.

;;Instead, we compute a simple "tidy" table, and then emit that as necessary.
(ns taapost.nlist
  (:require [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.dataset.reductions :as reds]
            [tech.v3.libs.fastexcel]
            [taapost.util :as u :refer [visualize]]
            [taapost.demandanalysis :as da]
            [taapost.iso :as iso]
            [spork.util.io :as io]
            [spork.util.excel.core :as xl]))

;;utils
;;=====

;;ref https://stackoverflow.com/questions/10751638/clojure-rounding-to-decimal-places
(defn round2
  "Round a double to the given precision (number of significant digits)"
  ^double [^long precision ^double d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn indices [xs] (zipmap xs (range)))
(defn min-max [xs]
  (reduce (fn [[l r] x]
            [(min (or l x) x) (max (or r x) x)])
          [nil nil] xs))

(def ^:dynamic *aggregate* reds/mean)

;;aux function to copy pandas...looks like we just drop out non-numerical.
(defn mean [ds]
  (tc/aggregate-columns ds :type/numerical  dfn/mean #_{:separate? false}))

;;this is a quick swap out to see if we get different results for our aggregate...
(defn naive-median ^double [xs]
  (let [n (count xs)
        sorted   (->> xs sort)
        ^objects
        arr (.array  ^clojure.lang.ArraySeq sorted)]
    (if (odd? n)
      (double (nth arr (inc (/ n 2))))
      (double (nth arr (/ n 2))))))

;;dupe from shavechart, maybe move to util...
(defn read-unit-detail [path]
  (-> (tc/dataset path {:key-fn keyword})
      (tc/select-columns [:SRC :TITLE :STR :BRANCH])))

;;exact median might be good, but I am uncertain about the amount of data we have to work with....
;;The gist is that we [probably] have all the data in memory as it stands though....
;;The only outliers would be for either very large rep runs, or full compo runs.....
;;We need to test the space efficiency for these cases....Might need to be smarter
;;about how we're messing with large datasets in that case.
#_
(defn median-reducer [colname] (reds/reducer->column-reducer dfn/median))

;;we can account for grouped data...
;;as pandas does natively.
;;do ungrouped version first...
(defn numeric-column-names [ds]
  (->> ds
       (keep (fn [[nm col]]
               (let [m (meta col)]
                 (when-not (#{:object :string :boolean} (m :datatype))
                   (m :name)))))))

;;can we get a simple median?

;;this is so much faster, like 500x.  We should always prefer to
;;group and aggregate at the same time if possible.....
(defn agg-by [col->reducer ds group-fields]
  (let [original  (tc/column-names ds)
        gf        (set group-fields)
        targets   (filter (complement gf) (numeric-column-names ds))
        fns       (->> targets (map (fn [k] [k (col->reducer k)])) (into {}))]
    (-> (reds/group-by-column-agg group-fields fns ds)
        (tc/reorder-columns original))))

(defn agg-mean [ds group-fields] (agg-by reds/mean ds group-fields))
;;probabilistic median for now.  We use built-ins.  Maybe we can reduce our noise
;;a bit.
(defn agg-median  [ds group-fields] (agg-by reds/prob-median ds group-fields))

;;helper to allow us to project aggregation strategies.
;;we could elevate this into a multimethod or some other registry at
;;some point.  For now, we just support 2 known methods to make it easier
;;to compare stuff.
(defn get-aggregate []
  (case *aggregate*
    :mean   reds/mean
    :median reds/prob-median
    (if (fn? *aggregate*)
      *aggregate*
      (throw (ex-info "unknown aggregate reducer" {:in *aggregate*})))))

(defn agg-dynamic
  "Wrapper for taapost.nlist/agg-by, infers aggregation strategy from the
   dynvar taapost.nlist/*aggregate*, where we expect the value to either be
   a keyword in #{:mean :median}, or a function.  If it's a function, it
   should conform to the standard for a custom reducer, such as
   tech.v3.dataset.reductions/prob-median or tech.v3.dataset.reductions/mean.
   Groups the dataset by group-fields, then aggregates all the numerical fields
   using whatever aggregation strategy is defined."
  [ds group-fields] (agg-by (get-aggregate) ds group-fields))


;;N List Computation
;;==================

;;output format

;;                         Demand Days
;;                         demand_met
;;                         excess_met
;;                         weight
;;                         dmet_times_weight
;;                         emet_times_weight X
;; OML SRC TITLE RA NG AR [comp1 comp2 phase1 phase2 phase3 phase4] Score Excess Demand_Total STR base_supply

;; combined
;; OML SRC2 TITLE RA Qty Most Stressed Score Excess STR


;;Quick overview of supply variation scoring to create an OML:
;;- We have multiple replications per design point, where a design point
;;  is [SRC AC NG RC].
;;  - this could involve variation across all compos, but we typically
;;    are only looking at AC in our deliverables for now.
;;- We have multiple phases per design, like [comp1 phase1 phas2 phase3 comp2]

;;For purposes of scoring, we want to look at each [SRC AC Phase] point, and
;;compute some notion of stress or performance.
;;The chosen metric is demand met and excess met.
;;  Relative to the demand, how much did with fill, and then how much did we have
;;  in excess?

;;Note all phases are created equal.  We have a mapping of phase-weights that
;;we should apply to the sample means for the performance measures for each phase.

;;Once we have weighted measures, we can then develop an order of merit by
;;computing a score (the sum of weighted demand met across phases), and a secondary
;;criteria for ordering (the weighted sum of excess demand met across phases).

;;Since we are dealing with multiple replications, we compute a sample mean
;;for these values.

;;We end up with an aggregated dataset where we can map each [SRC AC NG RC] to
;;a corresponding [Score Excess], or [AveragedWeightedDemandMet AverageWeightedExcess]

;;From here, we can order the [SRC AC NG RC] designs by [Score Excess] in descending order
;;to get an order of merit list for potential supply reductions.

;;We also want (for intuitive purposes) to ensure that our scores are
;;monotonically decreasing. That is, we don't want to see (even marginal)
;;increases in performance by decreasing supply. We typically accomplish this
;;with more replications or by smoothing the results to eliminate noise from
;;outliers. One typical cause is the notion of "excess" demand, which is driven
;;by available/not-deployed units. This can introduce variance due to the
;;dynamic availability of units and other timing phenomenon.

;;Each line in the OML represents a potential cut of 1 UIC from the supply.  We like
;;to present the resulting inventory as the inventory remaining if the cut is selected
;;(as opposed to the inventory prior to cutting).

;;The end result is a couple of different outputs.  We typically have multiple result sets
;;from experiments that we want to combine in some way (worst-case performance
;;for each design, e.g. min demand met, min excess type stuff).

;;Each the result sets gets its own OML table.  We then have a combined view (combining
;;by worse performance for each design), which we consolidate into a simplified "final"
;;OML, with just [SRC AC Score Excess]

;;computes a new column
(defn compute-excess [{:keys [NG-deployable AC-deployable RC-deployable total-quantity] :as in}]
  (tc/add-column in :excess-met (dfn// (dfn/+ NG-deployable AC-deployable RC-deployable) total-quantity)))


;; #compute % demand met (dmet) and % excess over the demand (emet)
;; #first by phase (use average group by with src, ac, phase)
;;tom - got bit by floating point zero comparisons a lot here, need to use clojure.core/zero?

;;this is mildly goofy. kind of maxing dataframe ops...

;;when there is no demand in a phase, dmet is 100%
;;When there is no demand in a phase, emet is the max emet across all SRCs and phases.
;;emet will be 0 because if there is no demand, we don't have a record.
(defn by-phase-percentages [res-df]
  (let [group-df  (-> res-df
                      (agg-dynamic [:SRC :AC :NG :RC :phase]) ;;we should be going off all compos...
                      (tc/map-columns :demand-met :float64 [:NG-fill :AC-fill :RC-fill :total-quantity]
                          (fn ^double [^double ng ^double ac ^double rc ^double total]
                            (if (zero? total) 1.0 (/ (+ ng ac rc) total)))))
        excess-df  (->  group-df (tc/select-rows #(not (zero? ^long (% :total-quantity)))) compute-excess)
        max-excess (->> (excess-df :excess-met)  (reduce dfn/max) double inc)]
    (tc/map-columns group-df :excess-met :float64 [:NG-deployable :AC-deployable :RC-deployable :total-quantity]
        (fn ^double [^double ng ^double ac ^double rc ^double total]
          (if (zero? total) max-excess (/ (+ ng ac rc) total))))))

(defn load-results [in]
  (cond (string? in) (-> in (tc/dataset {:separator "\t" :key-fn keyword}))
        (tc/dataset? in) in
        :else (throw (ex-info "expected file path or dataset" {:in in}))))

(defn results-by-phase [df] (-> df (agg-mean [:SRC :AC :phase])))

;;Make sure that scores are monotonically decreasing as inventory decreases
;;I think we can implement this more simply....We ignore all the writing bs.
;;Too much interweaving of IO (excel writing) for my taste.

;;compute score and excess from a path to results.txt
;;we want to collect sum of scores for excess and weighted.
(defn compute-scores [results phase-weights title-strength & {:keys [smooth demand-name]}]
   (-> results
       load-results
       (tc/select-rows (fn [{:keys [AC NG RC]}] (> (+ AC NG RC) 0)))
       by-phase-percentages
       (u/map-columns* :weight     [:phase] phase-weights
                       :d-weighted [:demand-met :weight]  *
                       :e-weighted [:excess-met :weight]  *)
       (tc/select-columns
        [:SRC :AC :NG :RC :phase :total-quantity :demand-met :excess-met :weight :d-weighted :e-weighted])
       (tc/rename-columns {:total-quantity :DemandDays})))

;;works but OBE.  This is a slow path.
#_
(defn consolidate-scores [ds]
  (-> ds
      (tc/group-by [:SRC :AC :NG :RC])
      (tc/aggregate {:Score #(-> % :d-weighted dfn/sum)
                     :Excess #(-> % :e-weighted dfn/sum)})
      (tc/order-by [:Score :Excess] :desc)))

;;same but way faster.
;;We can just use this to build lookup table we can join on after
;;we compute the spread jank.
;;computes our aggregated score/excess by design point.  Similar to what we
;;get on the final worksheet.
(defn consolidate-scores [ds]
  (-> (->> ds
           (reds/group-by-column-agg [:SRC :AC :NG :RC]
                                     {:Score  (reds/sum :d-weighted)
                                      :Excess (reds/sum :e-weighted)}))
      (tc/order-by [:Score :Excess] :desc)))

;;we can get closer to the exact python output with this...
;;This is like having the multi-index jank, or tuples for column
;;names in pandas...
;;we can use concat-value-with option to customize the resulting column names.
;;we typically want drop-missing? to be false since we lose entries most of the time
;;due to sparse data....

;;This seems really complicated for what we're after.  There might be a simpler way
;;to refactor this, or accomplish it at the dataset level.  Maybe column metadata
;;would work.
(def nested-cols [:DemandDays :demand-met :excess-met :weight :d-weighted :e-weighted])
(defn reorder-nested-cols [ds phase-weights]
  (let [colnames (tc/column-names ds)
        cnames (->> colnames
                    (map-indexed vector)
                    (filter (fn [[idx x]] (vector? x)))
                    (mapv   (fn [[idx x]] (vary-meta x assoc :idx idx))))
        phase-order (->> phase-weights keys (mapv keyword) indices)
        corder     (indices nested-cols)
        ordered-names (sort (fn [l r]
                              (let [res (compare (-> l first corder)
                                                 (-> r first corder))]
                                (if (not (zero? res)) res
                                    (compare (-> l second phase-order)
                                             (-> r second phase-order)))))
                            cnames)
        new-names  (vec (concat (filter #(not (vector? %)) colnames)
                                ordered-names))]
    (-> ds (tc/reorder-columns new-names))))

;;this is currently only necessary if we want to compute the "human readable" version
;;of worksheets from the legacy process.

;;It basically unrolls the DemandDays, demand-met, excess-met, weight,
;;d-weight, e-weight columns by phase.  So you get 24 extra columns.
(defn summary-cols [phase-weights]
  (->> (for [from [:e-weighted :d-weighted]]
         [from (vec (for [ph (map keyword (keys phase-weights))]
                      [from ph]))])
       (into {})))

(defn spread-metrics [ds phase-weights]
  (let [mcols (summary-cols phase-weights)]
    (-> ds
        (tc/pivot->wider :phase  nested-cols
           {:drop-missing? false :concat-value-with (fn [phase x] [ x (keyword phase)])})
        (reorder-nested-cols phase-weights)
        (tc/map-rows (fn [row]
                       {:TotalDemand (->> (mcols :DemandDays) (map #(get row % 0)) (reduce + 0))
                        :Score  (->> (mcols :d-weighted) (map #(get row % 0)) (reduce + 0))
                        :Excess (->> (mcols :e-weighted) (map #(get row % 0)) (reduce + 0))})))))


;;sketching out our skeleton here...

;;for each result in results-map
;;  compute (consolidated) scores for each entry (path) in the results-map
;;  - this requires leveraging peak-max to determine the max demand, which informs our scoring and eventual combination step..
;;  add spread-metrics for each results map to create human-readable workbooks
;;  join src_str_branch by src
;;combine the results thusly

;;  for each [src ac rc ng] design
;;  where more than one result exists, pick the one with the most stressful demand.
;;    where most stressful demand is defined as
;;      the lowest score, excess, most peak demand, total demand days.

;;we need to add peaks.  That would make it easy to add to our sorting criteria...
;;If we have [Score Excess TotalDemand Peak] then
;;we just sort (within a design) by [-Score -Excess Peak TotalDemand]

;;need to append peak to the record.
;;assume peak maps [src demand-name] -> peak demand quantity.
;;we can just add the peak column and use it for sorting...

;;we really want to sort among a batch of records though....
;;not sure if we can make the reducer work out of the box.
;;We can aggregate a new dataset

;;Assume peak is already present in results.

;;This is just invoking taa.demandanalysis/maxes->xlsx!
;;we can do that in-memory though.
;;We parse a map of {Scenario M4-workbook-path}
;;into a lookup table of {SRC {:keys [Scenario Peak]}}
(defn m4books->peak-lookup
  "Given a map of {Scenario m4-workbook-path}, and a period that
   maps to one of the periods in the workbook/PeriodRecords, e.g.
   \"Surge\", we generate a lookup table of {SRC {:keys [Scenario Peak]}},
   where we can project each SRC onto the Scenario that yielded the
   highest peak demand."
  ([workbook-map period]
   (->> (for [[SRC recs]  (->> (-> workbook-map
                                   (da/max-demand period)
                                   da/maxes->records)
                               (group-by :SRC))]
          (let [{:keys [SRC demand_name peak]} (->> recs (reduce (partial max-key :peak)))]
            [SRC {:Scenario demand_name :Peak peak}]))
        (into {})))
  ([workbook-map] (m4books->peak-lookup workbook-map "Surge")))

;;we can see if group-by and sorting is slow.  for now, just do the
;;naive version?

;;for combining, we can just concat the datasets, group-by [src ac rc ng], sort by
;;stress, pick the first result....

;;Alternately, just reduce over the dataset and collect the min by that comparison criteria.
;;E.g. find the minimum performance.

(def score-key (juxt :Score :Excess (comp - :TotalDemand) (comp - :Peak)))
;;this won't work for our monotone signals....
;;Worst case, we may have scores interleaving.
;;so instead of naive sorting, we need to do a group-by, or filter the remaining
;;based on the first chosen?
;;legacy impl goes with first chosen.
(defn most-stressful [d]
  (->> (for [[{:keys [SRC AC RC NG]} data] (tc/group-by d [:SRC :AC :RC :NG] {:result-type :as-map})]
         (let [scored (-> data  (tc/order-by score-key))
               sc     (-> scored
                          (tc/select-rows [0])
                          :Scenario
                          first)]
           (tc/select-rows scored (fn [{:keys [Scenario]}] (= Scenario sc)))))
       (apply tc/concat)))

;;drop all the intermediate computed fields...
(defn narrow [ds]
  (let [cnames (->> (tc/column-names ds) (filterv (complement vector?)))]
    (-> ds
        (tc/select-columns cnames))))

;;consistent sorting critera for datasets.
(defn order-scores [d]
  (tc/order-by d [:RemainingScore :RemainingExcess] :desc))

;;take a dataset with both scenarios, consolidate s.t. there is only 1 record for
;;each [SRC AC RC NG] mixture, which is the "most stressful", drop the intermediate
;;column names

;;TBD we get a problem with the sample data with 19539RC00, it's picking from both,
;;which screws the monotonicity.  We need to pick one and stick with it.
(defn combine [d]
  (-> d
      most-stressful
      (tc/drop-columns [:Scenario])
      order-scores
      narrow))
;;For the final pass (emission), we can traverse the cols that are vector names,
;;concat into a nice string name, then dump to excel or tsv as normal.


;;beautification of results.
;;we just go through and concat the vector column names for now...
;;e.g. [:phase1 :e-met] -> "phase1-emet"
(defn simple-names [d]
  (let [cnames (->> d
                    tc/column-names
                    (map (fn [x] (if (vector? x)
                                   (->> x (map name) (clojure.string/join "-"))
                                   (name x)))))]
    (tc/rename-columns d (zipmap (tc/column-names d) cnames))))

;;we add a little bias into the ordered resulting score to ensure
;;we have decreasing scores by AC compo.
;;This lets us bake some order into the isotonic regression, where
;;we may end up with duplicate values.  Having a little noise shouldn't
;;affect the spirit of the result, but it helps us order things.
;;More importantly, we document the bias so the smoothed score can
;;be inferred if one is so inclined.

(defn add-bias [d]
  (let [scores     (d :Score)
        bias       (mapv #(* % 0.001) (range 0 (count scores)))
        new-scores (dfn/+ scores bias)]
    (-> d
        (tc/add-or-replace-columns {:Score new-scores
                                    :Bias  bias}))))

;;given a dataset (subgroup) of a single scenario's SRC score data,
;;we apply isotonic regression to smooth the original score.
(defn smooth-scores [d]
  (-> d
      (tc/order-by [:AC] :asc)
      (tc/add-column :ScoreRaw (d :Score))
      (tc/add-or-replace-column :Score (->> (vec (d :Score))
                                            iso/iso))
      add-bias
      (tc/order-by [:AC] :desc)))

;;I think instead of replacing the scores, we just add a couple of columns.
;;We want a 2d index, of {SRC {AC LowerScore}}
;;-1.1 is a placeholder.  We use that to indicate complete cut.
(defn smooth-and-drop-scores [d]
  (->> (for [[{:keys [SRC]} data] (tc/group-by d [:SRC] {:result-type :as-map})]
         (let [data (smooth-scores data)
               score-index (->> data
                                u/records
                                (reduce (fn [acc {:keys [AC Score Excess]}]
                                          (assoc acc AC [Score Excess])) {}))
               dropped-index (->> (keys score-index)
                                  (map (fn [AC]
                                         [AC (get score-index (dec AC) [-1.1 -1.1])])) ;;we tack on artifiical negative scores.
                                  (into {}))]
           (-> data
               (tc/map-rows
                (fn [{:keys [AC]}]
                  (let [[s e] (dropped-index AC)]
                    {:Remaining      (dec AC)
                     :RemainingScore  s
                     :RemainingExcess e})))
               (tc/select-rows (fn [{:keys [Remaining]}] (>=  Remaining 0))))))
       (apply tc/concat)))

(defn round-cols [ds]
  (let [rounder #(if % (round2 5 %) %)]
    (tc/update-columns ds :type/float #(mapv rounder %))))

;;Right now, we focus on AC cuts....
;;It's possible there's no AC to begin with....so we only have 1

(defn non-mono-SRCs [d]
  (->> (for [[{:keys [SRC]} data] (tc/group-by d [:SRC] {:result-type :as-map})]
         (when-not (->> data
                        :Score
                        (reduce (fn [l r] (if (>= l r) r (reduced false))) (first d))
                        boolean)
           SRC))
       vec))

;;sanity check to ensure we're not producing counter intuitive results.
(defn mono-check [d]
  (let [srcs (non-mono-SRCs d)]
    (if (empty? srcs) d
        (do (println [:WARNING "Non Montonic Scores Detected!" {:SRCs srcs}])
            d))))

;;We want to handle our smoothing/offset/
(defn make-one-n [results-map m4-workbooks out-root phase-weights one-n-name src-title-str-path {:keys [smooth aggregate] :as opts}]
  (let [title-strength (some-> src-title-str-path u/as-dataset) ;;for now...
        peaks          (m4books->peak-lookup m4-workbooks)  ;;we just need the demand records for each result....
        consolidated   (binding [*aggregate* (or aggregate *aggregate*)]
                         (->> (for [[k path] results-map]
                                (let [in           (-> path u/as-dataset)
                                      scores       (-> in (compute-scores phase-weights title-strength))
                                      wide         (-> scores
                                                       (spread-metrics phase-weights)
                                                       (tc/add-columns {:Scenario k})
                                                       smooth-and-drop-scores)]
                                  wide))
                              (apply tc/concat)))
        scenarios (keys results-map) ;;same as Scenario field.
        combined  (-> consolidated
                      (tc/map-rows (fn [{:keys [SRC]}]
                                     (let [{:keys [Scenario Peak]} (peaks SRC)]
                                       {:Peak Peak
                                        :Most-Stressful Scenario})))
                      combine
                      (tc/rename-columns name)
                      round-cols
                      mono-check)]
    (->> (for [s scenarios]
           [s (-> consolidated
                  (tc/select-rows (fn [{:keys [Scenario]}] (= Scenario s)))
                  order-scores
                  simple-names
                  round-cols
                  mono-check)])
         (into {:Combined combined})
         (xl/tables->xlsx (io/file-path out-root (str one-n-name ".xlsx"))))))

(comment
  (def dt (-> (io/file-path "~/repos/make-one-to-n/resources/results.txt")
              (tc/dataset {:separator "\t" :key-fn keyword})))
#_
  (read-unit-detail (io/file-path "~/SRC_STR_BRANCH.xlsx"))

  (def phase-weights
    {"comp1" 0.1
     "phase1" 0.1
     "phase2" 0.1
     "phase3" 0.25
     "phase4" 0.1
     "comp2" 0.1})
  #_
  (compute-scores dt phase-weights nil)

  (def results-map {"A" "~/repos/make-one-to-n/resources/results.txt"
                    "B" "~/repos/make-one-to-n/resources/results.txt"})
  (def m4-books {"A" "A.xlsx"
                 "B" "B.xlsx"})

  (def res (make-one-n results-map m4-books "."  phase-weights "one_n"
                       "../make-one-to-n/resources/SRC_BASELINE.xlsx" {}))
  ;;dump results to a file...
  #_
  (->> (-> res simple-names  (tc/rename-columns (fn [k] (name k))))
       (spork.util.excel.core/table->xlsx "res.xlsx" "results"))

  )

;; #So far, we've had an agreed upon weighting for each phase of the demand, and
;; #now that we're weighting results across multiple Marathon runs, we want to say
;; #demand c has 75% weight and demand A has 25% weight and then multiple those
;; #weights by the weight for each phase.
;; #results_weight looks like {demand_A : .25, demand_C : .75}
;; #phase_breakout looks like {comp: 75, phase_1 : .06, phase_2: .19}
;; #there might be cases where we don't want to concat the run name to the phase
;; #like if we only have one entry in the results_map.

;; def split_run_weights(results_map, results_weights, phase_breakout, 
;;                       concat_run: bool=True):
;;     all_weights={}
;;     for demand_name in results_map:
;;         for phase in phase_breakout:
;;             if concat_run:
;;                 k=phase + '-' + demand_name
;;             else:
;;                 k=phase
;;             all_weights[k]= phase_breakout[phase]*results_weights[demand_name]
;;     return all_weights
