;;This is a port of the legacy n-list functionality from
;;pandas to tablecloth.  We substantially change the
;;computation by decoupling the generation of output
;;and not trying to retain the merged cell behavior
;;from multi-indexed dataframes.

;;Instead, we compute a simple "tidy" table,
;;and then emit that as necessary.
(ns taapost.nlist
  (:require [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [taapost.util :refer [visualize]]
            [spork.util.io :as io]))
;;aux function to copy pandas...looks like we just drop out non-numerical.
(defn mean [ds]
  (tc/aggregate-columns ds :type/numerical  dfn/mean #_{:separate? false}))

;;non-broadcasting where, per craig's usage. simple replacement.
(defn where [xs pred v else]
  (tech.v3.datatype/emap (fn [x] (if (pred x) v else)) nil xs))

#_
(defmacro derive-column
  ([df colname coltype cols body]
   (let [vars (mapv name cols)]
     `(tc/map-columns ~colname ~coltype [~@cols]
            (fn [~@vars]
              ~body))))
  ([df colname cols body]
   (let [vars (mapv name cols)]
     `(tc/map-columns ~colname [~@cols]
                      (fn [~@vars]
                        ~body)))))

;; #!/usr/bin/env python
;; # coding: utf-8

;; # In[ ]:

;;utils, maybe obe

;; #given an ordered list of initial columns, put the rest of the columns in the dataframe at the end
;; def reorder_columns(order, df):
;;     cols=[c for c in order if c in df] + [c for c in df if c not in order]
;;     return df[cols]

;; def ac_not_sorted(group):
;;     return not all(x>=y for x, y in zip(group[('AC', '')], group[('AC', '')].iloc[1:]))

;; def add_smoothed(group, col_name, new_name):
;;     col=[i for i in range(min(group[col_name]), max(group[col_name])+1)]
;;     col.reverse()
;;     group[new_name]=col
;;     return group

;; # # TAA Post Processing

;; # ## Output Checking

;; # ### Standard Capacity Analysis Run with Default Initial Conditions


;;might not be 1:1 translation though, can't verify pandas output.
;; (defn check-rand-results []
;;   (let [ds (-> (io/file-path "./resources" "results.txt")
;;                (tc/dataset {:separator "\t" :key-fn keyword}))]
;;     (-> (tc/group-by ds [:SRC :AC])
;;         (tc/add-columns {:row-count tc/row-count
;;                          :group (fn [ds] (str [(-> ds :SRC first) (-> ds :AC first)]))}) ;;group is janky here.
;;         tc/ungroup
;;         (tc/select [:SRC :AC :rep-seed :row-count :group] (comp #(not= 12 %) :row-count)))))

;; # ## Post Processing

;; # We'd like to compute Score and Excess for each \[SRC, AC\] tuple.  
;; # 
;; # First, average NG fill, then average RC fill, then average NG fill, then sum and divide by demand for Score (note that fill is fill from demandtrends and NOT just deployed like the field was renamed in 2327)
;; # Excess is sum of available for each component divided by demand

;; # In[ ]:


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

;; dmet='demand_met'
;; emet='excess_met'
(def dmet :demand-met)
(def emet :excess-met)


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

;;We also want (for intuitive purposes) to ensure that our scores are monotonically
;;decreasing.  That is, we don't want to see increases in performance by decreasing
;;supply.  We typically accomplish this with more replications or by smoothing the results
;;to eliminate noise from outliers.  One typical cause is the notion of "excess" demand,
;;which is driven by available/not-deployed units.  This can introduce variance due
;;to the dynamic availability of units and other timing phenomenon.

;;Each line in the OML represents a potential cut of 1 UIC from the supply.  We like
;;to present the resulting inventory as the inventory remaining if the cut is selected
;;(as opposed to the inventory prior to cutting).

;;The end result is a couple of different outputs.  We typically have multiple result sets
;;from experiments that we want to combine in some way (effecting worst-case performance
;;for each design, e.g. min demand met, min excess type stuff).

;;Each the result sets gets its own OML table.  We then have a combined view (combining
;;by worse performance for each design), which we consolidate into a simplified "final"
;;OML, with just [SRC AC Score Excess ]

;;computes a new column
(defn compute-excess [{:keys [NG-deployable AC-deployable RC-deployable total-quantity] :as in}]
  (tc/add-column in emet (dfn// (dfn/+ NG-deployable AC-deployable RC-deployable) total-quantity))


;; #compute % demand met (dmet) and % excess over the demand (emet)
;; #first by phase (use average group by with src, ac, phase)
;;tom - got bit by floating point zero comparisons a lot here, need to use clojure.core/zero?

;;this is mildly goofy.  kind of maxing dataframe ops...

;;when there is no demand in a phase, dmet is 100%
;;When there is no demand in a phase, emet is the max emet across all SRCs and phases.
;;emet will be 0 because if there is no demand, we don't have a record.
(defn by-phase-percentages [res-df]
  (let [group-df  (-> (tc/group-by res-df [:SRC :AC :phase])
                      (mean)
                      (tc/map-columns dmet :float64 [:NG-fill :AC-fill :RC-fill :total-quantity]
                         (fn ^double [^double ng ^double ac ^double rc ^double total]  (if (zero? total) 1.0 (/ (+ ng ac rc) total)))))
        excess-df  (->  group-df (tc/select-rows #(not (zero? (% :total-quantity)))) compute-excess)
        max-excess (->> (excess-df emet)  (reduce dfn/max) inc)]
    (tc/map-columns group-df emet :float64 [:NG-deployable :AC-deployable :RC-deployable :total-quantity]
      (fn ^double [^double ng ^double ac ^double rc ^double total]   (if (zero? total) max-excess (/ (+ ng ac rc) total))))))

(defn results-by-phase [df]  (-> df (tc/group-by [:SRC :AC :phase]) mean))

;; d_weighted = 'dmet_times_weight'
;; e_weighted = 'emet_times_weight'
;; #dmet_sum='weighted_dmet_sum'
;; #emet_sum='weighted_emet_sum'
;; dmet_sum=''
;; emet_sum=''

(defn load-results [in]
  (cond (string? in) (-> in (tc/dataset {:separator "\t" :key-fn keyword}))
        (tc/dataset? in) in
        :else (throw (ex-info "expected file path or dataset" {:in in}))))

;;this seems wrong....how are the entries going to be the same size?
;;maybe it's guaranteed.
(defn add-smoothed [gr col-name new-name]
  (let [from (gr col-name)
        [mn mx] (reduce (fn [[l r] x]
                          [(min (or l x) x) (max (or r x) x)])
                        [nil nil] from)]
    (tc/add-column gr new-name (range mx (dec mn) -1))))

;;Make sure that scores are monotonically decreasing as inventory decreases
;;I think we can implement this more simply....We ignore all the writing bs.
;;Too much interweaving of IO (excel writing) for my taste.

;;compute score and excess from a path to results.txt
(defn compute-scores [results phase-weights title-strength & {:keys [smooth demand-name]}]
  (let [df (load-results results)
        ;;     df= df[(df[['AC', 'NG', 'RC']] == 0).all(axis=1)==False]
        ;;     #sometimes all inventory was equal to 0, but we shouldn't have that. 
        ;;     #We should have all phases if all inventory ==0

        ]
    ))

;; def compute_scores(results_path, phase_weights, title_strength, smooth: bool, demand_name, order_writer):
;;     df=load_results(results_path)
;;     #sometimes all inventory was equal to 0, but we shouldn't have that. 
;;     #We should have all phases if all inventory ==0
;;     df= df[(df[['AC', 'NG', 'RC']] == 0).all(axis=1)==False]
;;     scores = by_phase_percentages(df)
;;     scores['weight']=scores['phase'].map(phase_weights)
;;     scores[d_weighted]=scores[dmet]*scores['weight']
;;     scores[e_weighted]=scores[emet]*scores['weight']
;;     res = results_by_phase(scores[['SRC', 'AC', 'NG', 'RC', 
;;                                    'phase', 'total-quantity', dmet, emet, 
;;                                    'weight', d_weighted,
;;                                   e_weighted]])
;;     res[('Score', dmet_sum)]=res.iloc[:, res.columns.get_level_values(0)==d_weighted].sum(axis=1)
;;     res[('Excess', emet_sum)]=res.iloc[:, res.columns.get_level_values(0)==e_weighted].sum(axis=1)
;;     res[('Demand_Total', '')]=res.iloc[:, res.columns.get_level_values(0)=='total-quantity'].sum(axis=1)
;;     res[('NG_inv', '')]=res.iloc[:, res.columns.get_level_values(0)=='NG'].max(axis=1)
;;     res[('RC_inv', '')]=res.iloc[:, res.columns.get_level_values(0)=='RC'].max(axis=1)
;;     res.sort_values(by=[('Score', ''), ('Excess', '')], ascending=False, inplace=True)
;;     #need to join multindex columns to single index columns in title_strength, so this the merge process
;;     tuples = [('SRC', ''), ('TITLE', ''), ('STR', '')]
;;     titles=copy.deepcopy(title_strength)
;;     titles.columns=pd.MultiIndex.from_tuples(tuples, names=(None, 'phase'))
;;     res=res.reset_index()
;;     #Make sure that scores are monotonically decreasing as inventory decreases
;;     res=res.groupby(('SRC', ''), sort=False)
;;     #add a smoothed AC column
;;     res=res.apply(add_smoothed, ('AC', ''), ('AC_smoothed', ''))
;;     res=check_order(order_writer, demand_name, res, smooth)
;;     res = pd.merge(res,
;;           titles,
;;           on=[('SRC', '')],
;;           how='inner'
;;          )
;;     res=res.set_index(['SRC', 'AC'])
;;     res.drop(['NG', 'RC'], axis=1, level=0, inplace=True)
;;     return res

;; #The name of the score column used in the combined worksheet before renaming for output
;; combined_score_out='min_score_peak'
;; def cols_to_round(df):
;;     floats = df.select_dtypes('float').columns
;;     d = dict(zip(floats, [4 for x in floats]))
;;     d.pop(combined_score_out, None)
;;     d.pop(('Score', dmet_sum), None)
;;     return d

;; #take any multiindex column tuple and if the second level has no value (thus a single entry for both levels),
;; #then swap the second level with the first level.
;; def move_col_names_down(df):
;;     #if the last columns have an empty first level, they will get merge_celled with the previously-titled
;;     #column, so we use ' ' instead of '' to avoid this.
;;     new_cols = [(' ', x) if y=='' else (x, y) for (x, y) in df.columns]
;;     #phase is actually named after the column here
;;     df.columns=pd.MultiIndex.from_tuples(new_cols, names=(None, 'OML'))

;; #Find the the most stressful demand by first choosing the lowest total demand, then choosing
;; #lowest score and then choosing lowest excess
;; def min_score_demand_total(results_map, row):
;;     score_excesses = sorted([(-1*row['Demand_Total_'+demand_name], row['Score_'+demand_name], row['Excess_'+demand_name], demand_name) 
;;                              for demand_name in results_map])
;;     min_total, min_score, min_excess, min_demand = score_excesses[0]
;;     return min_demand

;; def min_score_demand(results_map, row):
;;     score_excesses = sorted([(row['Score_'+demand_name], row['Excess_'+demand_name], demand_name) 
;;                              for demand_name in results_map])
;;     min_score, min_excess, min_demand = score_excesses[0]
;;     return min_demand

;; def min_score_demand_peak(peak_map, default_max, row):
;;     return peak_map.get(row['SRC'], default_max)

;; #returns the actual minimum score
;; #pull the score by the demand in the pull column and put it in the out column
;; def pull_score(df, pull, out):
;;     df[out]=df.apply(lambda row: row['Score_'+row[pull]], axis=1)

;; #need to add the excess of the min score case as well for sorting.
;; def add_excess(row, pull, left):
;;     score_index = left.columns.get_loc('Score_'+row[pull])
;;     return row.iloc[score_index+1]

;; #Given a cell in a sheet starting at row row_start and in column,clear all cell contents
;; def clear_column(row_start, column, sh):
;;     for row in range(row_start,sh.max_row):
;;         if(sh.cell(row,column).value is  None):
;;             break
;;         sh.cell(row,column).value= None

;; # When writing the multi-index dataframes to Excel, pandas put an extra blank row below the column names, which messes up the filter in LibreOffice, but not Excel.  In Excel, you could turn the filter on the blank row.  In LibreOffice, that didn't work.  Although, in LibreOffice, you can turn it on the first row and it captures the first value.  Excel does not.  So those are the filter workarounds, but it looks cleaner to just remove that blank row.
;; def remove_blank_row(excel_path, out_path):
;;     if isinstance(excel_path, str):
;;         wb=openpyxl.reader.excel.load_workbook(excel_path)
;;     else:
;;         wb=excel_path
;;     for sheet in wb.worksheets:
;;         if not sheet['A3'].value:
;;             sheet.delete_rows(3, 1)
;;     wb.save(out_path)

;; #For the negative scores for last cuts, set the Score to 0 for display and
;; #do the same with the Excess.
;; def zero_negative_scores(group_df, excess_col, score_col):
;;     group_df[excess_col] = np.where((group_df[score_col]<0), 0,                 
;;                                        group_df[excess_col])
;;     group_df[score_col] = np.where((group_df[score_col]<0), 0,                 
;;                                        group_df[score_col])    
;;     return group_df
    
;; #add the last ra cuts if there is no rc
;; def add_last_ra_cuts(scored_results):
;;     #Since we are using the score from the next lowest inventory, we 
;;         #need to add additional records to cover the case where we we have no
;;         #runs from 0 RA 0 NG and 0 RC so that we could cut the entire RA if we
;;         #wanted to.
;;         test_frame=scored_results[(scored_results['AC']==1) 
;;                               & (scored_results['NG_inv']==0) 
;;                               & (scored_results['RC_inv']==0)].copy()
;;         test_frame[('Score', '')]=test_frame[('Score', '')]-1.1
;;         test_frame[('AC', '')]=0
;;         zero_cols=['demand_met', 'dmet_times_weight', 'emet_times_weight',
;;                    'excess_met']
;;         test_frame[zero_cols]=0
;;         return test_frame
    
;; def make_one_n(results_map, peak_max_workbook, out_root, phase_weights, one_n_name, baseline_path, smooth: bool):
;;     print("Building ", one_n_name)
;;     # Read in the SRC baseline for strength and OI title.
;;     baseline = pd.read_excel(baseline_path)
;;     title_strength=baseline[['SRC', 'TITLE', 'STR']]
;;     maxes=pd.read_excel(peak_max_workbook, "by_src")
;;     maxes['demand_name'] = maxes['demand_name'].astype(str)
;;     maxes=maxes.set_index('SRC')
;;     peak_map=maxes['demand_name'].to_dict()
    
;;     wb = openpyxl.reader.excel.load_workbook(peak_max_workbook)
;;     ws=wb['default']
;;     default_max=str(ws['A1'].value)
    
;;     writer = pd.ExcelWriter(out_root+one_n_name, engine='xlsxwriter')
;;     left=pd.DataFrame()
;;     order_path = out_root+"out_of_order.xlsx"
;;     orders= Path(order_path)
;;     if orders.is_file():
;;         order_writer = pd.ExcelWriter(order_path, engine='openpyxl', mode='a', if_sheet_exists='replace')
;;         # if we're on windows, we need to modify some things for replace to
;;         #work properly.  Might be a bug in ExcelWriter on the high side
;;         #Windows version of Anaconda.
;;         if sys.platform=='win32':
;;             book=openpyxl.load_workbook(out_root+"out_of_order.xlsx")
;;             order_writer.book = openpyxl.load_workbook(out_root+"out_of_order.xlsx")
;;             order_writer.sheets = dict((ws.title, ws) for ws in book.worksheets)
;;     else:
;;         order_writer = pd.ExcelWriter(out_root+"out_of_order.xlsx", engine='xlsxwriter')
            
;;     for demand_name in results_map:
;;         #START OF SINGLE DEMAND OUTPUT WORKSHEET
;;         scored_results = compute_scores(results_map[demand_name], phase_weights, title_strength, smooth, demand_name, order_writer)
;;         if left.empty:
;;             max_df=scored_results.reset_index().groupby('SRC')['AC'].apply(max)
;;             maxes=max_df.to_dict()
;;         #just to repeat the SRC in the output. Also will add an index on the left.
;;         scored_results.reset_index(inplace=True)
        
;;         test_frame=add_last_ra_cuts(scored_results)
        
;;         #add on records to cut the last unit in the inventory.
;;         scored_results=pd.concat([scored_results, test_frame], ignore_index=True)
        
;;         #add max ac inventory
;;         scored_results['max_AC_inv']=scored_results['SRC'].map(maxes)
;;         #filter out the base inventories
;;         scored_results=scored_results[scored_results['AC']!=scored_results['max_AC_inv']]
;;         #add one to the remaining inventory records
;;         scored_results['AC']=scored_results['AC']+1
;;         #indicate those records that are the base supply
;;         scored_results['base_supply']=np.where((scored_results['AC']==scored_results['max_AC_inv']), 'X', 'Down')
;;         #remove maxes
;;         scored_results.drop(columns=['max_AC_inv'], level=0, inplace=True)
;;         #TEMP END OF SINGLE DEMAND OUTPUT WORKSHEET
            
;;         #add scores to all_scores
;;         #join tables so that you have two score columns for two demands
;;         #add column called min_score
;;         #add another column called min_score_demand that indicates which demand this came from
;;         scored_results=scored_results.set_index(['SRC', 'AC'])
;;         score_columns=[('Score', dmet_sum), ('Excess', emet_sum), ('Demand_Total', '')]
;;         score_col_names=['Score_'+demand_name, 'Excess_'+demand_name, 'Demand_Total_'+demand_name]
;;         scores = scored_results[score_columns]
;;         scores.columns=score_col_names
;;         if left.empty:
;;             left=scores
;;         else:
;;             right=scores
;;             left = pd.merge(left,right,on=['SRC', 'AC'], how='outer')
            
;;         #RESTART OF SINGLE DEMAND OUTPUT WORKSHEET
;;         #write to excel file here
;;         scored_results.reset_index(inplace=True)
;;         #need to sort again after changing the index..
;;         scored_results.sort_values(by=[('Score', ''), ('Excess', '')], ascending=False, inplace=True)
;;         scored_results=zero_negative_scores(scored_results, ('Excess', ''), 
;;                                             ('Score', ''))
;;         scored_results.rename(columns={'NG_inv':'NG', 'RC_inv':'AR', 'AC':'RA'}, inplace=True, level=0)
;;         initial_cols = [('SRC', ''), ('TITLE', ''), ('RA', ''), ('NG', ''), 
;;                         ('AR', ''),
;;                        ]
;;         reordered=reorder_columns(initial_cols, scored_results)
;;         reordered.reset_index(inplace=True)
;;         reordered.drop(['index'], axis=1, level=0, inplace=True)
;;         #avoid rounding when we choose the min score later by doing a deep copy.
;;         reordered=copy.deepcopy(reordered)
;;         reordered = reordered.round(cols_to_round(reordered))
;;         reordered.index=[i for i in range(1, len(reordered.index)+1)]
;;         move_col_names_down(reordered)
;;         reordered.rename(columns={"total-quantity": "Demand Days"}, level=0, inplace=True)
;;         reordered.to_excel(writer, sheet_name=demand_name) 
;;         #PERMANENT END OF SINGLE DEMAND OUTPUT WORKSHEET
    
;;     #NOW DOING STUFF TO OUTPUT THE COMBINED SCORE AND EXCESSES THAT WERE COLLECTED
;;     left.reset_index(inplace=True)
;;     #deepcopy prevents working on a slice
;;     left=copy.deepcopy(left)
;;     left['min_score_demand']=left.apply(partial(min_score_demand, results_map), axis=1)
;;     left['min_score_demand_total']=left.apply(partial(min_score_demand_total, results_map), axis=1)
;;     left['min_score_demand_peak']=left.apply(partial(min_score_demand_peak, peak_map, default_max), axis=1)

;;     #left['min_score']=left.apply(lambda row: row['Score_'+row['min_score_demand']], axis=1)
;;     pull_score(left, 'min_score_demand', 'min_score')
;;     pull_score(left, 'min_score_demand_total', 'min_score_total')
;;     pull_score(left, 'min_score_demand_peak', 'min_score_peak')

;;     left['min_demand_excess']=left.apply(add_excess, axis=1, pull='min_score_demand', left=left)
;;     left['min_demand_total_excess']=left.apply(add_excess, axis=1, pull='min_score_demand_total', left=left)
;;     left['min_demand_peak_excess']=left.apply(add_excess, axis=1, pull='min_score_demand_peak', left=left)
;;     #write third worksheet with all scores here
;;     left = pd.merge(left, title_strength, on='SRC', validate='many_to_one')
;;     left['SRC2']=left.SRC.str[:2]
;;     initial_cols=['SRC2','SRC', 'TITLE', 'AC']
;;     left = reorder_columns(initial_cols, left)
;;     #resort after choosing the max demand records from each demand case.
;;     left.sort_values(by=['min_score_peak', 'min_demand_peak_excess'], ascending=False, inplace=True)
;;     left.reset_index(inplace=True)
;;     left.drop(['index'], axis=1, inplace=True)
;;     left = left.round(cols_to_round(left))
    
;;     #only select final output here
;;     left=left[initial_cols + ['min_score_demand_peak', 'min_score_peak', 'min_demand_peak_excess', 'STR']]
;;     left.columns=['SRC2', 'SRC', 'TITLE', 'RA Qty', 'Most Stressful', 'Score', 'Excess', 'STR']
;;     left.index=[i for i in range(1, len(left.index)+1)]  
;;     left=zero_negative_scores(left, 'Excess', 'Score')
;;      #output     
;;     left.to_excel(writer, sheet_name='combined')
;;     writer.save()
;;     order_writer.save()
;;     wb = openpyxl.reader.excel.load_workbook(out_root+one_n_name)
;;     ws=wb['combined']
;;     ws.cell(1, 1).value = "OML"
;;     remove_blank_row(wb, out_root+one_n_name)
    
;; def concat_run_to_phase(run_key, results_path):
;;     df=pd.read_csv(results_path, sep='\t')
;;     df['phase']=df['phase']+'-'+run_key
;;     return df

;; #Before we call make_one_n, concatenate all of the results files into one
;; #DataFrame, concatenating phase labels with the run key in the results_map.
;; def one_n_across_runs(results_map, peak_max_workbook, out_root, phase_weights, one_n_name, baseline_path, smooth: bool):
;;     concatenated_dfs=[]
;;     for demand_name in results_map:
;;         concatenated_df=concat_run_to_phase(demand_name, results_map[demand_name])
;;         concatenated_dfs.append(concatenated_df)
;;     all_runs_df=pd.concat(concatenated_dfs)
;;     dummy_results_map={'all_runs' : all_runs_df}
;;     make_one_n(dummy_results_map, peak_max_workbook, out_root, phase_weights, one_n_name, baseline_path, smooth)
    
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


(comment
  (def dt (-> (io/file-path "~/repos/make-one-to-n/resources/results.txt") (tc/dataset {:separator "\t" :key-fn keyword})))

  )






