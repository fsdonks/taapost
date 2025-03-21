;;lifted from taa.demandanalysis.
;;we may add an explicit dependency on taa in the future....
;;would be nice to not have to though, since we cascade dependencies...
(ns taapost.demandanalysis
  (:require
   [spork.util.io :as io]
   [proc.demandanalysis :as analysis]
   [spork.util.excel.docjure :as dj]))

(defn peaks-for-period
  "Given a path to a marathon input workbook, return a [peak-demand
  tag]
  by :SRC for the specified period.  Throw an exception if the period
  doesn't exist. "
  [path for-period tag]
  (let [res
        (->>
         (analysis/peaks-from path :group-fn identity)
         (filter (fn [{:keys [period]}] (= period for-period))))]
    (when (empty? res)
      (throw
       (Exception. (str "The period " for-period " might not exist!"))))
    (into {} (for [{:keys [group peak]} res]
               ;;:SRC to peak demand
               [group [peak tag]]))))

(defn max-tag
  "Used for merge-with to return the demand-name and peak demand of
  the workbook with the max demand."
  [[peak-left tag-left :as left]
   [peak-right tag-right :as right]]
  (if (> peak-right peak-left)
    right
    left))

;;modified form, we include the peak here.
(defn maxes->records
  [max-demands]
  (for [[src [peak demand-name]] max-demands]
    {:SRC src :demand_name demand-name :peak peak}))

(defn max-demand
  "Compute the peak demand by SRC across a map of {demand-name
  workbookpath} for the period for-period."
  [wkbk-map for-period]
  (apply merge-with max-tag
         (map (fn [[demand-name path]]
                (peaks-for-period path for-period demand-name))
              wkbk-map)))

#_
(defn maxes->xlsx!
  [wkbk-map for-period default wkbk-path]
  (->> (max-demand wkbk-map for-period)
       (maxes->records)
       (util/records->xlsx wkbk-path "by_src"))
  (add-default! wkbk-path default))

#_
(defn add-default!
  [wkbk-path default]
  (let [wkbk (dj/load-workbook wkbk-path)]
    (dj/add-sheet! wkbk "default")
    (-> (dj/select-sheet "default" wkbk)
        (dj/add-row! [default]))
    (dj/save-workbook! wkbk-path wkbk)))

(comment
  (def maxes (max-demand {"A" (io/file-path "~/repos/notional/base-testdata-v8.xlsx")}))
  )
