;;Operations defining the infamous BarChartData
;;script, ported from legacy R to clojure.
;;assumes spork.util.table; will switch to tablecloth.
(ns taapost.bcd
  (:require [spork.util [io :as io] [table :as tbl]]))

;;We are geting wrong results for one of the scenarios
;;and one SRC.
;;Prefer to recompute it.

;;naive, replace with tablecloth ops.
(defn mean [xs]
  (->> xs
       (reduce (fn [[sum n] x]
                 [(+ sum x) (inc n)]) [0 0])
       (apply /)
       double))

;;just some aliases.

;;this causes us to have some janky fill results.
;;we choose to instead yield 0.
(defn safe-div [x y]
  (if (zero? y)
    0.0
    (double (/ x y))))

;;will replace this with table ops later.
(defn bar-chart-data [xs & {:keys [scenario]
                            :or {scenario "undefined"}}]
  (let [RAFill   :AC-fill
        RAExcess :AC-deployable
        Demand   :total-quantity
        RCFill   (fn [{:keys [NG-fill RC-fill]}] (+ NG-fill RC-fill))
        RCExcess (fn [{:keys [NG-deployable RC-deployable]}]
                   (+ NG-deployable RC-deployable))]
    (->> (for [[[SRC AC NG RC phase] recs] (group-by (juxt :SRC :AC :NG :RC :phase)
                                                     (filter (fn [{:keys [phase]}]
                                                               (#{"comp1" "phase3"} phase)) xs))]
           (let [mean-by (fn [f] (->> recs (map f) mean))]
             (array-map
              :SRC SRC
              :key (str SRC AC NG RC (case phase "comp1" "c" "p") scenario)
              :AC  AC
              :NG  NG
              :RC  RC
              :phase phase
              :Scenario scenario
              :dmetRA (mean-by #(safe-div (+ (RAFill %) (RAExcess %)) (Demand %)))
              :dmetRC (mean-by #(safe-div (+ (RCFill %) (RCExcess %)) (Demand %)))
              :ACunavailable (mean-by #(safe-div (:AC-not-ready %) (Demand %)))
              :RCunavailable (mean-by #(safe-div (+ (:RC-not-ready %) (:NG-not-ready %))
                                                 (Demand %))))))
         (sort-by (juxt :SRC (comp - :AC) (comp - :NG) (comp - :RC) :phase)))))

(defn bar-chart-entry [{:keys [SRC AC NG RC phase] :as r}]
  (let [RAFill   :AC-fill
        RAExcess :AC-deployable
        Demand   :total-quantity
        RCFill   (fn [{:keys [NG-fill RC-fill]}] (+ NG-fill RC-fill))
        RCExcess (fn [{:keys [NG-deployable RC-deployable]}]
                   (+ NG-deployable RC-deployable))]
    (array-map
     :SRC SRC
     :key (str SRC AC NG RC (case phase "comp1" "c" "p") "entry")
     :AC  AC
     :NG  NG
     :RC  RC
     :phase phase
     :Scenario "scenario"
     :dmetRA (safe-div (+ (RAFill r) (RAExcess r)) (Demand r))
     :dmetRC (safe-div (+ (RCFill r) (RCExcess r)) (Demand r))
     :ACunavailable (safe-div (:AC-not-ready r) (Demand r))
     :RCunavailable (safe-div (+ (:RC-not-ready r) (:NG-not-ready r))
                                        (Demand r)))))

(def results-schema
  (array-map
   :rep-seed :double
   :SRC :text
   :phase :text
   :AC-fill :int
   :NG-fill :int
   :RC-fill :int
   :AC-overlap :int
   :NG-overlap :int
   :RC-overlap :int
   :total-quantity :int
   :AC-deployable :int
   :NG-deployable :int
   :RC-deployable :int
   :AC-not-ready :int
   :NG-not-ready :int
   :RC-not-ready :int
   :AC-total :int
   :NG-total :int
   :RC-total :int
   :AC :int
   :NG :int
   :RC :int))

;;for multiple excursions...
(defn ename [path]
  (-> (->> path
           reverse
           (take-while (complement #{\_}))
           reverse
           (apply str))
      (clojure.string/replace ".txt" "")))

(defn get-excursions [root scenario-str]
  (->> root
       io/file
       file-seq
       (filter #(clojure.string/includes? (io/fname %) "results_"))
       (map (fn [fl]
              (let [nm (io/fname fl)
                    scenario (if (clojure.string/includes? nm scenario-str)
                               "A"
                               "B")]
                {:parent (io/parent-path (io/fpath fl))
                 :path  (io/fpath fl)
                 :scenario scenario})))
       (group-by :parent)))

(defn do-bcds [root scenario-str]
  (doseq [[dir fls] (get-excursions root scenario-str)]
    (let [_  (println [:doing dir])
          nm (-> fls first :path ename)
          bcds (apply concat
                      (for [{:keys [path scenario]} fls]
                        (let [res (into []
                                        (tbl/tabdelimited->records (slurp path)
                                                                   :schema results-schema))]
                          (bar-chart-data res :scenario scenario))))]
      (tbl/records->file bcds (io/file-path dir (str "bcd_" nm ".txt"))))))

(def outflds
  (array-map
    :SRC :text
    :key :text
    :AC :int
    :NG :int
    :RC :int
    :phase :text
    :Scenario :text
    :dmetRA :double
    :dmetRC :double
    :ACunavilable :double
    :RCunavailable :double))

(defn cat-bcds [root & {:keys [to]}]
  (let [to (or to (io/file-path root "bcd-all.txt"))]
    (-> (->> (file-seq (io/file root))
             (filter (fn [fl] (clojure.string/includes? (io/fname fl) "bcd_")))
             (mapcat (fn [fl]
                       (let [from (io/fname fl)
                             _ (println [:concatenating from])]
                         (->> (io/fpath fl)
                              tbl/tabdelimited->records
                              (into [] (map (fn [r] (assoc r :source from)))))))))
        (tbl/records->file (io/file-path to) :field-order (vec (keys outflds))))))
