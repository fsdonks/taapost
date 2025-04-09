;;Isotonic regression implementation.
;;Note: We will eventually port this to the newly inspired implementation
;;bundled in fastmath.
(ns taapost.iso
  (:require [ham-fisted.api :as hf]))

;;isotonic regression is pretty easy....
;;we can traverse in linear time, but every time we have an event,
;;we need to push an index back on the queue.
;;so it's possible we have to backtrack to keep monotonicity after we
;;introduce new points.

;;One small wrinkle that has big consequences: if you have substantially
;;non-monotonic jumps in your input, then it's possible that the naive
;;averaging method can degenerate into a really slow, almost perpetual
;;hill climb where you keep pushing pairwise averages left with new data,
;;trying to get the left points back to monotinicity if a new right
;;point violates it.  For simple inputs this can really blow up and
;;take forever.  So the alternative, smarter method, is to just
;;represent blocks of data, and keep a running sum/count and the current
;;value for the block, then compare blocks.  This collapses the comparisons
;;and averaging prospect down substantially and makes it tractable.

(defn merge-blocks [l r]
  (let [n2   (+ (l :n) (r :n))
        sum2 (+ (l :sum) (r :sum))]
    {:n  n2
     :sum sum2
     :v  (double (/ sum2 n2))}))

;;let's adopt a block-merge approach.
;;we can scan left on insert...
;;cheap insertion would be nice....

(defn splice [xs l r v]
  (let [base (conj (subvec xs 0 l) v)
        remainder (when (< r (dec (count xs)))
                    (subvec xs (inc r)))]
    (if remainder (into base remainder)
      base)))

#_
(defn merge-left [blocks new-block idx]
  (loop [l   idx
         acc new-block]
    (if (> l 0)
      (let [left (blocks l)]
        (if (<= (left :v) (acc :v))
          [(splice blocks (inc l) (+ idx 2) acc) (inc l)]
          (recur (dec l) (merge-blocks (blocks l) acc))))
      [(splice blocks (inc l) (+ idx 2) acc) (inc l)])))

;;if we merge-left, check the result to the left.
(defn merge-left [blocks new-block idx]
  (loop [l   idx
         acc new-block]
    (if (>= l 0)
      (let [left (blocks l)]
        (if (<= (left :v) (acc :v))
          [(splice blocks (inc l) (+ idx 2) acc) (inc l)]
          (recur (dec l) (merge-blocks (blocks l) acc))))
      [(splice blocks (inc l) (+ idx 2) acc) (inc l)])))

(defn expand [dir xs]
  (->>  (for [{:keys [n sum v]} (case dir
                                  :asc xs
                                  :desc (rseq xs)
                                  (throw (ex-info "unknown direction" {:expected #{:asc :desc} :got dir})))]
          (repeat n v))
        (apply concat)
        vec))

(defn iso
  ([dir xs]
   (let [blocks (->> xs (mapv (fn [x] {:n 1 :sum x :v x})))
         bound (count xs)
         steps (atom 0)]
     (loop [acc blocks
            idx 0]
       (if (< idx  (dec (count acc)))
         (let [l (acc idx)
               r (nth acc (inc idx) nil)]
           (if (or (not r) (<= (l :v) (r :v)))
             (recur acc (unchecked-inc idx))
             (let [[new-blocks new-idx] (merge-left acc (merge-blocks l r) (dec idx))
                   _ (when (> (swap! steps inc) bound) (throw (ex-info "out of time" {:idx idx :acc acc})))]
               (recur new-blocks new-idx))))
         (expand dir acc)))))
  ([xs] (iso :asc xs)))

;;original naive block solution, starts from 0 every time.  we do better.
#_
(defn iso1 [xs]
  (let [blocks (->> xs (mapv (fn [x] {:n 1 :sum x :v x})))]
    (loop [acc blocks
           idx 0]
      (if (< idx (dec (count acc)))
        (let [l (acc idx)
              r (acc (inc idx))]
          (if (<= (l :v) (r :v))
            (recur acc (unchecked-inc idx))
            (let [n2   (+ (l :n) (r :n))
                  sum2 (+ (l :sum) (r :sum))
                  new-block {:n  n2
                             :sum sum2
                             :v  (double (/ sum2 n2))}
                  new-blocks (into (conj (subvec acc 0 idx) new-block) (subvec acc (+ idx 2)))]
              (recur new-blocks 0))))
        (->>  (for [{:keys [n sum v]} acc]
                (repeat n v))
              (apply concat)
              vec)))))

;;ported from
;;https://github.com/lorentzenchr/scipy/blob/88abb5ed1cd85b23c4ae669d331a04599c28c145/scipy/optimize/_pava/pava_pybind.cpp
;;under the BSD-3 License

;; // x is the response variable (often written as y). Its ordering is crucial.
;; // Usually, it is sorted according to some other data (feature or covariate), e.g.
;; //   indices = np.argsort(z)
;; //   x = x[indices]
;; // Note that x is modified inplace and, on return, will contain the solution.
;; // w is an array of case weights, modified inplace.
;; // r is an array of indices such that x[r[i]:r[i+1]] contains the i-th block,
;; // modified inplace.

;; // Algorithm 1 of
;; // Busing, F. M. T. A. (2022).
;; // Monotone Regression: A Simple and Fast O(n) PAVA Implementation.
;; // Journal of Statistical Software, Code Snippets, 102(1), 1-25.
;; // https://doi.org/10.18637/jss.v102.c01
;; // Notes:
;; //  - We translated it to 0-based indices.
;; //  - xb, wb, sb instead of x, w and S to avoid name collisions
;; //  - xb_prev and wb_prev instead of x_hat and w_hat
;; //  - ERROR CORRECTED: Lines 9 and 10 have index i instead of b.
;; //  - MODIFICATIONS: Lines 11 and 22 both have >= instead of >
;; //    to get correct block indices in r. Otherwise, same values can get in
;; //    different blocks, e.g. x = [2, 2] would produce
;; //    r = [0, 1, 2] instead of r = [0, 2].
;; //
;; // procedure monotone(n, x, w)      // 1: x in expected order and w nonnegative

;;Doesn't work right, need to rewrite it [now we can just use the java impl from fastmath though]
#_
(defn pava [xa wa ra]
   (let  [x       (hf/mut-list xa)
          w       (hf/mut-list wa)
          r       (hf/mut-list [0 1])
          n       (count x)
          b       (atom 0)
          i       (atom 1)
          xb-prev (atom (x 0))
          wb-prev (atom (w 0))
          bound   (count x)]
     (while (< @i bound)
       (let [bnxt (swap! b inc)
             xb   (atom (x @i))
             wb   (atom (w @i))
             sb   (atom 0.0)]
         (when (>= @xb-prev @xb)
           (swap! b dec)
           (reset! sb (+ (* @wb-prev @xb-prev) (* @wb @xb)))
           (swap!  wb #(/ @sb %))
           (while  (and (<  @i (dec n))
                        (>= @xb (x (inc @i))))
             (swap! i inc)
             (swap! sb + (* (w @i) (x @i)))
             (swap! wb + (w @i))
             (reset! xb (/ @sb @wb)))
           (while (and (> @b 0)
                       (>= (x (dec @b)) @xb))
             (swap! b dec)
             (swap! sb + (* (w @b) (x @b)))
             (swap! wb + (w @b))
             (swap! xb (/ @sb @wb))))
         (reset! xb-prev @xb)
         (reset! wb-prev @wb)
         (assoc x @b @xb)
         (assoc w @b @wb)
         (assoc r (inc @b) (inc @i))
         (swap! i inc)))
     (println :mid)
     (let [f (atom (dec n))
           k (atom @b)]
       (while (>= @k 0)
         (let [t  (r @k)
               xk (x @k)
               _ (reset! i @f)]
           (while (>= @i t)
             (assoc x @i xk)
             (swap! i dec))
           (reset! f (dec t))
           (swap! k dec)))
       [x w r (inc @b)])))
