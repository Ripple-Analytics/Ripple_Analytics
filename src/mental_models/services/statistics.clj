(ns mental-models.services.statistics
  "Statistical analysis service for mental model effectiveness"
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as stats]
            [taoensso.timbre :as log]))

;; Initialize matrix implementation
(m/set-current-implementation :vectorz)

;; -- Basic Statistics --------------------------------------------------------

(defn mean [xs]
  (when (seq xs)
    (/ (reduce + xs) (count xs))))

(defn variance [xs]
  (when (seq xs)
    (let [m (mean xs)
          n (count xs)]
      (/ (reduce + (map #(Math/pow (- % m) 2) xs)) n))))

(defn std-dev [xs]
  (when-let [v (variance xs)]
    (Math/sqrt v)))

(defn median [xs]
  (when (seq xs)
    (let [sorted (sort xs)
          n (count sorted)
          mid (quot n 2)]
      (if (odd? n)
        (nth sorted mid)
        (/ (+ (nth sorted mid) (nth sorted (dec mid))) 2)))))

(defn percentile [xs p]
  (when (seq xs)
    (let [sorted (sort xs)
          n (count sorted)
          idx (int (* p (dec n)))]
      (nth sorted idx))))

;; -- Correlation Analysis ----------------------------------------------------

(defn pearson-correlation
  "Calculate Pearson correlation coefficient between two vectors"
  [xs ys]
  (when (and (seq xs) (seq ys) (= (count xs) (count ys)))
    (let [n (count xs)
          mx (mean xs)
          my (mean ys)
          sx (std-dev xs)
          sy (std-dev ys)]
      (when (and (pos? sx) (pos? sy))
        (/ (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
           (* n sx sy))))))

(defn correlation-matrix
  "Calculate correlation matrix for multiple variables"
  [data-matrix]
  (let [n (count (first data-matrix))
        vars (m/transpose data-matrix)]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (if (= i j)
                    1.0
                    (pearson-correlation (nth vars i) (nth vars j)))))))))

;; -- Regression Analysis -----------------------------------------------------

(defn linear-regression
  "Simple linear regression: y = mx + b"
  [xs ys]
  (when (and (seq xs) (seq ys) (= (count xs) (count ys)))
    (let [n (count xs)
          mx (mean xs)
          my (mean ys)
          ss-xy (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
          ss-xx (reduce + (map (fn [x] (Math/pow (- x mx) 2)) xs))
          slope (if (zero? ss-xx) 0 (/ ss-xy ss-xx))
          intercept (- my (* slope mx))
          ;; R-squared
          y-pred (map #(+ intercept (* slope %)) xs)
          ss-res (reduce + (map (fn [y yp] (Math/pow (- y yp) 2)) ys y-pred))
          ss-tot (reduce + (map (fn [y] (Math/pow (- y my) 2)) ys))
          r-squared (if (zero? ss-tot) 0 (- 1 (/ ss-res ss-tot)))]
      {:slope slope
       :intercept intercept
       :r-squared r-squared})))

(defn multiple-regression
  "Multiple linear regression using normal equations"
  [X y]
  (try
    (let [X-mat (m/matrix X)
          y-vec (m/matrix y)
          Xt (m/transpose X-mat)
          XtX (m/mmul Xt X-mat)
          XtX-inv (m/inverse XtX)
          Xty (m/mmul Xt y-vec)
          coefficients (m/mmul XtX-inv Xty)]
      {:coefficients (m/to-vector coefficients)
       :success true})
    (catch Exception e
      (log/warn "Multiple regression failed:" (.getMessage e))
      {:coefficients []
       :success false})))

;; -- Cluster Analysis --------------------------------------------------------

(defn euclidean-distance [a b]
  (Math/sqrt (reduce + (map (fn [x y] (Math/pow (- x y) 2)) a b))))

(defn k-means-step
  "One iteration of k-means clustering"
  [points centroids]
  (let [;; Assign points to nearest centroid
        assignments (map (fn [p]
                           (apply min-key
                                  (fn [i] (euclidean-distance p (nth centroids i)))
                                  (range (count centroids))))
                         points)
        ;; Update centroids
        clusters (group-by identity assignments)
        new-centroids (vec (for [i (range (count centroids))]
                             (let [cluster-points (map first
                                                       (filter #(= i (second %))
                                                               (map vector points assignments)))]
                               (if (seq cluster-points)
                                 (vec (map mean (apply map vector cluster-points)))
                                 (nth centroids i)))))]
    {:assignments assignments
     :centroids new-centroids}))

(defn k-means
  "K-means clustering algorithm"
  [points k & {:keys [max-iterations] :or {max-iterations 100}}]
  (when (and (seq points) (pos? k))
    (let [initial-centroids (vec (take k (shuffle points)))]
      (loop [centroids initial-centroids
             iter 0]
        (if (>= iter max-iterations)
          {:centroids centroids
           :iterations iter}
          (let [{:keys [assignments centroids]} (k-means-step points centroids)]
            (recur centroids (inc iter))))))))

;; -- Anomaly Detection -------------------------------------------------------

(defn z-score [x mean std-dev]
  (if (zero? std-dev)
    0
    (/ (- x mean) std-dev)))

(defn detect-anomalies
  "Detect anomalies using z-score method"
  [xs & {:keys [threshold] :or {threshold 2.5}}]
  (when (seq xs)
    (let [m (mean xs)
          s (std-dev xs)]
      (map-indexed
       (fn [i x]
         (let [z (z-score x m s)]
           {:index i
            :value x
            :z-score z
            :is-anomaly (> (Math/abs z) threshold)}))
       xs))))

(defn isolation-forest-score
  "Simplified isolation forest anomaly score"
  [point points sample-size]
  (let [sample (take sample-size (shuffle points))
        ;; Calculate average distance to sample points
        distances (map #(euclidean-distance point %) sample)
        avg-distance (mean distances)]
    {:point point
     :score avg-distance
     :is-anomaly (> avg-distance (* 2 (mean (map #(mean (map (fn [p] (euclidean-distance % p)) sample)) sample))))}))

;; -- Time Series Analysis ----------------------------------------------------

(defn moving-average
  "Calculate moving average with window size n"
  [xs n]
  (when (and (seq xs) (pos? n))
    (map mean (partition n 1 xs))))

(defn exponential-smoothing
  "Simple exponential smoothing"
  [xs alpha]
  (when (seq xs)
    (reduce
     (fn [acc x]
       (conj acc (+ (* alpha x) (* (- 1 alpha) (last acc)))))
     [(first xs)]
     (rest xs))))

(defn trend-detection
  "Detect trend in time series"
  [xs]
  (when (seq xs)
    (let [n (count xs)
          indices (range n)
          {:keys [slope r-squared]} (linear-regression indices xs)]
      {:trend (cond
                (> slope 0.1) :increasing
                (< slope -0.1) :decreasing
                :else :stable)
       :slope slope
       :strength r-squared})))

;; -- Model Effectiveness Analysis --------------------------------------------

(defn analyze-model-effectiveness
  "Analyze effectiveness of mental models from usage data"
  [usage-data]
  (when (seq usage-data)
    (let [by-model (group-by :model-id usage-data)
          model-stats (for [[model-id usages] by-model]
                        (let [outcomes (map :outcome-rating usages)]
                          {:model-id model-id
                           :usage-count (count usages)
                           :mean-outcome (mean outcomes)
                           :std-dev (std-dev outcomes)
                           :success-rate (/ (count (filter #(> % 3) outcomes)) (count outcomes))}))]
      {:models (sort-by :mean-outcome > model-stats)
       :total-usages (count usage-data)
       :overall-mean (mean (map :outcome-rating usage-data))})))

(defn model-combination-analysis
  "Analyze which combinations of models work best together"
  [decision-data]
  (when (seq decision-data)
    (let [;; Group by model combinations
          by-combo (group-by #(set (:models-applied %)) decision-data)
          combo-stats (for [[combo decisions] by-combo
                           :when (> (count combo) 1)]
                        (let [outcomes (map :outcome-rating decisions)]
                          {:models combo
                           :count (count decisions)
                           :mean-outcome (mean outcomes)
                           :synergy-score (* (mean outcomes) (Math/log (count combo)))}))]
      (sort-by :synergy-score > combo-stats))))
