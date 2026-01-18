(ns mental-models.statistics
  "Statistical Engine in Clojure
   Comprehensive statistical analysis functions
   
   This module provides:
   - Descriptive statistics
   - Correlation analysis (Pearson, Spearman)
   - Regression analysis
   - Covariance analysis
   - Hypothesis testing"
  (:require [clojure.math.numeric-tower :as math]))

;; ============================================
;; Basic Statistical Functions
;; ============================================

(defn mean
  "Calculate arithmetic mean."
  [xs]
  (if (seq xs)
    (/ (reduce + xs) (count xs))
    0))

(defn median
  "Calculate median value."
  [xs]
  (if (seq xs)
    (let [sorted (sort xs)
          n (count sorted)
          mid (quot n 2)]
      (if (odd? n)
        (nth sorted mid)
        (/ (+ (nth sorted mid) (nth sorted (dec mid))) 2)))
    0))

(defn mode
  "Calculate mode (most frequent value)."
  [xs]
  (if (seq xs)
    (->> xs
         frequencies
         (sort-by val >)
         first
         key)
    nil))

(defn variance
  "Calculate population variance."
  [xs]
  (if (> (count xs) 0)
    (let [m (mean xs)]
      (/ (reduce + (map #(math/expt (- % m) 2) xs))
         (count xs)))
    0))

(defn sample-variance
  "Calculate sample variance (n-1 denominator)."
  [xs]
  (if (> (count xs) 1)
    (let [m (mean xs)]
      (/ (reduce + (map #(math/expt (- % m) 2) xs))
         (dec (count xs))))
    0))

(defn std-dev
  "Calculate population standard deviation."
  [xs]
  (math/sqrt (variance xs)))

(defn sample-std-dev
  "Calculate sample standard deviation."
  [xs]
  (math/sqrt (sample-variance xs)))

(defn quartiles
  "Calculate quartiles (Q1, Q2, Q3)."
  [xs]
  (if (>= (count xs) 4)
    (let [sorted (sort xs)
          n (count sorted)
          q1-idx (quot n 4)
          q2-idx (quot n 2)
          q3-idx (quot (* 3 n) 4)]
      {:q1 (nth sorted q1-idx)
       :q2 (nth sorted q2-idx)
       :q3 (nth sorted q3-idx)})
    {:q1 nil :q2 nil :q3 nil}))

(defn iqr
  "Calculate interquartile range."
  [xs]
  (let [q (quartiles xs)]
    (if (and (:q1 q) (:q3 q))
      (- (:q3 q) (:q1 q))
      nil)))

(defn skewness
  "Calculate skewness (measure of asymmetry)."
  [xs]
  (if (> (count xs) 2)
    (let [m (mean xs)
          s (std-dev xs)
          n (count xs)]
      (if (> s 0)
        (/ (reduce + (map #(math/expt (/ (- % m) s) 3) xs))
           n)
        0))
    0))

(defn kurtosis
  "Calculate kurtosis (measure of tailedness)."
  [xs]
  (if (> (count xs) 3)
    (let [m (mean xs)
          s (std-dev xs)
          n (count xs)]
      (if (> s 0)
        (- (/ (reduce + (map #(math/expt (/ (- % m) s) 4) xs))
              n)
           3)
        0))
    0))

;; ============================================
;; Correlation Analysis
;; ============================================

(defn covariance
  "Calculate covariance between two variables."
  [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)))
    (let [mx (mean xs)
          my (mean ys)
          n (count xs)]
      (/ (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
         n))
    0))

(defn sample-covariance
  "Calculate sample covariance (n-1 denominator)."
  [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 1))
    (let [mx (mean xs)
          my (mean ys)
          n (count xs)]
      (/ (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
         (dec n)))
    0))

(defn pearson-correlation
  "Calculate Pearson correlation coefficient.
   
   Measures linear relationship between two variables.
   Returns value between -1 and 1."
  [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [cov (covariance xs ys)
          sx (std-dev xs)
          sy (std-dev ys)]
      (if (and (> sx 0) (> sy 0))
        (/ cov (* sx sy))
        0))
    0))

(defn rank
  "Assign ranks to values (for Spearman correlation)."
  [xs]
  (let [sorted-indexed (sort-by first (map-indexed (fn [i x] [x i]) xs))
        ranks (map-indexed (fn [rank [_ orig-idx]] [orig-idx (inc rank)]) sorted-indexed)]
    (mapv second (sort-by first ranks))))

(defn spearman-correlation
  "Calculate Spearman rank correlation coefficient.
   
   Measures monotonic relationship between two variables.
   More robust to outliers than Pearson."
  [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [rx (rank xs)
          ry (rank ys)]
      (pearson-correlation rx ry))
    0))

(defn interpret-correlation
  "Interpret correlation coefficient strength."
  [r]
  (let [abs-r (Math/abs r)]
    (cond
      (>= abs-r 0.9) "very strong"
      (>= abs-r 0.7) "strong"
      (>= abs-r 0.5) "moderate"
      (>= abs-r 0.3) "weak"
      :else "very weak or none")))

(defn correlation-analysis
  "Perform comprehensive correlation analysis."
  [xs ys]
  (let [r (pearson-correlation xs ys)
        rho (spearman-correlation xs ys)]
    {:pearson r
     :spearman rho
     :pearson-interpretation (interpret-correlation r)
     :spearman-interpretation (interpret-correlation rho)
     :r-squared (* r r)
     :sample-size (count xs)
     :recommendation (cond
                       (> (Math/abs (- r rho)) 0.2)
                       "Large difference between Pearson and Spearman suggests non-linear relationship or outliers"
                       
                       (>= (Math/abs r) 0.7)
                       "Strong linear relationship detected"
                       
                       :else
                       "Weak or no linear relationship")}))

;; ============================================
;; Correlation Matrix
;; ============================================

(defn correlation-matrix
  "Calculate correlation matrix for multiple variables.
   
   variables: map of {name -> [values]}"
  [variables]
  (let [names (keys variables)
        n (count names)
        matrix (into {}
                     (for [name-i names
                           name-j names]
                       [(str name-i "_" name-j)
                        (pearson-correlation (get variables name-i)
                                             (get variables name-j))]))]
    {:names (vec names)
     :matrix matrix
     :size n}))

(defn covariance-matrix
  "Calculate covariance matrix for multiple variables."
  [variables]
  (let [names (keys variables)
        n (count names)
        matrix (into {}
                     (for [name-i names
                           name-j names]
                       [(str name-i "_" name-j)
                        (covariance (get variables name-i)
                                    (get variables name-j))]))]
    {:names (vec names)
     :matrix matrix
     :size n}))

;; ============================================
;; Regression Analysis
;; ============================================

(defn linear-regression
  "Perform simple linear regression.
   
   Returns slope, intercept, r-squared, and predictions."
  [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [n (count xs)
          mx (mean xs)
          my (mean ys)
          sxy (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
          sxx (reduce + (map #(math/expt (- % mx) 2) xs))
          slope (if (> sxx 0) (/ sxy sxx) 0)
          intercept (- my (* slope mx))
          predictions (mapv #(+ intercept (* slope %)) xs)
          ss-res (reduce + (map (fn [y pred] (math/expt (- y pred) 2)) ys predictions))
          ss-tot (reduce + (map #(math/expt (- % my) 2) ys))
          r-squared (if (> ss-tot 0) (- 1 (/ ss-res ss-tot)) 0)]
      {:slope slope
       :intercept intercept
       :r-squared r-squared
       :predictions predictions
       :residuals (mapv (fn [y pred] (- y pred)) ys predictions)
       :equation (str "y = " (format "%.4f" (double slope)) "x + " (format "%.4f" (double intercept)))})
    {:error "Insufficient data for regression"}))

(defn predict
  "Make prediction using regression model."
  [model x]
  (+ (:intercept model) (* (:slope model) x)))

;; ============================================
;; Descriptive Statistics
;; ============================================

(defn descriptive-stats
  "Calculate comprehensive descriptive statistics."
  [xs]
  (if (and (seq xs) (> (count xs) 0))
    (let [sorted-xs (sort xs)
          q (quartiles xs)]
      {:count (count xs)
       :mean (mean xs)
       :median (median xs)
       :mode (mode xs)
       :std-dev (sample-std-dev xs)
       :variance (sample-variance xs)
       :min (apply min xs)
       :max (apply max xs)
       :range (- (apply max xs) (apply min xs))
       :q1 (:q1 q)
       :q2 (:q2 q)
       :q3 (:q3 q)
       :iqr (iqr xs)
       :skewness (skewness xs)
       :kurtosis (kurtosis xs)
       :sum (reduce + xs)})
    {:error "No data provided"}))

(defn summary-statistics
  "Calculate summary statistics for multiple variables."
  [variables]
  (into {}
        (map (fn [[name values]]
               [name (descriptive-stats values)])
             variables)))

;; ============================================
;; Hypothesis Testing
;; ============================================

(defn t-test-one-sample
  "One-sample t-test: test if mean differs from mu0."
  [xs mu0]
  (if (> (count xs) 1)
    (let [m (mean xs)
          s (sample-std-dev xs)
          n (count xs)
          se (/ s (math/sqrt n))
          t-stat (if (> se 0) (/ (- m mu0) se) 0)
          df (dec n)]
      {:t-statistic t-stat
       :degrees-of-freedom df
       :sample-mean m
       :hypothesized-mean mu0
       :standard-error se
       :interpretation (if (> (Math/abs t-stat) 2)
                         "Likely significant difference"
                         "May not be significant")})
    {:error "Insufficient data"}))

(defn t-test-two-sample
  "Two-sample t-test: test if means differ."
  [xs ys]
  (if (and (> (count xs) 1) (> (count ys) 1))
    (let [mx (mean xs)
          my (mean ys)
          sx (sample-std-dev xs)
          sy (sample-std-dev ys)
          nx (count xs)
          ny (count ys)
          se (math/sqrt (+ (/ (* sx sx) nx) (/ (* sy sy) ny)))
          t-stat (if (> se 0) (/ (- mx my) se) 0)
          df (+ nx ny -2)]
      {:t-statistic t-stat
       :degrees-of-freedom df
       :mean-difference (- mx my)
       :standard-error se
       :interpretation (if (> (Math/abs t-stat) 2)
                         "Likely significant difference"
                         "May not be significant")})
    {:error "Insufficient data"}))

;; ============================================
;; Mental Model Statistical Analysis
;; ============================================

(defn analyze-model-effectiveness
  "Analyze statistical effectiveness of mental model applications.
   
   model-applications: list of {:model :confidence :context}
   outcomes: list of {:success :magnitude}"
  [model-applications outcomes]
  (let [confidences (map #(get % :confidence 0.5) model-applications)
        successes (map #(if (get % :success false) 1 0) outcomes)
        magnitudes (map #(get % :magnitude 0) outcomes)]
    {:confidence-outcome-correlation (pearson-correlation confidences successes)
     :confidence-magnitude-correlation (pearson-correlation confidences magnitudes)
     :average-confidence (mean confidences)
     :success-rate (mean successes)
     :average-magnitude (mean magnitudes)
     :interpretation "Higher correlation suggests model is predictive"}))

(defn cross-model-analysis
  "Analyze relationships between different mental models' predictions."
  [model-results]
  (let [model-names (distinct (map :model model-results))
        by-model (into {}
                       (map (fn [name]
                              [name (map #(get % :confidence 0.5)
                                         (filter #(= (:model %) name) model-results))])
                            model-names))]
    {:models-analyzed model-names
     :correlations (correlation-matrix by-model)
     :summary (summary-statistics by-model)}))

;; ============================================
;; Comprehensive Statistical Analysis
;; ============================================

(defn comprehensive-statistical-analysis
  "Perform comprehensive statistical analysis on data.
   
   data: map of {variable-name -> [values]}"
  [data]
  {:descriptive (summary-statistics data)
   :correlations (correlation-matrix data)
   :covariances (covariance-matrix data)
   :timestamp (java.time.Instant/now)})
