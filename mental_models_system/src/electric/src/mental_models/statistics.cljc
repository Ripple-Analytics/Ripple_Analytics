(ns mental-models.statistics
  "Statistical Engine - Electric Clojure
   
   Comprehensive statistical analysis functions.
   Runs on both client and server for reactive statistics."
  #?(:clj (:require [clojure.math.numeric-tower :as math])
     :cljs (:require [goog.math :as math])))

;; ============================================
;; Math Helpers (cross-platform)
;; ============================================

(defn sqrt [x]
  #?(:clj (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn pow [x n]
  #?(:clj (Math/pow x n)
     :cljs (js/Math.pow x n)))

(defn abs [x]
  #?(:clj (Math/abs x)
     :cljs (js/Math.abs x)))

;; ============================================
;; Basic Statistical Functions
;; ============================================

(defn mean [xs]
  (if (seq xs)
    (/ (reduce + xs) (count xs))
    0))

(defn median [xs]
  (if (seq xs)
    (let [sorted (sort xs)
          n (count sorted)
          mid (quot n 2)]
      (if (odd? n)
        (nth sorted mid)
        (/ (+ (nth sorted mid) (nth sorted (dec mid))) 2)))
    0))

(defn mode [xs]
  (if (seq xs)
    (->> xs
         frequencies
         (sort-by val >)
         first
         key)
    nil))

(defn variance [xs]
  (if (> (count xs) 0)
    (let [m (mean xs)]
      (/ (reduce + (map #(pow (- % m) 2) xs))
         (count xs)))
    0))

(defn sample-variance [xs]
  (if (> (count xs) 1)
    (let [m (mean xs)]
      (/ (reduce + (map #(pow (- % m) 2) xs))
         (dec (count xs))))
    0))

(defn std-dev [xs] (sqrt (variance xs)))
(defn sample-std-dev [xs] (sqrt (sample-variance xs)))

(defn quartiles [xs]
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

(defn iqr [xs]
  (let [q (quartiles xs)]
    (when (and (:q1 q) (:q3 q))
      (- (:q3 q) (:q1 q)))))

;; ============================================
;; Correlation Analysis
;; ============================================

(defn covariance [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)))
    (let [mx (mean xs)
          my (mean ys)
          n (count xs)]
      (/ (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
         n))
    0))

(defn pearson-correlation [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [cov (covariance xs ys)
          sx (std-dev xs)
          sy (std-dev ys)]
      (if (and (> sx 0) (> sy 0))
        (/ cov (* sx sy))
        0))
    0))

(defn rank [xs]
  (let [sorted-indexed (sort-by first (map-indexed (fn [i x] [x i]) xs))
        ranks (map-indexed (fn [rank [_ orig-idx]] [orig-idx (inc rank)]) sorted-indexed)]
    (mapv second (sort-by first ranks))))

(defn spearman-correlation [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [rx (rank xs)
          ry (rank ys)]
      (pearson-correlation rx ry))
    0))

(defn interpret-correlation [r]
  (let [abs-r (abs r)]
    (cond
      (>= abs-r 0.9) "very strong"
      (>= abs-r 0.7) "strong"
      (>= abs-r 0.5) "moderate"
      (>= abs-r 0.3) "weak"
      :else "very weak or none")))

(defn correlation-analysis [xs ys]
  (let [r (pearson-correlation xs ys)
        rho (spearman-correlation xs ys)]
    {:pearson r
     :spearman rho
     :pearson-interpretation (interpret-correlation r)
     :spearman-interpretation (interpret-correlation rho)
     :r-squared (* r r)
     :sample-size (count xs)}))

;; ============================================
;; Regression Analysis
;; ============================================

(defn linear-regression [xs ys]
  (if (and (seq xs) (seq ys) (= (count xs) (count ys)) (> (count xs) 2))
    (let [n (count xs)
          mx (mean xs)
          my (mean ys)
          sxy (reduce + (map (fn [x y] (* (- x mx) (- y my))) xs ys))
          sxx (reduce + (map #(pow (- % mx) 2) xs))
          slope (if (> sxx 0) (/ sxy sxx) 0)
          intercept (- my (* slope mx))
          predictions (mapv #(+ intercept (* slope %)) xs)
          ss-res (reduce + (map (fn [y pred] (pow (- y pred) 2)) ys predictions))
          ss-tot (reduce + (map #(pow (- % my) 2) ys))
          r-squared (if (> ss-tot 0) (- 1 (/ ss-res ss-tot)) 0)]
      {:slope slope
       :intercept intercept
       :r-squared r-squared
       :predictions predictions
       :residuals (mapv (fn [y pred] (- y pred)) ys predictions)})
    {:error "Insufficient data for regression"}))

;; ============================================
;; Descriptive Statistics
;; ============================================

(defn descriptive-stats [xs]
  (if (and (seq xs) (> (count xs) 0))
    (let [q (quartiles xs)]
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
       :sum (reduce + xs)})
    {:error "No data provided"}))

(defn summary-statistics [variables]
  (into {}
        (map (fn [[name values]]
               [name (descriptive-stats values)])
             variables)))

;; ============================================
;; Hypothesis Testing
;; ============================================

(defn t-test-one-sample [xs mu0]
  (if (> (count xs) 1)
    (let [m (mean xs)
          s (sample-std-dev xs)
          n (count xs)
          se (/ s (sqrt n))
          t-stat (if (> se 0) (/ (- m mu0) se) 0)
          df (dec n)]
      {:t-statistic t-stat
       :degrees-of-freedom df
       :sample-mean m
       :hypothesized-mean mu0
       :standard-error se
       :interpretation (if (> (abs t-stat) 2)
                         "Likely significant difference"
                         "May not be significant")})
    {:error "Insufficient data"}))

(defn t-test-two-sample [xs ys]
  (if (and (> (count xs) 1) (> (count ys) 1))
    (let [mx (mean xs)
          my (mean ys)
          sx (sample-std-dev xs)
          sy (sample-std-dev ys)
          nx (count xs)
          ny (count ys)
          se (sqrt (+ (/ (* sx sx) nx) (/ (* sy sy) ny)))
          t-stat (if (> se 0) (/ (- mx my) se) 0)
          df (+ nx ny -2)]
      {:t-statistic t-stat
       :degrees-of-freedom df
       :mean-difference (- mx my)
       :standard-error se
       :interpretation (if (> (abs t-stat) 2)
                         "Likely significant difference"
                         "May not be significant")})
    {:error "Insufficient data"}))

;; ============================================
;; Comprehensive Analysis
;; ============================================

(defn comprehensive-statistical-analysis [data]
  {:descriptive (summary-statistics data)
   :correlations (when (>= (count data) 2)
                   (let [vars (vec (vals data))]
                     (when (and (first vars) (second vars))
                       (correlation-analysis (first vars) (second vars)))))})
