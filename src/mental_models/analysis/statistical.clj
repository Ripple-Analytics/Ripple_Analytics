(ns mental-models.analysis.statistical
  "Statistical Analysis Engine - Electric Clojure
   Replaces Python statistical.py and statistical_engine.py
   Descriptive and inferential statistics for mental model analysis"
  (:require [clojure.math :as math]
            [taoensso.timbre :as log]))

;; -- Descriptive Statistics --

(defn mean
  "Calculate arithmetic mean"
  [values]
  (if (empty? values)
    0.0
    (/ (reduce + values) (count values))))

(defn median
  "Calculate median"
  [values]
  (let [sorted (sort values)
        n (count sorted)
        mid (/ n 2)]
    (if (even? n)
      (/ (+ (nth sorted (dec mid)) (nth sorted mid)) 2)
      (nth sorted (int mid)))))

(defn mode
  "Calculate mode (most frequent value)"
  [values]
  (let [frequencies (frequencies values)]
    (key (apply max-key val frequencies))))

(defn variance
  "Calculate variance"
  [values]
  (let [m (mean values)
        squared-diffs (map #(math/pow (- % m) 2) values)]
    (/ (reduce + squared-diffs) (count values))))

(defn std-dev
  "Calculate standard deviation"
  [values]
  (math/sqrt (variance values)))

(defn quartiles
  "Calculate Q1, Q2 (median), Q3"
  [values]
  (let [sorted (sort values)
        n (count sorted)
        q2 (median values)
        q1 (median (take (int (/ n 2)) sorted))
        q3 (median (drop (int (/ n 2)) sorted))]
    {:q1 q1 :q2 q2 :q3 q3
     :iqr (- q3 q1)}))

(defn percentile
  "Calculate nth percentile"
  [values n]
  (let [sorted (sort values)
        idx (int (* (/ n 100) (count sorted)))]
    (nth sorted (min (dec (count sorted)) idx))))

;; -- Correlation & Covariance --

(defn covariance
  "Calculate covariance between two variables"
  [x y]
  (let [mean-x (mean x)
        mean-y (mean y)
        products (map * (map #(- % mean-x) x) (map #(- % mean-y) y))]
    (/ (reduce + products) (count x))))

(defn correlation
  "Calculate Pearson correlation coefficient"
  [x y]
  (let [cov (covariance x y)
        std-x (std-dev x)
        std-y (std-dev y)]
    (if (or (zero? std-x) (zero? std-y))
      0.0
      (/ cov (* std-x std-y)))))

(defn autocorrelation
  "Calculate autocorrelation at lag k"
  [values lag]
  (let [n (count values)
        mean-val (mean values)
        c0 (/ (reduce + (map #(math/pow (- % mean-val) 2) values)) n)
        ck (/ (reduce + (map-indexed (fn [i v]
                                      (if (< (+ i lag) n)
                                        (* (- v mean-val) (- (nth values (+ i lag)) mean-val))
                                        0))
                                    values))
             n)]
    (if (zero? c0) 0.0 (/ ck c0))))

;; -- Hypothesis Testing --

(defn z-score
  "Calculate z-score"
  [value mean std-dev]
  (if (zero? std-dev)
    0.0
    (/ (- value mean) std-dev)))

(defn t-statistic
  "Calculate t-statistic for sample mean"
  [sample-mean population-mean sample-std-dev sample-size]
  (if (zero? sample-std-dev)
    0.0
    (/ (- sample-mean population-mean)
       (/ sample-std-dev (math/sqrt sample-size)))))

(defn chi-square
  "Calculate chi-square statistic"
  [observed expected]
  (reduce + (map (fn [o e]
                  (if (zero? e)
                    0.0
                    (/ (math/pow (- o e) 2) e)))
                observed expected)))

;; -- Distributions --

(defn normal-pdf
  "Probability density function for normal distribution"
  [x mean std-dev]
  (let [variance (math/pow std-dev 2)
        coefficient (/ 1 (math/sqrt (* 2 math/PI variance)))
        exponent (- (/ (math/pow (- x mean) 2) (* 2 variance)))]
    (* coefficient (math/exp exponent))))

(defn normal-cdf
  "Cumulative distribution function for normal distribution (approximation)"
  [x mean std-dev]
  (let [z (/ (- x mean) std-dev)
        ;; Approximation using error function
        a1 0.254829592
        a2 -0.284496736
        a3 1.421413741
        a4 -1.453152027
        a5 1.061405429
        p 0.3275911
        
        sign (if (< z 0) -1 1)
        z-abs (math/abs z)
        
        t (/ 1 (+ 1 (* p z-abs)))
        y (- 1 (* (+ (* (+ (* (+ (* (+ a1 (* a2 t)) a3) t) a4) t) a5) t)
                  (math/exp (- (math/pow z-abs 2)))))]
    
    (+ 0.5 (* sign (/ y 2)))))

(defn exponential-pdf
  "Probability density function for exponential distribution"
  [x lambda]
  (if (< x 0)
    0.0
    (* lambda (math/exp (* -1 lambda x)))))

;; -- Time Series Analysis --

(defn moving-average
  "Calculate moving average"
  [values window-size]
  (mapv (fn [i]
         (let [window (subvec values i (min (+ i window-size) (count values)))]
           (mean window)))
       (range (- (count values) window-size -1))))

(defn exponential-smoothing
  "Calculate exponential smoothing"
  [values alpha]
  (reduce (fn [result v]
           (let [last-smoothed (last result)
                 smoothed (+ (* alpha v) (* (- 1 alpha) last-smoothed))]
             (conj result smoothed)))
         [(first values)]
         (rest values)))

(defn trend
  "Calculate linear trend"
  [values]
  (let [n (count values)
        x-values (range 1 (inc n))
        x-mean (mean x-values)
        y-mean (mean values)
        
        numerator (reduce + (map * (map #(- % x-mean) x-values)
                                  (map #(- % y-mean) values)))
        denominator (reduce + (map #(math/pow (- % x-mean) 2) x-values))]
    
    (if (zero? denominator)
      0.0
      (/ numerator denominator))))

;; -- Outlier Detection --

(defn z-score-outliers
  "Detect outliers using z-score method"
  [values threshold]
  (let [m (mean values)
        s (std-dev values)]
    (filter (fn [v]
             (> (math/abs (z-score v m s)) threshold))
           values)))

(defn iqr-outliers
  "Detect outliers using IQR method"
  [values]
  (let [{:keys [q1 q3 iqr]} (quartiles values)
        lower-bound (- q1 (* 1.5 iqr))
        upper-bound (+ q3 (* 1.5 iqr))]
    (filter (fn [v]
             (or (< v lower-bound) (> v upper-bound)))
           values)))

;; -- Clustering --

(defn euclidean-distance
  "Calculate Euclidean distance between two points"
  [p1 p2]
  (math/sqrt (reduce + (map #(math/pow (- %1 %2) 2) p1 p2))))

(defn manhattan-distance
  "Calculate Manhattan distance between two points"
  [p1 p2]
  (reduce + (map #(math/abs (- %1 %2)) p1 p2)))

(defn centroid
  "Calculate centroid of points"
  [points]
  (let [n (count points)
        d (count (first points))]
    (mapv (fn [i]
           (/ (reduce + (map #(nth % i) points)) n))
         (range d))))

;; -- Regression --

(defn linear-regression
  "Calculate linear regression coefficients (slope and intercept)"
  [x y]
  (let [n (count x)
        x-mean (mean x)
        y-mean (mean y)
        
        numerator (reduce + (map * (map #(- % x-mean) x)
                                  (map #(- % y-mean) y)))
        denominator (reduce + (map #(math/pow (- % x-mean) 2) x))
        
        slope (if (zero? denominator) 0.0 (/ numerator denominator))
        intercept (- y-mean (* slope x-mean))]
    
    {:slope slope
     :intercept intercept
     :equation (str "y = " (format "%.4f" slope) "x + " (format "%.4f" intercept))}))

(defn r-squared
  "Calculate R-squared (coefficient of determination)"
  [y y-predicted]
  (let [ss-res (reduce + (map #(math/pow (- %1 %2) 2) y y-predicted))
        ss-tot (reduce + (map #(math/pow (- % (mean y)) 2) y))]
    (if (zero? ss-tot)
      0.0
      (- 1 (/ ss-res ss-tot)))))

;; -- Probability Calculations --

(defn binomial-probability
  "Calculate binomial probability P(X=k)"
  [n k p]
  (let [combination (/ (math/factorial n) (* (math/factorial k) (math/factorial (- n k))))
        prob (math/pow p k)
        prob-not (math/pow (- 1 p) (- n k))]
    (* combination prob prob-not)))

(defn poisson-probability
  "Calculate Poisson probability P(X=k)"
  [lambda k]
  (/ (* (math/pow lambda k) (math/exp (* -1 lambda)))
     (math/factorial k)))

;; -- Utilities --

(defn normalize
  "Normalize values to 0-1 range"
  [values]
  (let [min-val (apply min values)
        max-val (apply max values)
        range (- max-val min-val)]
    (if (zero? range)
      (vec (repeat (count values) 0.5))
      (mapv #(/ (- % min-val) range) values))))

(defn standardize
  "Standardize values (z-score normalization)"
  [values]
  (let [m (mean values)
        s (std-dev values)]
    (if (zero? s)
      (vec (repeat (count values) 0.0))
      (mapv #(/ (- % m) s) values))))

(defn summary-statistics
  "Calculate comprehensive summary statistics"
  [values]
  {:count (count values)
   :mean (mean values)
   :median (median values)
   :mode (mode values)
   :std-dev (std-dev values)
   :variance (variance values)
   :min (apply min values)
   :max (apply max values)
   :range (- (apply max values) (apply min values))
   :quartiles (quartiles values)})
