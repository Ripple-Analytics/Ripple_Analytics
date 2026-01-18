;; Statistical Analysis Engine in Hy (Lisp)
;; Comprehensive statistical functions for mental models analysis
;; 
;; This module provides:
;; - Correlation analysis (Pearson, Spearman, Kendall)
;; - Regression analysis (linear, multiple)
;; - Factor analysis
;; - Covariance analysis
;; - Descriptive statistics
;; - Hypothesis testing

(import typing [Dict List Any Optional Tuple Union])
(import datetime [datetime])
(import functools [reduce])
(import collections [Counter])
(import math)

;; Try to import numpy/scipy, fall back to pure Lisp implementations
(setv NUMPY_AVAILABLE False)
(setv SCIPY_AVAILABLE False)

(try
  (import numpy :as np)
  (setv NUMPY_AVAILABLE True)
  (except [ImportError]
    None))

(try
  (import scipy.stats :as stats)
  (setv SCIPY_AVAILABLE True)
  (except [ImportError]
    None))

;; ============================================
;; Pure Lisp Statistical Functions
;; (Used when numpy/scipy not available)
;; ============================================

(defn mean [xs]
  "Calculate arithmetic mean."
  (if xs
      (/ (sum xs) (len xs))
      0.0))

(defn variance [xs]
  "Calculate population variance."
  (if (> (len xs) 1)
      (let [m (mean xs)]
        (/ (sum (lfor x xs (** (- x m) 2)))
           (len xs)))
      0.0))

(defn std-dev [xs]
  "Calculate population standard deviation."
  (** (variance xs) 0.5))

(defn sample-variance [xs]
  "Calculate sample variance (Bessel's correction)."
  (if (> (len xs) 1)
      (let [m (mean xs)]
        (/ (sum (lfor x xs (** (- x m) 2)))
           (- (len xs) 1)))
      0.0))

(defn sample-std-dev [xs]
  "Calculate sample standard deviation."
  (** (sample-variance xs) 0.5))

(defn covariance [xs ys]
  "Calculate covariance between two variables."
  (if (and xs ys (= (len xs) (len ys)) (> (len xs) 1))
      (let [mx (mean xs)
            my (mean ys)
            n (len xs)]
        (/ (sum (lfor [x y] (zip xs ys)
                     (* (- x mx) (- y my))))
           (- n 1)))
      0.0))

(defn pearson-correlation [xs ys]
  "Calculate Pearson correlation coefficient."
  (if (and xs ys (= (len xs) (len ys)) (> (len xs) 2))
      (let [cov (covariance xs ys)
            sx (sample-std-dev xs)
            sy (sample-std-dev ys)]
        (if (and (> sx 0) (> sy 0))
            (/ cov (* sx sy))
            0.0))
      0.0))

(defn rank [xs]
  "Assign ranks to values (for Spearman correlation)."
  (let [sorted-with-idx (sorted (enumerate xs) :key (fn [x] (get x 1)))
        ranks (dict)]
    (for [[rank [idx val]] (enumerate sorted-with-idx :start 1)]
      (setv (get ranks idx) rank))
    (lfor i (range (len xs)) (get ranks i))))

(defn spearman-correlation [xs ys]
  "Calculate Spearman rank correlation coefficient."
  (if (and xs ys (= (len xs) (len ys)) (> (len xs) 2))
      (let [rx (rank xs)
            ry (rank ys)]
        (pearson-correlation rx ry))
      0.0))

(defn median [xs]
  "Calculate median."
  (if xs
      (let [sorted-xs (sorted xs)
            n (len sorted-xs)
            mid (// n 2)]
        (if (= (% n 2) 0)
            (/ (+ (get sorted-xs (- mid 1)) (get sorted-xs mid)) 2)
            (get sorted-xs mid)))
      0.0))

(defn percentile [xs p]
  "Calculate p-th percentile."
  (if xs
      (let [sorted-xs (sorted xs)
            n (len sorted-xs)
            k (* (/ p 100) (- n 1))
            f (math.floor k)
            c (math.ceil k)]
        (if (= f c)
            (get sorted-xs (int f))
            (+ (* (get sorted-xs (int f)) (- c k))
               (* (get sorted-xs (int c)) (- k f)))))
      0.0))

(defn quartiles [xs]
  "Calculate quartiles (Q1, Q2, Q3)."
  {"q1" (percentile xs 25)
   "q2" (percentile xs 50)
   "q3" (percentile xs 75)})

(defn iqr [xs]
  "Calculate interquartile range."
  (let [q (quartiles xs)]
    (- (get q "q3") (get q "q1"))))

(defn skewness [xs]
  "Calculate skewness (measure of asymmetry)."
  (if (> (len xs) 2)
      (let [m (mean xs)
            s (sample-std-dev xs)
            n (len xs)]
        (if (> s 0)
            (* (/ n (* (- n 1) (- n 2)))
               (sum (lfor x xs (** (/ (- x m) s) 3))))
            0.0))
      0.0))

(defn kurtosis [xs]
  "Calculate kurtosis (measure of tailedness)."
  (if (> (len xs) 3)
      (let [m (mean xs)
            s (sample-std-dev xs)
            n (len xs)]
        (if (> s 0)
            (- (* (/ (* n (+ n 1))
                    (* (- n 1) (- n 2) (- n 3)))
                  (sum (lfor x xs (** (/ (- x m) s) 4))))
               (/ (* 3 (** (- n 1) 2))
                  (* (- n 2) (- n 3))))
            0.0))
      0.0))

;; ============================================
;; Correlation Analysis
;; ============================================

(defn correlation-matrix [variables]
  "Calculate correlation matrix for multiple variables.
   
   variables: dict of {name: [values]}
   Returns: matrix of correlations"
  (let [names (list (.keys variables))
        n (len names)
        matrix {}]
    (for [i (range n)]
      (for [j (range n)]
        (let [name-i (get names i)
              name-j (get names j)
              key (+ name-i "_" name-j)]
          (setv (get matrix key)
                (if (= i j)
                    1.0
                    (pearson-correlation 
                      (get variables name-i)
                      (get variables name-j)))))))
    {"names" names
     "matrix" matrix
     "size" n}))

(defn correlation-with-significance [xs ys #** kwargs]
  "Calculate correlation with statistical significance."
  (let [r (pearson-correlation xs ys)
        n (len xs)
        t-stat (if (and (< (abs r) 1) (> n 2))
                   (/ (* r (** (- n 2) 0.5))
                      (** (- 1 (* r r)) 0.5))
                   0)
        df (- n 2)]
    {"correlation" r
     "t_statistic" t-stat
     "degrees_of_freedom" df
     "sample_size" n
     "interpretation" (interpret-correlation r)}))

(defn interpret-correlation [r]
  "Interpret correlation coefficient strength."
  (let [abs-r (abs r)]
    (cond
      (>= abs-r 0.9) "very strong"
      (>= abs-r 0.7) "strong"
      (>= abs-r 0.5) "moderate"
      (>= abs-r 0.3) "weak"
      True "very weak or none")))

;; ============================================
;; Regression Analysis
;; ============================================

(defn linear-regression [xs ys]
  "Perform simple linear regression.
   
   Returns slope, intercept, r-squared, and predictions."
  (if (and xs ys (= (len xs) (len ys)) (> (len xs) 2))
      (let [n (len xs)
            mx (mean xs)
            my (mean ys)
            sxy (sum (lfor [x y] (zip xs ys) (* (- x mx) (- y my))))
            sxx (sum (lfor x xs (** (- x mx) 2)))
            slope (if (> sxx 0) (/ sxy sxx) 0)
            intercept (- my (* slope mx))
            predictions (lfor x xs (+ intercept (* slope x)))
            ss-res (sum (lfor [y pred] (zip ys predictions) (** (- y pred) 2)))
            ss-tot (sum (lfor y ys (** (- y my) 2)))
            r-squared (if (> ss-tot 0) (- 1 (/ ss-res ss-tot)) 0)]
        {"slope" slope
         "intercept" intercept
         "r_squared" r-squared
         "predictions" predictions
         "residuals" (lfor [y pred] (zip ys predictions) (- y pred))
         "equation" (+ "y = " (str (round slope 4)) "x + " (str (round intercept 4)))})
      {"error" "Insufficient data for regression"}))

(defn multiple-regression [X y]
  "Perform multiple linear regression using normal equations.
   
   X: list of lists (each inner list is a predictor variable)
   y: dependent variable"
  (if NUMPY_AVAILABLE
      (multiple-regression-numpy X y)
      (multiple-regression-pure X y)))

(defn multiple-regression-numpy [X y]
  "Multiple regression using numpy."
  (let [X-arr (np.array X)
        y-arr (np.array y)
        X-with-intercept (np.column_stack [(np.ones (len y)) X-arr.T])
        coeffs (np.linalg.lstsq X-with-intercept y-arr :rcond None)
        betas (get coeffs 0)
        predictions (np.dot X-with-intercept betas)
        residuals (- y-arr predictions)
        ss-res (np.sum (** residuals 2))
        ss-tot (np.sum (** (- y-arr (np.mean y-arr)) 2))
        r-squared (if (> ss-tot 0) (- 1 (/ ss-res ss-tot)) 0)]
    {"coefficients" (.tolist betas)
     "intercept" (get betas 0)
     "slopes" (.tolist (cut betas 1))
     "r_squared" (float r-squared)
     "predictions" (.tolist predictions)
     "residuals" (.tolist residuals)}))

(defn multiple-regression-pure [X y]
  "Multiple regression without numpy (simplified)."
  {"error" "Multiple regression requires numpy"
   "suggestion" "Install numpy: pip install numpy"})

;; ============================================
;; Factor Analysis (Simplified)
;; ============================================

(defn principal-components [variables #** kwargs]
  "Simplified principal component analysis.
   
   For full PCA, use scipy/sklearn."
  (let [n-components (get kwargs "n_components" 2)
        corr (correlation-matrix variables)
        names (get corr "names")]
    {"method" "correlation-based"
     "variables" names
     "correlation_matrix" (get corr "matrix")
     "note" "For full PCA, install scipy and sklearn"
     "interpretation" "High correlations suggest underlying factors"}))

(defn factor-loadings [variables factors]
  "Calculate factor loadings (correlations between variables and factors)."
  (let [loadings {}]
    (for [[var-name var-values] (.items variables)]
      (setv (get loadings var-name)
            (lfor [factor-name factor-values] (.items factors)
                 {"factor" factor-name
                  "loading" (pearson-correlation var-values factor-values)})))
    loadings))

;; ============================================
;; Covariance Analysis
;; ============================================

(defn covariance-matrix [variables]
  "Calculate covariance matrix for multiple variables."
  (let [names (list (.keys variables))
        n (len names)
        matrix {}]
    (for [i (range n)]
      (for [j (range n)]
        (let [name-i (get names i)
              name-j (get names j)
              key (+ name-i "_" name-j)]
          (setv (get matrix key)
                (covariance 
                  (get variables name-i)
                  (get variables name-j))))))
    {"names" names
     "matrix" matrix
     "size" n}))

(defn analyze-covariates [dependent independent covariates]
  "Analyze relationship controlling for covariates."
  (let [raw-corr (pearson-correlation dependent independent)
        partial-corr (partial-correlation dependent independent covariates)]
    {"raw_correlation" raw-corr
     "partial_correlation" partial-corr
     "covariate_effect" (- raw-corr partial-corr)
     "interpretation" (if (> (abs (- raw-corr partial-corr)) 0.1)
                        "Covariates have substantial effect"
                        "Covariates have minimal effect")}))

(defn partial-correlation [x y covariates]
  "Calculate partial correlation controlling for covariates."
  (if covariates
      (let [x-resid (residualize x covariates)
            y-resid (residualize y covariates)]
        (pearson-correlation x-resid y-resid))
      (pearson-correlation x y)))

(defn residualize [var covariates]
  "Get residuals after regressing var on covariates."
  (if (and covariates (> (len covariates) 0))
      (let [first-cov (get (list (.values covariates)) 0)
            reg (linear-regression first-cov var)]
        (get reg "residuals" var))
      var))

;; ============================================
;; Descriptive Statistics
;; ============================================

(defn descriptive-stats [xs]
  "Calculate comprehensive descriptive statistics."
  (if (and xs (> (len xs) 0))
      (let [sorted-xs (sorted xs)
            q (quartiles xs)]
        {"count" (len xs)
         "mean" (mean xs)
         "median" (median xs)
         "mode" (mode xs)
         "std_dev" (sample-std-dev xs)
         "variance" (sample-variance xs)
         "min" (min xs)
         "max" (max xs)
         "range" (- (max xs) (min xs))
         "q1" (get q "q1")
         "q2" (get q "q2")
         "q3" (get q "q3")
         "iqr" (iqr xs)
         "skewness" (skewness xs)
         "kurtosis" (kurtosis xs)
         "sum" (sum xs)})
      {"error" "No data provided"}))

(defn mode [xs]
  "Calculate mode (most frequent value)."
  (if xs
      (let [counts (Counter xs)
            most-common (.most_common counts 1)]
        (if most-common
            (get (get most-common 0) 0)
            None))
      None))

(defn summary-statistics [variables]
  "Calculate summary statistics for multiple variables."
  (let [summaries {}]
    (for [[name values] (.items variables)]
      (setv (get summaries name) (descriptive-stats values)))
    summaries))

;; ============================================
;; Hypothesis Testing (Simplified)
;; ============================================

(defn t-test-one-sample [xs mu0]
  "One-sample t-test: test if mean differs from mu0."
  (if (> (len xs) 1)
      (let [m (mean xs)
            s (sample-std-dev xs)
            n (len xs)
            se (/ s (** n 0.5))
            t-stat (if (> se 0) (/ (- m mu0) se) 0)
            df (- n 1)]
        {"t_statistic" t-stat
         "degrees_of_freedom" df
         "sample_mean" m
         "hypothesized_mean" mu0
         "standard_error" se
         "interpretation" (if (> (abs t-stat) 2)
                           "Likely significant difference"
                           "May not be significant")})
      {"error" "Insufficient data"}))

(defn t-test-two-sample [xs ys]
  "Two-sample t-test: test if means differ."
  (if (and (> (len xs) 1) (> (len ys) 1))
      (let [mx (mean xs)
            my (mean ys)
            sx (sample-std-dev xs)
            sy (sample-std-dev ys)
            nx (len xs)
            ny (len ys)
            se (** (+ (/ (* sx sx) nx) (/ (* sy sy) ny)) 0.5)
            t-stat (if (> se 0) (/ (- mx my) se) 0)
            df (+ nx ny -2)]
        {"t_statistic" t-stat
         "degrees_of_freedom" df
         "mean_difference" (- mx my)
         "standard_error" se
         "interpretation" (if (> (abs t-stat) 2)
                           "Likely significant difference"
                           "May not be significant")})
      {"error" "Insufficient data"}))

;; ============================================
;; Mental Model Statistical Analysis
;; ============================================

(defn analyze-model-effectiveness [model-applications outcomes]
  "Analyze statistical effectiveness of mental model applications.
   
   model-applications: list of {model, confidence, context}
   outcomes: list of {success, magnitude}"
  (let [confidences (lfor app model-applications (get app "confidence" 0.5))
        successes (lfor out outcomes (if (get out "success" False) 1 0))
        magnitudes (lfor out outcomes (get out "magnitude" 0))]
    {"confidence_outcome_correlation" (pearson-correlation confidences successes)
     "confidence_magnitude_correlation" (pearson-correlation confidences magnitudes)
     "average_confidence" (mean confidences)
     "success_rate" (mean successes)
     "average_magnitude" (mean magnitudes)
     "interpretation" "Higher correlation suggests model is predictive"}))

(defn cross-model-analysis [model-results]
  "Analyze relationships between different mental models' predictions."
  (let [model-names (list (set (lfor r model-results (get r "model"))))
        by-model {}]
    (for [name model-names]
      (setv (get by-model name)
            (lfor r model-results
                 :if (= (get r "model") name)
                 (get r "confidence" 0.5))))
    {"models_analyzed" model-names
     "correlations" (correlation-matrix by-model)
     "summary" (summary-statistics by-model)}))

;; ============================================
;; Export Functions
;; ============================================

(defn comprehensive-statistical-analysis [data]
  "Perform comprehensive statistical analysis on data.
   
   data: dict of {variable_name: [values]}"
  {"descriptive" (summary-statistics data)
   "correlations" (correlation-matrix data)
   "covariances" (covariance-matrix data)
   "timestamp" (str (datetime.now))
   "numpy_available" NUMPY_AVAILABLE
   "scipy_available" SCIPY_AVAILABLE})
