(ns mental-models.analysis.hmm
  "Hidden Markov Model Engine - Electric Clojure
   Replaces Python hmm_regime.py
   Detects regime changes and state transitions in mental model application"
  (:require [clojure.math :as math]
            [taoensso.timbre :as log]))

;; -- HMM Data Structures --

(defrecord HMMState
  [id name description])

(defrecord HMMModel
  [states transition-matrix emission-matrix initial-distribution])

(defrecord ForwardBackwardResult
  [forward-probabilities backward-probabilities likelihood])

(defrecord ViterbiPath
  [states probabilities likelihood])

;; -- Transition Matrix Operations --

(defn create-transition-matrix
  "Create n×n transition matrix"
  [n initial-value]
  (vec (repeat n (vec (repeat n initial-value)))))

(defn normalize-row
  "Normalize row to sum to 1"
  [row]
  (let [total (reduce + row)]
    (if (zero? total)
      (vec (repeat (count row) (/ 1.0 (count row))))
      (mapv #(/ % total) row))))

(defn normalize-matrix
  "Normalize transition matrix rows"
  [matrix]
  (mapv normalize-row matrix))

;; -- Forward Algorithm --

(defn forward-algorithm
  "Calculate forward probabilities
   α_t(i) = P(O_1...O_t, S_t=i | λ)"
  [observations emission-matrix transition-matrix initial-dist]
  (let [n-states (count initial-dist)
        n-obs (count observations)
        
        ;; Initialize alpha for t=0
        alpha-0 (vec (for [i (range n-states)]
                      (* (nth initial-dist i)
                         (nth (nth emission-matrix i) (nth observations 0)))))
        
        ;; Recursively calculate alpha for t=1..T
        alpha (reduce (fn [alphas t]
                       (let [prev-alpha (last alphas)
                             obs-t (nth observations t)
                             alpha-t (vec (for [j (range n-states)]
                                          (let [sum (reduce + (for [i (range n-states)]
                                                              (* (nth prev-alpha i)
                                                                 (nth (nth transition-matrix i) j))))]
                                            (* sum (nth (nth emission-matrix j) obs-t)))))]
                         (conj alphas alpha-t)))
                     [alpha-0]
                     (range 1 n-obs))]
    
    {:forward alpha
     :likelihood (reduce + (last alpha))}))

;; -- Backward Algorithm --

(defn backward-algorithm
  "Calculate backward probabilities
   β_t(i) = P(O_{t+1}...O_T | S_t=i, λ)"
  [observations emission-matrix transition-matrix]
  (let [n-states (count transition-matrix)
        n-obs (count observations)
        
        ;; Initialize beta for t=T (all 1.0)
        beta-T (vec (repeat n-states 1.0))
        
        ;; Recursively calculate beta for t=T-1..0
        beta (reduce (fn [betas t]
                      (let [prev-beta (first betas)
                            obs-next (nth observations (inc t))
                            beta-t (vec (for [i (range n-states)]
                                        (reduce + (for [j (range n-states)]
                                                  (* (nth (nth transition-matrix i) j)
                                                     (nth (nth emission-matrix j) obs-next)
                                                     (nth prev-beta j))))))]
                        (conj betas beta-t)))
                     [beta-T]
                     (range (dec n-obs) -1 -1))]
    
    {:backward (reverse beta)}))

;; -- Viterbi Algorithm --

(defn viterbi-algorithm
  "Find most likely sequence of states
   Returns path with highest probability"
  [observations emission-matrix transition-matrix initial-dist]
  (let [n-states (count initial-dist)
        n-obs (count observations)
        
        ;; Initialize viterbi for t=0
        viterbi-0 (vec (for [i (range n-states)]
                        (* (nth initial-dist i)
                           (nth (nth emission-matrix i) (nth observations 0)))))
        path-0 (vec (repeat n-states [i]))
        
        ;; Recursively calculate viterbi for t=1..T
        [viterbi-final path-final]
        (reduce (fn [[viterbi paths] t]
                 (let [obs-t (nth observations t)
                       viterbi-t (vec (for [j (range n-states)]
                                      (let [candidates (for [i (range n-states)]
                                                       (* (nth viterbi i)
                                                          (nth (nth transition-matrix i) j)
                                                          (nth (nth emission-matrix j) obs-t)))
                                            max-val (apply max candidates)
                                            max-idx (first (keep-indexed #(when (= %2 max-val) %1) candidates))]
                                        max-val)))
                       path-t (vec (for [j (range n-states)]
                                   (let [candidates (for [i (range n-states)]
                                                    (* (nth viterbi i)
                                                       (nth (nth transition-matrix i) j)))
                                         max-idx (first (keep-indexed #(when (= %2 (apply max candidates)) %1) candidates))]
                                     (conj (nth paths max-idx) j))))]
                   [viterbi-t path-t]))
                [viterbi-0 path-0]
                (range 1 n-obs))
        
        ;; Find best final state
        best-idx (first (keep-indexed #(when (= %2 (apply max viterbi-final)) %1) viterbi-final))
        best-path (nth path-final best-idx)
        best-prob (nth viterbi-final best-idx)]
    
    (->ViterbiPath best-path viterbi-final best-prob)))

;; -- Baum-Welch Algorithm (EM) --

(defn baum-welch-step
  "Single iteration of Baum-Welch algorithm
   Re-estimates model parameters from observations"
  [observations hmm-model]
  (let [{:keys [states transition-matrix emission-matrix initial-distribution]} hmm-model
        n-states (count states)
        n-obs (count observations)
        
        ;; Forward-backward
        forward-result (forward-algorithm observations emission-matrix transition-matrix initial-distribution)
        backward-result (backward-algorithm observations emission-matrix transition-matrix)
        
        alpha (:forward forward-result)
        beta (:backward backward-result)
        likelihood (:likelihood forward-result)
        
        ;; Calculate gamma (posterior state probabilities)
        gamma (vec (for [t (range n-obs)]
                   (let [alpha-t (nth alpha t)
                         beta-t (nth beta t)
                         denom (reduce + (for [i (range n-states)]
                                        (* (nth alpha-t i) (nth beta-t i))))]
                     (vec (for [i (range n-states)]
                          (/ (* (nth alpha-t i) (nth beta-t i)) denom))))))
        
        ;; Calculate xi (joint state probabilities)
        xi (vec (for [t (range (dec n-obs))]
                (let [alpha-t (nth alpha t)
                      beta-next (nth beta (inc t))
                      obs-next (nth observations (inc t))
                      denom (reduce + (for [i (range n-states) j (range n-states)]
                                     (* (nth alpha-t i)
                                        (nth (nth transition-matrix i) j)
                                        (nth (nth emission-matrix j) obs-next)
                                        (nth beta-next j))))]
                  (vec (for [i (range n-states)]
                       (vec (for [j (range n-states)]
                            (/ (* (nth alpha-t i)
                                  (nth (nth transition-matrix i) j)
                                  (nth (nth emission-matrix j) obs-next)
                                  (nth beta-next j))
                               denom))))))))
        
        ;; Re-estimate initial distribution
        new-initial (vec (for [i (range n-states)]
                         (nth (nth gamma 0) i)))
        
        ;; Re-estimate transition matrix
        new-transition (vec (for [i (range n-states)]
                            (vec (for [j (range n-states)]
                                 (let [numerator (reduce + (for [t (range (dec n-obs))]
                                                           (nth (nth (nth xi t) i) j)))
                                       denominator (reduce + (for [t (range (dec n-obs))]
                                                            (nth (nth gamma t) i)))]
                                   (if (zero? denominator) 0.0 (/ numerator denominator)))))))
        
        ;; Re-estimate emission matrix (simplified - assumes discrete observations)
        new-emission (vec (for [i (range n-states)]
                          (vec (for [k (range n-states)]  ;; Assuming same number of observation symbols
                               (let [numerator (reduce + (for [t (range n-obs)
                                                              :when (= (nth observations t) k)]
                                                          (nth (nth gamma t) i)))
                                     denominator (reduce + (for [t (range n-obs)]
                                                          (nth (nth gamma t) i)))]
                                 (if (zero? denominator) 0.0 (/ numerator denominator)))))))]
    
    (->HMMModel states new-transition new-emission new-initial)))

;; -- Regime Detection --

(defn detect-regime-change
  "Detect regime changes in observation sequence"
  [observations threshold]
  (let [n (count observations)
        changes (vec (for [t (range 1 n)]
                     (let [change (math/abs (- (nth observations t) (nth observations (dec t))))]
                       {:time t :change change :is-change (> change threshold)})))]
    
    (filter :is-change changes)))

(defn regime-probabilities
  "Calculate probability of being in each regime at each time"
  [observations hmm-model]
  (let [forward-result (forward-algorithm observations
                                         (:emission-matrix hmm-model)
                                         (:transition-matrix hmm-model)
                                         (:initial-distribution hmm-model))
        backward-result (backward-algorithm observations
                                           (:emission-matrix hmm-model)
                                           (:transition-matrix hmm-model))
        
        alpha (:forward forward-result)
        beta (:backward backward-result)
        likelihood (:likelihood forward-result)
        
        n-states (count (:states hmm-model))
        n-obs (count observations)
        
        gamma (vec (for [t (range n-obs)]
                   (let [alpha-t (nth alpha t)
                         beta-t (nth beta t)
                         denom (reduce + (for [i (range n-states)]
                                        (* (nth alpha-t i) (nth beta-t i))))]
                     (vec (for [i (range n-states)]
                          (/ (* (nth alpha-t i) (nth beta-t i)) denom))))))]
    
    {:probabilities gamma
     :likelihood likelihood}))

;; -- Utilities --

(defn most-likely-state
  "Get most likely state at each time"
  [gamma]
  (mapv (fn [gamma-t]
         (first (keep-indexed #(when (= %2 (apply max gamma-t)) %1) gamma-t)))
       gamma))

(defn state-duration
  "Calculate duration of each state"
  [states]
  (loop [s states durations [] current-state (first states) count 1]
    (if (empty? s)
      durations
      (let [next-state (first s)]
        (if (= next-state current-state)
          (recur (rest s) durations current-state (inc count))
          (recur (rest s) (conj durations {:state current-state :duration count})
                 next-state 1))))))
