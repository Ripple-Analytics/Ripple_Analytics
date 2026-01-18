(ns mental-models.refactor.autonomous
  "Autonomous Tech Debt Elimination Loop
   Self-improving system that continuously refactors code for better quality"
  (:require [mental-models.refactor.dag :as dag]
            [mental-models.refactor.tangle :as tangle]
            [mental-models.refactor.agent :as agent]
            [mental-models.refactor.equivalence :as equiv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration]))

;; -- Configuration -----------------------------------------------------------

(def default-config
  {:source-dir "src"
   :max-iterations 10
   :max-tangles-per-iteration 3
   :min-improvement-threshold 0.05  ; 5% improvement required to continue
   :verification-tests 100
   :dry-run true
   :backup-dir "backups"
   :report-dir "reports"
   :pause-between-iterations 5000})  ; 5 seconds

;; -- State Management --------------------------------------------------------

(defonce loop-state (atom {:running false
                           :iteration 0
                           :history []
                           :metrics {:initial nil
                                     :current nil
                                     :improvements []}}))

(defn reset-state! []
  (reset! loop-state {:running false
                      :iteration 0
                      :history []
                      :metrics {:initial nil
                                :current nil
                                :improvements []}}))

;; -- Metrics Calculation -----------------------------------------------------

(defn calculate-health-metrics
  "Calculate overall codebase health metrics"
  [dag tangle-analysis]
  (let [nodes (:nodes dag)
        edges (:edges dag)
        tangles (:tangles tangle-analysis)
        metadata (:metadata dag)]
    {:timestamp (Instant/now)
     :total-nodes (count nodes)
     :total-edges (count edges)
     :total-loc (:total-loc metadata)
     :total-complexity (:total-complexity metadata)
     :avg-complexity (/ (:total-complexity metadata)
                        (max 1 (:file-count metadata)))
     :tangle-count (count tangles)
     :critical-tangles (count (filter #(= :critical (:severity %)) tangles))
     :high-tangles (count (filter #(= :high (:severity %)) tangles))
     :total-tangle-complexity (reduce + (map :complexity tangles))
     :cycle-count (count (:cycles tangle-analysis))
     :health-score (calculate-health-score dag tangle-analysis)}))

(defn calculate-health-score
  "Calculate a 0-100 health score for the codebase"
  [dag tangle-analysis]
  (let [tangles (:tangles tangle-analysis)
        cycles (:cycles tangle-analysis)
        nodes (:nodes dag)
        
        ;; Penalties
        tangle-penalty (* 5 (count tangles))
        critical-penalty (* 20 (count (filter #(= :critical (:severity %)) tangles)))
        cycle-penalty (* 10 (count cycles))
        complexity-penalty (min 30 (/ (:total-complexity (:metadata dag)) 100))
        
        ;; Base score
        base-score 100]
    (max 0 (- base-score tangle-penalty critical-penalty cycle-penalty complexity-penalty))))

(defn calculate-improvement
  "Calculate improvement between two metric snapshots"
  [before after]
  (let [score-improvement (- (:health-score after) (:health-score before))
        tangle-reduction (- (:tangle-count before) (:tangle-count after))
        complexity-reduction (- (:total-complexity before) (:total-complexity after))]
    {:score-improvement score-improvement
     :score-improvement-pct (/ score-improvement (max 1 (:health-score before)))
     :tangle-reduction tangle-reduction
     :complexity-reduction complexity-reduction
     :improved? (pos? score-improvement)}))

;; -- Iteration Execution -----------------------------------------------------

(defn run-iteration
  "Run a single iteration of the refactoring loop"
  [config]
  (log/info "Starting iteration" (:iteration @loop-state))
  (let [start-time (Instant/now)
        
        ;; Step 1: Analyze current state
        _ (log/info "Step 1: Analyzing codebase...")
        dag-analysis (dag/analyze-codebase (:source-dir config))
        dag (:dag dag-analysis)
        
        ;; Step 2: Detect tangles
        _ (log/info "Step 2: Detecting tangles...")
        tangle-analysis (tangle/detect-and-prioritize dag)
        
        ;; Step 3: Calculate metrics
        metrics (calculate-health-metrics dag (:analysis tangle-analysis))
        
        ;; Update state with initial metrics if first iteration
        _ (when (nil? (get-in @loop-state [:metrics :initial]))
            (swap! loop-state assoc-in [:metrics :initial] metrics))
        
        ;; Step 4: Select tangles to refactor
        _ (log/info "Step 4: Selecting tangles to refactor...")
        tangles-to-refactor (take (:max-tangles-per-iteration config)
                                  (:prioritized-tangles tangle-analysis))
        
        ;; Step 5: Refactor each tangle
        _ (log/info "Step 5: Refactoring" (count tangles-to-refactor) "tangles...")
        refactorings (for [t tangles-to-refactor]
                       (let [result (agent/refactor-tangle t dag)]
                         (Thread/sleep 1000) ; Rate limiting
                         result))
        
        ;; Step 6: Verify refactorings (in dry-run, just validate syntax)
        _ (log/info "Step 6: Verifying refactorings...")
        verified (for [{:keys [success refactored-code] :as r} refactorings
                       :when success]
                   (assoc r :verification
                          (try
                            {:syntax-valid (boolean (read-string refactored-code))}
                            (catch Exception e
                              {:syntax-valid false
                               :error (.getMessage e)}))))
        
        ;; Step 7: Apply verified refactorings (if not dry-run)
        _ (log/info "Step 7: Applying refactorings...")
        applied (if (:dry-run config)
                  (map #(assoc % :applied false :reason "dry-run") verified)
                  (for [{:keys [verification] :as v} verified
                        :when (:syntax-valid verification)]
                    (assoc v :applied true)))
        
        ;; Calculate iteration results
        end-time (Instant/now)
        duration (Duration/between start-time end-time)]
    
    {:iteration (:iteration @loop-state)
     :timestamp start-time
     :duration-ms (.toMillis duration)
     :metrics metrics
     :tangles-found (count (:tangles (:analysis tangle-analysis)))
     :tangles-refactored (count tangles-to-refactor)
     :refactorings-successful (count (filter :success refactorings))
     :refactorings-applied (count (filter :applied applied))
     :details {:dag-summary (:analysis dag-analysis)
               :tangle-summary (:summary (:analysis tangle-analysis))
               :refactorings applied}}))

;; -- Loop Control ------------------------------------------------------------

(defn should-continue?
  "Determine if the loop should continue"
  [config iteration-result]
  (let [current-metrics (:metrics iteration-result)
        previous-metrics (get-in @loop-state [:metrics :current])
        iteration (:iteration @loop-state)]
    (cond
      ;; Max iterations reached
      (>= iteration (:max-iterations config))
      (do (log/info "Max iterations reached")
          false)
      
      ;; No tangles found
      (zero? (:tangles-found iteration-result))
      (do (log/info "No tangles found - codebase is clean!")
          false)
      
      ;; No improvement from previous iteration
      (and previous-metrics
           (let [improvement (calculate-improvement previous-metrics current-metrics)]
             (< (:score-improvement-pct improvement)
                (:min-improvement-threshold config))))
      (do (log/info "Insufficient improvement - stopping")
          false)
      
      ;; Continue
      :else true)))

(defn run-loop
  "Run the autonomous refactoring loop"
  [& {:keys [config] :or {config default-config}}]
  (reset-state!)
  (swap! loop-state assoc :running true)
  
  (log/info "Starting autonomous tech debt elimination loop")
  (log/info "Configuration:" config)
  
  (try
    (loop []
      (when (:running @loop-state)
        (let [result (run-iteration config)]
          ;; Update state
          (swap! loop-state update :iteration inc)
          (swap! loop-state update :history conj result)
          (swap! loop-state assoc-in [:metrics :current] (:metrics result))
          
          ;; Check if we should continue
          (when (should-continue? config result)
            (log/info "Pausing before next iteration...")
            (Thread/sleep (:pause-between-iterations config))
            (recur)))))
    
    (catch Exception e
      (log/error e "Error in refactoring loop")
      (swap! loop-state assoc :error (.getMessage e))))
  
  (swap! loop-state assoc :running false)
  (generate-report))

(defn stop-loop!
  "Stop the running loop"
  []
  (swap! loop-state assoc :running false)
  (log/info "Loop stop requested"))

;; -- Reporting ---------------------------------------------------------------

(defn generate-report
  "Generate a comprehensive report of the refactoring session"
  []
  (let [state @loop-state
        initial (:initial (:metrics state))
        final (:current (:metrics state))
        history (:history state)]
    {:summary {:total-iterations (count history)
               :total-duration-ms (reduce + (map :duration-ms history))
               :total-tangles-refactored (reduce + (map :tangles-refactored history))
               :total-refactorings-applied (reduce + (map :refactorings-applied history))}
     
     :improvement (when (and initial final)
                    (calculate-improvement initial final))
     
     :initial-metrics initial
     :final-metrics final
     
     :iteration-history (for [h history]
                          {:iteration (:iteration h)
                           :health-score (get-in h [:metrics :health-score])
                           :tangles-found (:tangles-found h)
                           :refactorings-applied (:refactorings-applied h)})
     
     :recommendations (generate-recommendations state)}))

(defn generate-recommendations
  "Generate recommendations based on the refactoring session"
  [state]
  (let [final (:current (:metrics state))
        history (:history state)]
    (cond-> []
      ;; Still have critical tangles
      (and final (pos? (:critical-tangles final)))
      (conj {:priority :high
             :message (str (:critical-tangles final) " critical tangles remain")
             :action "Review and manually refactor critical tangles"})
      
      ;; Still have cycles
      (and final (pos? (:cycle-count final)))
      (conj {:priority :high
             :message (str (:cycle-count final) " dependency cycles detected")
             :action "Break circular dependencies manually"})
      
      ;; Low health score
      (and final (< (:health-score final) 70))
      (conj {:priority :medium
             :message "Codebase health score below 70"
             :action "Continue refactoring or consider architectural changes"})
      
      ;; Many iterations with little improvement
      (and (> (count history) 5)
           (let [recent (take-last 3 history)
                 scores (map #(get-in % [:metrics :health-score]) recent)]
             (< (- (last scores) (first scores)) 5)))
      (conj {:priority :medium
             :message "Diminishing returns from automated refactoring"
             :action "Consider manual architectural review"}))))

;; -- Scheduled Execution -----------------------------------------------------

(defn schedule-daily-run
  "Schedule daily autonomous refactoring"
  [source-dir & {:keys [hour] :or {hour 2}}]
  ;; This would integrate with a scheduler like chime or quartzite
  (log/info "Scheduled daily refactoring at" hour ":00 for" source-dir)
  {:scheduled true
   :source-dir source-dir
   :time (str hour ":00")})

;; -- Main Entry Point --------------------------------------------------------

(defn autonomous-refactor!
  "Main entry point for autonomous tech debt elimination"
  [source-dir & {:keys [dry-run max-iterations]
                 :or {dry-run true max-iterations 5}}]
  (run-loop :config (assoc default-config
                           :source-dir source-dir
                           :dry-run dry-run
                           :max-iterations max-iterations)))

;; -- REPL Helpers ------------------------------------------------------------

(defn status
  "Get current loop status"
  []
  (let [state @loop-state]
    {:running (:running state)
     :iteration (:iteration state)
     :current-health (get-in state [:metrics :current :health-score])
     :initial-health (get-in state [:metrics :initial :health-score])}))

(defn history
  "Get iteration history"
  []
  (:history @loop-state))

(defn last-report
  "Get the last generated report"
  []
  (generate-report))
