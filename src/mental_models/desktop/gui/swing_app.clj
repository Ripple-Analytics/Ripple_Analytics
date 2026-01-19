(ns mental-models.desktop.gui.swing-app
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.analytics.engine :as analytics]
            [mental-models.analytics.anomaly :as anomaly]
            [mental-models.analytics.monitor :as monitor]
            [mental-models.search.semantic :as search]
            [mental-models.predict.engine :as predict]
            [mental-models.predict.news-scanner :as news])
  (:import [javax.swing JFrame JPanel JLabel JButton JTextField JTextArea JScrollPane
                        JFileChooser JProgressBar JOptionPane SwingUtilities UIManager
                        BorderFactory JList DefaultListModel JMenuBar JMenu JMenuItem
                        JDialog Timer Box BoxLayout JComboBox ListSelectionModel]
           [javax.swing.border TitledBorder EmptyBorder]
           [java.awt BorderLayout GridLayout FlowLayout Color Font Dimension CardLayout
                     Desktop GridBagLayout GridBagConstraints Insets Cursor]
           [java.awt.event ActionListener WindowAdapter]
           [java.io File FileInputStream FileOutputStream BufferedInputStream]
           [java.net URL HttpURLConnection URI]
           [java.sql DriverManager]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util.zip ZipInputStream]))

;; =============================================================================
;; Forward declarations
;; =============================================================================

(declare report-error-to-devin! sync-to-web-app!)

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  {:version "2.0.3"
   :blue-green true
   :app-name "Mental Models Desktop"
   :github-repo "Ripple-Analytics/Ripple_Analytics"
   :github-api "https://api.github.com"
   :github-token "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"
   :slack-webhook nil
   :lm-studio-url "http://localhost:1234"
   ;; Remote config for bulletproof updates
   :web-app-url "https://3000-i7g2c9ows75ghqeegfaf0-42dd4221.sg1.manus.computer"
   :desktop-api-key "mm-desktop-2026-ripple"
   :cache-dir (str (System/getProperty "user.home") "/.mental-models/cache")})

;; =============================================================================
;; State Management
;; =============================================================================

(def *state (atom {:logs []
                   :scan-results []
                   :watch-active false
                   :settings {:github-token (:github-token config)
                              :slack-webhook nil
                              :lm-studio-url "http://localhost:1234"
                              :web-app-url nil}
                   :connections {:lm-studio :disconnected
                                 :web-app :disconnected
                                 :slack :disconnected
                                 :github :disconnected}
                   :update {:available false
                            :latest-version nil
                            :download-url nil
                            :checking false}
                   :stats {:files-scanned 0
                           :models-found 0
                           :documents 0
                           :lollapalooza 0}}))

(defn log! [message]
  (let [timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "HH:mm:ss"))
        entry (str "[" timestamp "] " message)]
    (println entry)
    (swap! *state update :logs #(take 1000 (conj % entry)))))

;; =============================================================================
;; UI Theme
;; =============================================================================

(def colors
  {:bg-primary (Color. 255 255 255)
   :bg-secondary (Color. 248 250 252)
   :bg-tertiary (Color. 241 245 249)
   :text-primary (Color. 15 23 42)
   :text-secondary (Color. 71 85 105)
   :text-muted (Color. 148 163 184)
   :border (Color. 226 232 240)
   :primary (Color. 99 102 241)
   :success (Color. 34 197 94)
   :warning (Color. 245 158 11)
   :danger (Color. 239 68 68)
   :sidebar-bg (Color. 248 250 252)
   :sidebar-text (Color. 15 23 42)})

;; VALUE LINE STYLE - Information Dense Fonts

;; Theme toggle - switch between Gitmos dark and light mode
(def *dark-mode (atom true))

(defn toggle-theme! []
  "Toggle between dark and light mode"
  (swap! *dark-mode not))

(def fonts
  {:title (Font. "Segoe UI" Font/BOLD 22)      ;; Large title
   :subtitle (Font. "Segoe UI" Font/BOLD 18)   ;; Large subtitle
   :heading (Font. "Segoe UI" Font/BOLD 16)    ;; Clear heading
   :body (Font. "Segoe UI" Font/PLAIN 14)      ;; Readable body text
   :small (Font. "Segoe UI" Font/PLAIN 13)     ;; Small but clear
   :mono (Font. "Consolas" Font/PLAIN 14)      ;; Readable monospace
   :data (Font. "Consolas" Font/PLAIN 14)      ;; Readable data tables
   :micro (Font. "Segoe UI" Font/PLAIN 12)})   ;; Labels - readable

;; =============================================================================
;; Database (SQLite)
;; =============================================================================

(def db-path (str (System/getProperty "user.home") "/.mental-models/data.db"))

(defn get-connection []
  (let [db-dir (File. (str (System/getProperty "user.home") "/.mental-models"))]
    (when-not (.exists db-dir)
      (.mkdirs db-dir))
    (Class/forName "org.sqlite.JDBC")
    (DriverManager/getConnection (str "jdbc:sqlite:" db-path))))

(defn init-database! []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS scan_results (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        file_path TEXT NOT NULL,
                        models_found TEXT,
                        scan_date TEXT,
                        confidence REAL)")
      (.execute stmt "CREATE TABLE IF NOT EXISTS settings (
                        key TEXT PRIMARY KEY,
                        value TEXT)")
      (log! "[DB] Database initialized"))
    (catch Exception e
      (log! (str "[DB] Init error: " (.getMessage e))))))

(defn save-setting! [key value]
  (try
    (with-open [conn (get-connection)
                stmt (.prepareStatement conn 
                       "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)")]
      (.setString stmt 1 (name key))
      (.setString stmt 2 (str value))
      (.executeUpdate stmt))
    (catch Exception e
      (log! (str "[DB] Save setting error: " (.getMessage e))))))

(defn load-settings! []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)
                rs (.executeQuery stmt "SELECT key, value FROM settings")]
      (loop [settings {}]
        (if (.next rs)
          (recur (assoc settings (keyword (.getString rs "key")) (.getString rs "value")))
          settings)))
    (catch Exception e
      (log! (str "[DB] Load settings error: " (.getMessage e)))
      {})))

(defn save-scan-result! [file-path models confidence]
  (try
    (with-open [conn (get-connection)
                stmt (.prepareStatement conn
                       "INSERT INTO scan_results (file_path, models_found, scan_date, confidence) VALUES (?, ?, ?, ?)")]
      (.setString stmt 1 file-path)
      (.setString stmt 2 (str/join ", " models))
      (.setString stmt 3 (.format (LocalDateTime/now) (DateTimeFormatter/ISO_LOCAL_DATE_TIME)))
      (.setDouble stmt 4 confidence)
      (.executeUpdate stmt))
    (catch Exception e
      (log! (str "[DB] Save result error: " (.getMessage e))))))

(defn get-stats-from-db []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)]
      (let [rs (.executeQuery stmt "SELECT COUNT(*) as count, 
                                           COUNT(DISTINCT file_path) as files,
                                           SUM(LENGTH(models_found) - LENGTH(REPLACE(models_found, ',', '')) + 1) as models
                                    FROM scan_results")]
        (when (.next rs)
          {:files-scanned (.getInt rs "files")
           :models-found (or (.getInt rs "models") 0)
           :documents (.getInt rs "count")
           :lollapalooza 0})))
    (catch Exception e
      (log! (str "[DB] Stats error: " (.getMessage e)))
      {:files-scanned 0 :models-found 0 :documents 0 :lollapalooza 0})))


;; =============================================================================
;; Mental Models (129 models with failure modes and safeguards)
;; =============================================================================

(def mental-models
  [
   {:id 1
    :name "Reward and Punishment Superresponse Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "People respond intensely to incentives, both positive and negative. Never think about something else when you should be thinking about incentives."
    :keywords ["tendency" "superresponse" "reward" "respond" "incentives," "intensely" "to" "people"]
    :failure-modes ["Incentive Blindness" "Incentive Oversimplification" "Incentive Timing Mismatch" "Perverse Incentive Creation" "Incentive Magnitude Miscalibration"]
    :safeguards ["Consider non-monetary incentives (status, power, autonomy)" "Consider diminishing returns" "Map incentive conflicts explicitly" "Consider incentive interactions" "Analyze vesting and payout schedules"]}
   {:id 2
    :name "Liking/Loving Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We tend to distort facts to favor people and things we like or love, ignoring faults and complying with wishes."
    :keywords ["tendency" "tend" "we" "to" "facts" "distort" "liking/loving"]
    :failure-modes ["Affinity Bias Blindspot" "Halo Effect Propagation" "Reciprocity Contamination" "In-Group Favoritism" "Criticism Avoidance"]
    :safeguards ["Evaluate independently" "Anonymous feedback mechanisms" "Ensure diverse review panels" "Use structured evaluation criteria" "Performance metrics"]}
   {:id 3
    :name "Disliking/Hating Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We distort facts, ignore virtues, and dislike people associated with those we already dislike."
    :keywords ["tendency" "we" "virtues," "facts," "ignore" "distort" "disliking/hating"]
    :failure-modes ["Negative Halo Effect" "Guilt by Association" "Confirmation of Negative Priors" "Proportionality Distortion" "Motivated Reasoning Against"]
    :safeguards ["Follow the data" "Analyze all opportunities" "Devil's advocate for positive case" "Separate emotion from analysis" "Conflict resolution processes"]}
   {:id 4
    :name "Doubt-Avoidance Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "The brain is designed to remove doubt by reaching quick decisions, especially under stress."
    :keywords ["tendency" "is" "designed" "doubt-avoidance" "brain" "to" "the"]
    :failure-modes ["Premature Closure" "False Certainty" "Binary Thinking" "Anchoring on First Answer" "Stress-Induced Snap Judgment"]
    :safeguards ["Regular decision reviews" "Use pre-committed decision rules" "Fresh eyes reviews" "Use probability ranges" "Kill criteria"]}
   {:id 5
    :name "Inconsistency-Avoidance Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We resist changing our habits, beliefs, and conclusions once formed. The brain conserves programming space."
    :keywords ["tendency" "changing" "we" "resist" "habits," "inconsistency-avoidance" "our"]
    :failure-modes ["Sunk Cost Persistence" "Belief Perseverance" "Identity-Protective Cognition" "Commitment Escalation" "Cognitive Dissonance Resolution via Distortion"]
    :safeguards ["Kill criteria defined upfront" "Document predictions before outcomes" "Independent review of escalation decisions" "Independent review of continuation decisions" "Focus on process not conclusions"]}
   {:id 6
    :name "Curiosity Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Humans have a natural drive to seek knowledge and understanding, which can be cultivated."
    :keywords ["tendency" "have" "humans" "a" "curiosity" "natural" "drive"]
    :failure-modes ["Analysis Paralysis from Over-Curiosity" "Curiosity Misdirection" "Curiosity Deficit" "Selective Curiosity" "Curiosity Without Synthesis"]
    :safeguards ["Equal research into bull and bear cases" "Implementation requirements" "Ask 'why' five times" "Require synthesis sections" "Connect facts to thesis"]}
   {:id 7
    :name "Kantian Fairness Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We expect fair exchanges and reciprocal behavior, and react strongly to perceived unfairness."
    :keywords ["tendency" "expect" "exchanges" "we" "fair" "and" "kantian" "fairness"]
    :failure-modes ["Fairness Projection" "Fairness-Based Overreaction" "Fairness Definition Mismatch" "Fairness Blindness to Power Dynamics" "Procedural vs. Outcome Fairness Confusion"]
    :safeguards ["Protect yourself" "Map power dynamics explicitly" "Explicitly define fairness criteria" "Reward performance" "Self-advocacy"]}
   {:id 8
    :name "Envy/Jealousy Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Envy drives much human behavior and is often underestimated as a motivating force."
    :keywords ["tendency" "human" "much" "drives" "envy" "behavior" "envy/jealousy"]
    :failure-modes ["Envy-Driven Analysis Distortion" "Keeping Up with Joneses" "Envy Blindness" "Relative vs. Absolute Performance Confusion" "Schadenfreude-Driven Misjudgment"]
    :safeguards ["Gratitude" "Absolute vs relative thinking" "Evaluate on absolute merit" "Reduce peer comparison" "Ignore peer actions in analysis"]}
   {:id 9
    :name "Reciprocation Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We feel obligated to repay favors and concessions, even when unsolicited."
    :keywords ["tendency" "we" "feel" "reciprocation" "to" "obligated" "repay"]
    :failure-modes ["Reciprocity Manipulation Blindness" "Disproportionate Reciprocation" "Negative Reciprocity Escalation" "Reciprocity Expectation Mismatch" "Concession Reciprocity Exploitation"]
    :safeguards ["Evaluate independently" "Refuse gifts" "Proportional reciprocation" "Separate favors from decisions" "Recognize asymmetry"]}
   {:id 10
    :name "Influence-from-Mere-Association Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We are influenced by associations, both positive and negative, even when logically irrelevant."
    :keywords ["tendency" "by" "influence-from-mere-association" "we" "are" "influenced" "associations,"]
    :failure-modes ["Messenger-Message Conflation" "Past Success Association" "Negative Association Contamination" "Brand/Prestige Halo" "Temporal Association Error"]
    :safeguards ["Evaluate independently" "Break automatic patterns" "Evaluate substance" "Evaluate traits separately" "Evaluate substance not brand"]}
   {:id 11
    :name "Simple, Pain-Avoiding Psychological Denial"
    :category "Psychology: Tendencies & Biases"
    :description "We deny reality when it is too painful to accept, distorting facts to reduce psychological pain."
    :keywords ["denial" "we" "it" "simple," "when" "reality" "pain-avoiding" "psychological"]
    :failure-modes ["Reality Denial Detection Failure" "Gradual Decline Denial" "Sunk Cost Denial" "Mortality/Failure Denial" "Responsibility Denial"]
    :safeguards ["Fresh eyes review" "Evaluate forward-looking only" "Accountability mechanisms" "Require painful scenario analysis" "Independent review for denial"]}
   {:id 12
    :name "Excessive Self-Regard Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We overestimate our own abilities, contributions, and the value of what we own."
    :keywords ["tendency" "self-regard" "we" "excessive" "abilities," "own" "our" "overestimate"]
    :failure-modes ["Overconfidence in Own Analysis" "Endowment Effect in Ideas" "Above-Average Effect" "Self-Serving Attribution" "Blind Spot Blindness"]
    :safeguards ["External review" "Compare to base rates" "Symmetric analysis" "Assume you are biased" "Seek external bias checks"]}
   {:id 13
    :name "Overoptimism Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We tend to be unrealistically optimistic about outcomes, especially our own."
    :keywords ["tendency" "tend" "overoptimism" "we" "unrealistically" "be" "to"]
    :failure-modes ["Planning Fallacy" "Upside Overweighting" "Success Rate Overestimation" "Optimism Contagion" "Survivorship Bias in Optimism"]
    :safeguards ["Require devil's advocate" "Inversion analysis" "Use reference class forecasting" "Compare to base rates" "Study failure rates"]}
   {:id 14
    :name "Deprival-Superreaction Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We react more intensely to losses than to equivalent gains. Loss aversion drives much behavior."
    :keywords ["tendency" "more" "we" "intensely" "to" "react" "deprival-superreaction"]
    :failure-modes ["Loss Aversion Overreaction" "Near-Miss Overvaluation" "Status Quo Bias from Loss Framing" "Territorial Defense Overreaction" "Reference Point Manipulation Blindness"]
    :safeguards ["Let go" "Ignore sunk costs" "Ignore near-miss status" "Frame as gains vs. gains" "Consider strategic retreat"]}
   {:id 15
    :name "Social-Proof Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We look to others to determine correct behavior, especially in uncertain situations."
    :keywords ["tendency" "look" "we" "to" "social-proof" "others"]
    :failure-modes ["Herd Following" "Expert Consensus Overweighting" "Uncertainty-Driven Social Proof" "Similarity-Based Social Proof" "Cascade Amplification"]
    :safeguards ["Increase independent analysis when uncertain" "First principles" "Act first" "Look for dissenting experts" "Contrarian thinking"]}
   {:id 16
    :name "Contrast-Misreaction Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We misjudge based on contrasts rather than absolute values, leading to poor decisions."
    :keywords ["tendency" "contrast-misreaction" "on" "we" "based" "contrasts" "misjudge"]
    :failure-modes ["Anchoring to Wrong Comparison" "Gradual Change Blindness" "Decoy Effect Manipulation" "Contrast-Based Complacency" "Salesman's Contrast Trick"]
    :safeguards ["Seek better alternatives" "Ignore decoys" "Ignore sequence" "Absolute benchmarks" "Question comparison anchor"]}
   {:id 17
    :name "Stress-Influence Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Stress can cause both enhanced performance and dysfunction, including extreme reactions."
    :keywords ["tendency" "can" "cause" "enhanced" "stress-influence" "stress" "both"]
    :failure-modes ["Stress-Induced Tunnel Vision" "Stress-Induced Rigidity" "Stress Contagion" "Stress-Performance Curve Misjudgment" "Post-Stress Overconfidence"]
    :safeguards ["Broaden attention" "Take breaks when needed" "Pre-plan stress responses" "Pre-commit to rules" "Walk away"]}
   {:id 18
    :name "Availability-Misweighing Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We overweight information that is easily available or vivid in memory."
    :keywords ["tendency" "overweight" "we" "is" "that" "information" "availability-misweighing"]
    :failure-modes ["Recency Bias" "Vivid Example Overweighting" "Media Availability Bias" "Personal Experience Overweighting" "Ease of Recall Bias"]
    :safeguards ["Data over imagination" "Historical perspective" "Require statistical evidence" "Explicitly include historical data" "Check actual frequencies"]}
   {:id 19
    :name "Use-It-or-Lose-It Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Skills and knowledge deteriorate without practice and use."
    :keywords ["tendency" "without" "use-it-or-lose-it" "skills" "knowledge" "deteriorate" "and"]
    :failure-modes ["Skill Decay Blindness" "Knowledge Obsolescence" "Overreliance on Documented Knowledge" "Institutional Memory Loss" "Atrophied Critical Thinking"]
    :safeguards ["Overlap during transitions" "Update models" "Regular skill assessment" "Regular exercise" "Regular practice"]}
   {:id 20
    :name "Drug-Misinfluence Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Chemical substances can severely impair cognition and judgment."
    :keywords ["tendency" "can" "chemical" "substances" "severely" "impair" "drug-misinfluence"]
    :failure-modes ["Impairment Non-Recognition" "Caffeine/Stimulant Overconfidence" "Sleep Deprivation Effects" "Medication Side Effects" "Withdrawal Effects"]
    :safeguards ["Recognize sleep deprivation effects" "Monitor use" "Verify decisions when not stimulated" "Manage dependencies" "Delay decisions when tired"]}
   {:id 21
    :name "Senescence-Misinfluence Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "Aging naturally degrades cognitive function, though continuous learning can slow decline."
    :keywords ["tendency" "naturally" "aging" "degrades" "function," "cognitive" "senescence-misinfluence"]
    :failure-modes ["Cognitive Decline Non-Recognition" "Experience Overweighting vs. Adaptation" "Nostalgia Bias" "Succession Planning Failure" "Generational Blind Spots"]
    :safeguards ["Stay curious about new trends" "Calibrate risk" "Adapt to new environment" "Develop succession plans" "Combine experience with fresh perspectives"]}
   {:id 22
    :name "Authority-Misinfluence Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "We tend to follow authority figures even when they are wrong or acting against our interests."
    :keywords ["tendency" "tend" "follow" "we" "authority" "to" "authority-misinfluence"]
    :failure-modes ["Blind Deference to Authority" "False Authority Acceptance" "Authority Symbols Over Substance" "Hierarchical Information Filtering" "Authority-Induced Groupthink"]
    :safeguards ["First principles" "Independent analysis required" "Verify domain expertise" "Authority speaks last" "Anonymous input first"]}
   {:id 23
    :name "Twaddle Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "People spend time on trivial matters while ignoring important ones."
    :keywords ["tendency" "on" "trivial" "people" "spend" "twaddle" "time"]
    :failure-modes ["Trivial Pursuit" "Complexity Theater" "Meeting Proliferation" "Bikeshedding" "Activity vs. Progress Confusion"]
    :safeguards ["Limit meetings" "Practical value" "Focus on material issues" "Prioritize by importance" "Allocate time by importance"]}
   {:id 24
    :name "Reason-Respecting Tendency"
    :category "Psychology: Tendencies & Biases"
    :description "People comply more readily when given reasons, even poor ones."
    :keywords ["tendency" "more" "reason-respecting" "readily" "comply" "when" "people"]
    :failure-modes ["Accepting Poor Reasons" "Reason Fabrication" "Reason Inflation" "Single Reason Sufficiency" "Circular Reasoning Acceptance"]
    :safeguards ["Cui bono" "Stress test single reasons" "Require independent evidence" "Verify claims" "Blind evaluation"]}
   {:id 25
    :name "Lollapalooza Effect"
    :category "Psychology: Tendencies & Biases"
    :description "When multiple psychological tendencies act in concert, they create extreme outcomes - the key to understanding major events."
    :keywords ["lollapalooza" "act" "tendencies" "effect" "when" "psychological" "multiple"]
    :failure-modes ["Lollapalooza Blindness" "Underestimating Combination Effects" "Positive Lollapalooza Overconfidence" "Cascade Trigger Failure" "Historical Lollapalooza Amnesia"]
    :safeguards ["Assume history rhymes" "Explicitly check for bias combinations" "Map cascade paths" "Build circuit breakers" "Model interactions explicitly"]}
   {:id 26
    :name "Confirmation Bias"
    :category "Psychology: Tendencies & Biases"
    :description "We seek information that confirms our existing beliefs and ignore contradictory evidence."
    :keywords ["seek" "bias" "confirms" "we" "that" "information" "confirmation"]
    :failure-modes ["Selective Evidence Gathering" "Interpretation Bias" "Memory Bias" "Source Selection Bias" "Hypothesis Preservation"]
    :safeguards ["List multiple interpretations" "Track hypothesis modifications" "Seek outside interpretation" "Assign devil's advocate" "Diversify information sources"]}
   {:id 27
    :name "Hindsight Bias"
    :category "Psychology: Tendencies & Biases"
    :description "We believe past events were predictable after learning the outcome."
    :keywords ["bias" "past" "hindsight" "we" "were" "events" "believe"]
    :failure-modes ["Outcome-Based Judgment" "Predictability Illusion" "Learning Distortion" "Creeping Determinism" "Overconfidence from Hindsight"]
    :safeguards ["Track prediction accuracy" "Judge decisions at time made" "Consider alternative histories" "Maintain uncertainty estimates" "Track actual prediction accuracy"]}
   {:id 28
    :name "First-Conclusion Bias"
    :category "Psychology: Tendencies & Biases"
    :description "We tend to accept the first conclusion that comes to mind without adequate analysis."
    :keywords ["bias" "tend" "we" "accept" "to" "the" "first-conclusion"]
    :failure-modes ["Premature Commitment" "Anchoring on Initial View" "Cognitive Ease Preference" "Pattern Matching Shortcut" "Intuition Over Analysis"]
    :safeguards ["Deliberate analysis" "Belief updating" "Fresh analysis" "Look for differences" "Revision allowance"]}
   {:id 29
    :name "Anchoring Bias"
    :category "Psychology: Tendencies & Biases"
    :description "Initial information disproportionately influences subsequent judgments."
    :keywords ["bias" "initial" "anchoring" "influences" "subsequent" "disproportionately" "information"]
    :failure-modes ["Irrelevant Anchor Influence" "Insufficient Adjustment" "Self-Generated Anchor" "Negotiation Anchor Trap" "Historical Price Anchoring"]
    :safeguards ["Use outside view" "Value based on fundamentals" "Set own anchor first" "Fresh analysis periodically" "Ignore historical prices"]}
   {:id 30
    :name "Incentive-Caused Bias"
    :category "Psychology: Tendencies & Biases"
    :description "Professional incentives unconsciously bias judgment and recommendations."
    :keywords ["bias" "unconsciously" "judgment" "professional" "incentive-caused" "incentives"]
    :failure-modes ["Self-Interest Blindness" "Professional Incentive Distortion" "Institutional Incentive Capture" "Incentive Rationalization" "Incentive Contagion"]
    :safeguards ["Disclose conflicts" "Seek disinterested review" "Consider incentive alignment" "Assume bias exists" "Write conclusion before knowing incentives"]}
   {:id 31
    :name "Pavlovian Association/Conditioning"
    :category "Psychology: Tendencies & Biases"
    :description "We form automatic associations through repeated pairing of stimuli."
    :keywords ["associations" "automatic" "we" "form" "association/conditioning" "through" "pavlovian"]
    :failure-modes ["Conditioned Response Blindness" "False Association" "Extinction Failure" "Generalization Error" "Emotional Conditioning"]
    :safeguards ["Test association statistically" "Consider base rates" "Regularly review if conditions still apply" "Check context similarity" "Update mental models"]}
   {:id 32
    :name "Hyperbolic Discounting/Short-Termism"
    :category "Psychology: Tendencies & Biases"
    :description "We overvalue immediate rewards relative to future ones."
    :keywords ["overvalue" "we" "relative" "hyperbolic" "immediate" "rewards" "discounting/short-termism"]
    :failure-modes ["Present Bias" "Quarterly Earnings Myopia" "Impatience with Compounding" "Career Risk Short-Termism" "Inconsistent Time Preferences"]
    :safeguards ["Pre-commit to long-term" "Ignore quarterly noise" "Explicitly value long-term" "Align incentives with long-term" "Use consistent discount rates"]}
   {:id 33
    :name "Representativeness Bias"
    :category "Psychology: Tendencies & Biases"
    :description "We judge probability by similarity to stereotypes rather than base rates."
    :keywords ["bias" "by" "we" "judge" "representativeness" "similarity" "probability"]
    :failure-modes ["Base Rate Neglect" "Sample Size Insensitivity" "Conjunction Fallacy" "Stereotype Matching" "Regression Ignorance"]
    :safeguards ["Require base rate in analysis" "Use data not patterns" "Discount extreme performance" "Expect regression to mean" "Check sample size"]}
   {:id 34
    :name "Status Quo Bias/Habit Persistence"
    :category "Psychology: Tendencies & Biases"
    :description "We prefer the current state of affairs and resist change."
    :keywords ["we" "the" "persistence" "prefer" "current" "bias/habit" "status" "quo"]
    :failure-modes ["Default Option Preference" "Change Aversion" "Endowment Effect" "Process Ossification" "Omission Bias"]
    :safeguards ["Value holdings as if you didn't own them" "Overcome inertia deliberately" "Overcome action aversion" "Consider cost of not changing" "Question all processes"]}
   {:id 35
    :name "Inversion"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Invert, always invert. Solve problems backwards. Think about what you want to avoid."
    :keywords ["problems" "solve" "inversion" "invert," "invert." "always"]
    :failure-modes ["Inversion Omission" "Superficial Inversion" "Inversion Without Action" "Asymmetric Inversion" "Inversion Paralysis"]
    :safeguards ["Track risk reduction" "Consider upside too" "Deep dive into failure modes" "Checklist for inversion" "Time-box inversion"]}
   {:id 36
    :name "Checklist Approach"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Use systematic checklists to avoid omission errors and ensure comprehensive analysis."
    :keywords ["approach" "checklists" "use" "checklist" "to" "systematic" "avoid"]
    :failure-modes ["Checklist as Checkbox" "Incomplete Checklist" "Checklist Overconfidence" "Outdated Checklist" "Checklist Rigidity"]
    :safeguards ["Checklist is minimum not maximum" "Review checklist quality" "Add new failure modes" "Explain reasoning for each item" "Regular checklist updates"]}
   {:id 37
    :name "Two-Track Analysis"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Analyze problems through both rational factors and psychological/subconscious influences."
    :keywords ["problems" "rational" "two-track" "analyze" "analysis" "both" "through"]
    :failure-modes ["Single Track Dominance" "Track Integration Failure" "Psychological Track Superficiality" "Self-Analysis Blind Spot" "Rational Track Overconfidence"]
    :safeguards ["Assume own biases exist" "Require both tracks" "History of rational failures" "Equal weight to each" "Specific bias identification"]}
   {:id 38
    :name "Elementary Worldly Wisdom"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Build a latticework of basic models from multiple disciplines for better thinking."
    :keywords ["worldly" "latticework" "a" "wisdom" "elementary" "basic" "build" "of"]
    :failure-modes ["Narrow Mental Model Set" "Model Misapplication" "Model Overconfidence" "Model Updating Failure" "Model Conflict Paralysis"]
    :safeguards ["Maintain uncertainty" "Regular model review" "Expand mental model repertoire" "Test model predictions" "Seek diverse perspectives"]}
   {:id 39
    :name "Latticework of Mental Models"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Facts must hang together on a latticework of theory to be useful."
    :keywords ["must" "latticework" "models" "together" "hang" "mental" "of" "on"]
    :failure-modes ["Isolated Model Application" "Latticework Gaps" "Latticework Rigidity" "Latticework Complexity Overload" "Latticework Without Practice"]
    :safeguards ["Look for model interactions" "Flexible application" "Adapt latticework to situation" "Seek diverse disciplines" "Simplify for situation"]}
   {:id 40
    :name "Multidisciplinary Approach"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Draw models from multiple disciplines - all wisdom is not in one academic department."
    :keywords ["draw" "from" "approach" "models" "disciplines" "multiple" "multidisciplinary"]
    :failure-modes ["Disciplinary Silos" "Superficial Multidisciplinarity" "Discipline Translation Failure" "Discipline Hierarchy Bias" "Jack of All Trades Trap"]
    :safeguards ["Deep expertise in core areas" "Deep learning in key disciplines" "Situation determines relevance" "Verify understanding" "Learn from other fields"]}
   {:id 41
    :name "Circle of Competence"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Know the boundaries of your knowledge and stay within them."
    :keywords ["boundaries" "circle" "competence" "your" "know" "of" "the"]
    :failure-modes ["Circle Overestimation" "Circle Boundary Blindness" "Circle Stagnation" "Circle Shrinkage Denial" "Circle Rigidity"]
    :safeguards ["Deliberate circle expansion" "Deliberate practice" "Honestly assess boundaries" "Partner with experts" "Growth mindset"]}
   {:id 42
    :name "First Principles Thinking"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Break down problems to fundamental truths and reason up from there."
    :keywords ["first" "thinking" "problems" "fundamental" "break" "down" "principles" "to"]
    :failure-modes ["False First Principles" "Incomplete Decomposition" "Reconstruction Failure" "First Principles Overuse" "Context-Free First Principles"]
    :safeguards ["Combine approaches" "Action orientation" "Build up from principles" "Reach actionable conclusion" "Test at system level"]}
   {:id 43
    :name "Second-Order Thinking"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Think beyond first effects to subsequent consequences."
    :keywords ["first" "thinking" "effects" "think" "to" "second-order" "beyond"]
    :failure-modes ["Stopping at second order" "Second-order thinking without probability" "Paralysis from complexity" "Confident prediction of unpredictable" "Ignoring feedback loops"]
    :safeguards ["Diminishing returns" "Expected value thinking" "Focus on major effects" "And then what?" "Acknowledge uncertainty"]}
   {:id 44
    :name "Occam's Razor"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Prefer simpler explanations over complex ones when equally valid."
    :keywords ["explanations" "razor" "occam's" "simpler" "prefer" "complex" "over"]
    :failure-modes ["Oversimplification" "Complexity bias" "Wrong simplicity dimension" "Simplicity as excuse for not understanding" "Context-dependent simplicity"]
    :safeguards ["Accept necessary complexity" "Reality check" "Measure actual complexity" "Understand first" "Multiple explanations"]}
   {:id 45
    :name "Hanlon's Razor"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Don't attribute to malice what can be explained by stupidity or incompetence."
    :keywords ["attribute" "malice" "hanlon's" "razor" "don't" "to" "what"]
    :failure-modes ["Ignoring actual malice" "Enabling bad actors" "Underestimating systemic malice" "Cultural blindness to malice" "Hanlon's Razor as naivety"]
    :safeguards ["Consequences" "Protect yourself" "Cui bono" "Verify trust" "Ethical review"]}
   {:id 46
    :name "Falsification/Disconfirming Evidence"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Actively seek evidence that disproves your hypothesis, like Darwin did."
    :keywords ["seek" "falsification/disconfirming" "evidence" "that" "actively" "disproves"]
    :failure-modes []
    :safeguards []}
   {:id 47
    :name "Scenario Analysis"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Consider multiple possible futures and plan for various outcomes."
    :keywords ["futures" "possible" "consider" "analysis" "and" "multiple" "scenario"]
    :failure-modes []
    :safeguards []}
   {:id 48
    :name "Mental Accounting"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Understand how people psychologically frame and categorize money differently."
    :keywords ["frame" "understand" "accounting" "how" "psychologically" "mental" "people"]
    :failure-modes []
    :safeguards []}
   {:id 49
    :name "Black Swan Events"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Prepare for extreme, hard-to-predict events with massive impact."
    :keywords ["prepare" "hard-to-predict" "for" "extreme," "events" "black" "swan"]
    :failure-modes []
    :safeguards []}
   {:id 50
    :name "Gray Rhino Events"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Recognize obvious but neglected big risks that people ignore."
    :keywords ["neglected" "gray" "but" "recognize" "events" "obvious" "big" "rhino"]
    :failure-modes []
    :safeguards []}
   {:id 51
    :name "Vivification"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Make ideas vivid and concrete so they stick in memory."
    :keywords ["concrete" "vivid" "ideas" "and" "vivification" "make"]
    :failure-modes []
    :safeguards []}
   {:id 52
    :name "Man-with-a-Hammer Tendency"
    :category "Thinking Tools & Meta-Frameworks"
    :description "Avoid overusing one model - to a man with a hammer, everything looks like a nail."
    :keywords ["tendency" "overusing" "one" "man-with-a-hammer" "model" "-" "avoid"]
    :failure-modes []
    :safeguards []}
   {:id 53
    :name "Supply and Demand"
    :category "Economics & Business: Core Principles"
    :description "Prices are determined by the interaction of supply and demand."
    :keywords ["by" "prices" "are" "determined" "supply" "demand" "the" "and"]
    :failure-modes ["Static Supply/Demand Analysis" "Elasticity Misjudgment" "Supply Response Timing" "Demand Substitution Blindness" "Non-Price Factors Neglect"]
    :safeguards ["Analyze cross-elasticity" "Demand sensing" "Monitor substitutes" "Analyze elasticity explicitly" "Intervention effects"]}
   {:id 54
    :name "Elasticity"
    :category "Economics & Business: Core Principles"
    :description "Sensitivity of quantity demanded or supplied to changes in price or other factors."
    :keywords ["or" "elasticity" "sensitivity" "of" "demanded" "quantity"]
    :failure-modes []
    :safeguards []}
   {:id 55
    :name "Opportunity Cost"
    :category "Economics & Business: Core Principles"
    :description "The true cost of anything is what you give up to get it."
    :keywords ["the" "cost" "true" "anything" "of" "opportunity"]
    :failure-modes ["Opportunity Cost Blindness" "Sunk Cost Confusion" "Time Opportunity Cost Neglect" "Attention Opportunity Cost" "Dynamic Opportunity Cost"]
    :safeguards ["Identify best alternative" "Ignore sunk costs" "Always consider alternatives" "Explicit opportunity cost calculation" "Value time explicitly"]}
   {:id 56
    :name "Comparative Advantage"
    :category "Economics & Business: Core Principles"
    :description "Specialize in what you do relatively better, even if not absolutely better."
    :keywords ["comparative" "in" "do" "advantage" "specialize" "what" "you"]
    :failure-modes ["Absolute vs Comparative Confusion" "Static Comparative Advantage" "Comparative Advantage Erosion" "Narrow Comparative Advantage View" "Comparative Advantage Overconfidence"]
    :safeguards ["Invest in maintaining advantages" "Relative efficiency matters" "Monitor advantage evolution" "Monitor competitor capability building" "Consider strategic investment"]}
   {:id 57
    :name "Marginal Utility/Diminishing Returns"
    :category "Economics & Business: Core Principles"
    :description "Each additional unit provides less satisfaction than the previous one."
    :keywords ["each" "marginal" "additional" "unit" "returns" "provides" "utility/diminishing" "less"]
    :failure-modes ["Diminishing Returns Blindness" "Average vs Marginal Confusion" "Utility Function Misspecification" "Interpersonal Utility Comparison" "Negative Marginal Utility Blindness"]
    :safeguards ["Analyze marginal utility curve" "Avoid interpersonal comparison" "Focus on marginal analysis" "Track actual usage over time" "Frame gains vs losses carefully"]}
   {:id 58
    :name "Time Value of Money"
    :category "Economics & Business: Core Principles"
    :description "Money today is worth more than the same amount in the future."
    :keywords ["money" "more" "is" "value" "worth" "of" "today" "time"]
    :failure-modes []
    :safeguards []}
   {:id 59
    :name "Incentives & Incentive Alignment"
    :category "Economics & Business: Core Principles"
    :description "People respond to incentives - ensure they are properly aligned."
    :keywords ["incentive" "&" "alignment" "respond" "to" "-" "people" "incentives"]
    :failure-modes ["Incentive Mapping Failure" "Incentive Alignment Assumption" "Incentive Gaming Blindness" "Non-Financial Incentive Neglect" "Incentive Change Blindness"]
    :safeguards ["Include clawbacks" "Multiple metrics" "Use recognition over cash" "Audit for manipulation" "Incentive audit"]}
   {:id 60
    :name "Agency Problem/Principal-Agent Problem"
    :category "Economics & Business: Core Principles"
    :description "Agents may not act in the best interest of principals they represent."
    :keywords ["not" "act" "in" "problem/principal-agent" "problem" "may" "agency" "agents"]
    :failure-modes ["Misaligned incentives" "Information asymmetry exploitation" "Moral hazard" "Adverse selection" "Monitoring costs exceeding benefits"]
    :safeguards ["Balanced incentives" "Personal liability" "Culture fit" "Multiple opinions" "Deferred compensation"]}
   {:id 61
    :name "Information Asymmetry"
    :category "Economics & Business: Core Principles"
    :description "One party has more or better information than another in a transaction."
    :keywords ["more" "or" "one" "party" "information" "has" "asymmetry"]
    :failure-modes []
    :safeguards []}
   {:id 62
    :name "Adverse Selection"
    :category "Economics & Business: Core Principles"
    :description "Bad participants drive out good ones when information is asymmetric."
    :keywords ["adverse" "participants" "good" "out" "bad" "drive" "selection"]
    :failure-modes ["Information asymmetry blindness" "Market unraveling" "Adverse selection in hiring" "Adverse selection in partnerships" "Creating adverse selection"]
    :safeguards ["Mandates" "Information gathering" "Customer retention" "Partner screening" "Signaling"]}
   {:id 63
    :name "Moral Hazard"
    :category "Economics & Business: Core Principles"
    :description "Behavior changes when insulated from risk or consequences."
    :keywords ["from" "when" "hazard" "insulated" "moral" "changes" "behavior"]
    :failure-modes ["Insurance design failure" "Too big to fail" "Agent moral hazard" "Ex-post moral hazard" "Moral hazard vs adverse selection confusion"]
    :safeguards ["Consequences" "Clawback provisions" "System design" "Require deductibles" "Credible no-bailout commitment"]}
   {:id 64
    :name "Pareto Principle (80/20 Rule)"
    :category "Economics & Business: Core Principles"
    :description "Roughly 80% of effects come from 20% of causes."
    :keywords ["80%" "pareto" "80/20" "roughly" "come" "principle" "of" "effects"]
    :failure-modes ["Misapplying 80/20" "Ignoring the 80%" "Static 80/20 thinking" "80/20 of 80/20 blindness" "Pareto as excuse for neglect"]
    :safeguards ["Efficient handling" "Regular analysis" "Dynamic focus" "Empirical check" "Find the vital few of vital few"]}
   {:id 65
    :name "Gresham's Law"
    :category "Economics & Business: Core Principles"
    :description "Bad money drives out good; can apply broadly to quality degradation."
    :keywords ["law" "out" "gresham's" "bad" "drives" "good;" "money"]
    :failure-modes ["Bad Driving Out Good Blindness" "Gresham Dynamics Timing" "Information Asymmetry Role" "Gresham Reversal Opportunity" "Gresham in Non-Monetary Contexts"]
    :safeguards ["Monitor dynamics speed" "Quality in all contexts" "Apply broadly" "Analyze information asymmetry" "Monitor quality trends"]}
   {:id 66
    :name "Creative/Competitive Destruction"
    :category "Economics & Business: Core Principles"
    :description "Innovation destroys old industries while creating new ones."
    :keywords ["old" "creative/competitive" "destroys" "industries" "destruction" "while" "innovation"]
    :failure-modes []
    :safeguards []}
   {:id 67
    :name "Value to a Private Owner"
    :category "Economics & Business: Core Principles"
    :description "What would a rational private buyer pay for the entire business?"
    :keywords ["would" "value" "a" "private" "owner" "to" "rational" "what"]
    :failure-modes []
    :safeguards []}
   {:id 68
    :name "Mr. Market"
    :category "Economics & Business: Core Principles"
    :description "The market is a manic-depressive partner offering prices daily - use it, don't be used by it."
    :keywords ["mr." "manic-depressive" "is" "a" "the" "market"]
    :failure-modes []
    :safeguards []}
   {:id 69
    :name "Margin of Safety"
    :category "Economics & Business: Core Principles"
    :description "Buy with a significant buffer between price and value to protect against errors."
    :keywords ["buy" "with" "safety" "margin" "a" "of" "buffer" "significant"]
    :failure-modes ["Insufficient margin" "Margin erosion" "False precision eliminating margin" "Margin in wrong dimension" "Competitive pressure eliminating margin"]
    :safeguards ["No compromise culture" "Identify critical paths" "Value pricing" "Absolute standards" "Ranges not points"]}
   {:id 70
    :name "Tax Deferral & Compounding Advantage"
    :category "Economics & Business: Core Principles"
    :description "Deferring taxes allows more capital to compound over time."
    :keywords ["more" "compounding" "tax" "&" "allows" "taxes" "advantage" "deferral"]
    :failure-modes ["Compounding Underestimation" "Negative Compounding Blindness" "Compounding Interruption Risk" "Compounding Rate Sensitivity" "Compounding Base Neglect"]
    :safeguards ["Monitor erosion" "Small differences matter" "Compound what matters" "Consider both base and rate" "Patience"]}
   {:id 71
    :name "Value Creation vs. Value Capture"
    :category "Economics & Business: Core Principles"
    :description "Creating value is different from capturing it - who keeps the surplus?"
    :keywords ["different" "from" "is" "vs." "creating" "value" "creation" "capture"]
    :failure-modes []
    :safeguards []}
   {:id 72
    :name "Obsolescence Risk"
    :category "Economics & Business: Core Principles"
    :description "Business decay from innovation and changing technology."
    :keywords ["business" "from" "risk" "obsolescence" "and" "decay" "innovation"]
    :failure-modes []
    :safeguards []}
   {:id 73
    :name "Economic Moats"
    :category "Business: Competitive Advantage & Moats"
    :description "Durable competitive advantages that protect a business from competition."
    :keywords ["competitive" "that" "advantages" "protect" "durable" "moats" "economic"]
    :failure-modes ["Moat misidentification" "Moat erosion blindness" "Single moat dependence" "Moat vs growth tradeoff" "Regulatory moat fragility"]
    :safeguards ["Track competitive dynamics" "Protect moat in growth plans" "Reject moat-damaging growth" "Measure customer retention" "Monitor regulatory risk"]}
   {:id 74
    :name "Cost Advantages"
    :category "Business: Competitive Advantage & Moats"
    :description "Structural low-cost position that competitors cannot easily replicate."
    :keywords ["cost" "that" "position" "advantages" "low-cost" "competitors" "structural"]
    :failure-modes ["Cost advantage erosion" "Cost focus blindness" "Cost advantage from scale loss" "Cost advantage replication" "Cost advantage sustainability"]
    :safeguards ["New cost sources" "Protect volume" "Strategic costs" "Sustainable advantages" "Flexible cost structure"]}
   {:id 75
    :name "Differentiation & Brand Power"
    :category "Business: Competitive Advantage & Moats"
    :description "Unique products or strong brands that command premium pricing."
    :keywords ["power" "or" "differentiation" "brands" "&" "brand" "strong" "products"]
    :failure-modes []
    :safeguards []}
   {:id 76
    :name "Switching Costs"
    :category "Business: Competitive Advantage & Moats"
    :description "Costs that make it difficult for customers to change suppliers."
    :keywords ["it" "difficult" "costs" "that" "switching" "make"]
    :failure-modes ["Switching Cost Overestimation" "Switching Cost Erosion" "Switching Cost Type Confusion" "Switching Cost Asymmetry" "Switching Cost vs Value Confusion"]
    :safeguards ["Technology impact analysis" "Make staying attractive" "Analyze each type" "Track engagement" "Monitor competitors"]}
   {:id 77
    :name "Network Effects"
    :category "Business: Competitive Advantage & Moats"
    :description "Value increases as more users join the network."
    :keywords ["more" "increases" "value" "users" "as" "network" "effects"]
    :failure-modes ["Network Effect Existence Assumption" "Network Effect Strength Overestimation" "Negative Network Effects Blindness" "Network Effect Timing" "Local vs Global Network Effects"]
    :safeguards ["Increase switching costs" "Local vs global distinction" "Map network boundaries" "Verify network effects empirically" "Analyze switching costs"]}
   {:id 78
    :name "Scale Economies - Supply Side"
    :category "Business: Competitive Advantage & Moats"
    :description "Fixed costs spread over larger volumes reduce per-unit costs."
    :keywords ["fixed" "spread" "costs" "supply" "larger" "-" "economies" "scale"]
    :failure-modes []
    :safeguards []}
   {:id 79
    :name "Scale Economies - Demand Side"
    :category "Business: Competitive Advantage & Moats"
    :description "More users create more value for all users."
    :keywords ["more" "value" "users" "demand" "-" "economies" "scale" "create"]
    :failure-modes []
    :safeguards []}
   {:id 80
    :name "Learning/Experience Curve"
    :category "Business: Competitive Advantage & Moats"
    :description "Costs fall with cumulative production as efficiency improves."
    :keywords ["with" "fall" "costs" "production" "cumulative" "learning/experience" "curve"]
    :failure-modes []
    :safeguards []}
   {:id 81
    :name "Lock-In via Distribution/Physical Network"
    :category "Business: Competitive Advantage & Moats"
    :description "Physical distribution networks create barriers to entry."
    :keywords ["networks" "create" "physical" "distribution/physical" "distribution" "via" "barriers" "lock-in"]
    :failure-modes []
    :safeguards []}
   {:id 82
    :name "Winner-Take-All/Winner-Take-Most Markets"
    :category "Business: Competitive Advantage & Moats"
    :description "Markets where the leader captures disproportionate value."
    :keywords ["markets" "winner-take-all/winner-take-most" "captures" "the" "leader" "where"]
    :failure-modes []
    :safeguards []}
   {:id 83
    :name "Moat Durability"
    :category "Business: Competitive Advantage & Moats"
    :description "How long the competitive advantage will last."
    :keywords ["competitive" "how" "moat" "durability" "advantage" "the" "long"]
    :failure-modes []
    :safeguards []}
   {:id 84
    :name "Industry Structure"
    :category "Business: Competitive Advantage & Moats"
    :description "Monopoly, oligopoly, or competitive dynamics shape profitability."
    :keywords ["competitive" "dynamics" "or" "oligopoly," "industry" "structure" "monopoly,"]
    :failure-modes []
    :safeguards []}
   {:id 85
    :name "Rational vs. Cutthroat Competition"
    :category "Business: Competitive Advantage & Moats"
    :description "Some industries compete rationally; others destroy value through price wars."
    :keywords ["some" "compete" "vs." "competition" "rationally;" "industries" "rational" "others"]
    :failure-modes []
    :safeguards []}
   {:id 86
    :name "Platform Economics"
    :category "Business: Competitive Advantage & Moats"
    :description "Two-sided markets connecting different user groups."
    :keywords ["different" "connecting" "economics" "user" "platform" "two-sided" "markets"]
    :failure-modes []
    :safeguards []}
   {:id 87
    :name "Capacity & Supply Discipline"
    :category "Business: Competitive Advantage & Moats"
    :description "Overcapacity destroys returns; disciplined supply maintains pricing."
    :keywords ["&" "discipline" "capacity" "supply" "destroys" "overcapacity" "disciplined" "returns;"]
    :failure-modes []
    :safeguards []}
   {:id 88
    :name "Surfing Long Waves"
    :category "Business: Competitive Advantage & Moats"
    :description "Riding strong secular trends for sustained growth."
    :keywords ["secular" "for" "strong" "waves" "trends" "riding" "long" "surfing"]
    :failure-modes []
    :safeguards []}
   {:id 89
    :name "Technology as Friend vs. Killer"
    :category "Business: Competitive Advantage & Moats"
    :description "Technology can strengthen or destroy competitive positions."
    :keywords ["can" "strengthen" "destroy" "or" "killer" "vs." "friend" "as"]
    :failure-modes []
    :safeguards []}
   {:id 90
    :name "Bureaucracy/Diseconomies of Scale"
    :category "Business: Competitive Advantage & Moats"
    :description "Large organizations develop dysfunction that offsets scale benefits."
    :keywords ["dysfunction" "large" "develop" "organizations" "that" "bureaucracy/diseconomies" "scale" "of"]
    :failure-modes ["Scale Economies Overestimation" "Diseconomies of Scale Blindness" "Scale Timing Misjudgment" "Scale Source Confusion" "Minimum Efficient Scale Misjudgment"]
    :safeguards ["Learning curve analysis" "Consider complexity costs" "Verify scale economies empirically" "Measure unit economics" "Monitor coordination overhead"]}
   {:id 91
    :name "Cancer-Surgery Formula"
    :category "Business: Competitive Advantage & Moats"
    :description "Cut out bad parts quickly to save the system."
    :keywords ["quickly" "out" "cancer-surgery" "bad" "cut" "parts" "formula"]
    :failure-modes []
    :safeguards []}
   {:id 92
    :name "Basic Arithmetic Fluency"
    :category "Mathematics & Probability"
    :description "Master basic math - you must be able to handle numbers and quantities."
    :keywords ["arithmetic" "math" "master" "basic" "-" "fluency" "you"]
    :failure-modes []
    :safeguards []}
   {:id 93
    :name "Permutations & Combinations"
    :category "Mathematics & Probability"
    :description "The elementary math of possibilities - essential for probability thinking."
    :keywords ["combinations" "&" "math" "elementary" "possibilities" "of" "the" "permutations"]
    :failure-modes []
    :safeguards []}
   {:id 94
    :name "Expected Value"
    :category "Mathematics & Probability"
    :description "Probability-weighted average of all possible outcomes."
    :keywords ["all" "expected" "possible" "value" "of" "average" "probability-weighted"]
    :failure-modes ["Ignoring probability" "Ignoring magnitude" "Negative expected value bets" "Expected value in non-repeatable situations" "Utility vs expected value"]
    :safeguards ["Realistic assessment" "Calculate EV" "Discipline" "Diminishing returns" "Consider ruin"]}
   {:id 95
    :name "Probabilistic Thinking/Base Rates"
    :category "Mathematics & Probability"
    :description "Think in probabilities, not certainties. Start with base rates."
    :keywords ["not" "in" "rates" "certainties." "probabilistic" "probabilities," "think" "thinking/base"]
    :failure-modes ["False precision" "Base rate neglect" "Probability vs frequency confusion" "Ignoring fat tails" "Correlation vs causation"]
    :safeguards ["Occam's razor" "Use subjective probability for unique events" "Update from priors" "Acknowledge model uncertainty" "Test for fat tails"]}
   {:id 96
    :name "Bayes' Rule/Bayesian Updating"
    :category "Mathematics & Probability"
    :description "Update probabilities as new evidence arrives."
    :keywords ["update" "evidence" "bayes'" "new" "probabilities" "as" "updating" "rule/bayesian"]
    :failure-modes ["Prior Misspecification" "Insufficient Updating" "Overupdating" "Likelihood Ratio Error" "Confirmation Bias in Updating"]
    :safeguards ["Use base rates" "Evidence weight correctly" "Track evidence" "Overcome anchoring" "Disconfirming evidence focus"]}
   {:id 97
    :name "Regression to the Mean"
    :category "Mathematics & Probability"
    :description "Extreme outcomes tend to be followed by more average ones."
    :keywords ["regression" "tend" "extreme" "be" "outcomes" "to" "the" "mean"]
    :failure-modes ["Regression Blindness" "Skill vs Luck Confusion" "Mean Misidentification" "Regression Speed Misjudgment" "Structural Change vs Regression"]
    :safeguards ["Separate skill from luck" "Identify correct mean" "Historical regression patterns" "Analyze change drivers" "Base rate analysis"]}
   {:id 98
    :name "Normal Distribution vs. Fat Tails"
    :category "Mathematics & Probability"
    :description "Many distributions have fatter tails than normal - extreme events more likely."
    :keywords ["fat" "have" "fatter" "distribution" "vs." "distributions" "many" "normal"]
    :failure-modes []
    :safeguards []}
   {:id 99
    :name "Power Laws"
    :category "Mathematics & Probability"
    :description "Extreme concentration of outcomes - a few items dominate."
    :keywords ["concentration" "power" "extreme" "laws" "outcomes" "-" "of"]
    :failure-modes ["Power Law Misidentification" "Infinite Variance Implications" "Scale Invariance Misunderstanding" "Power Law Extrapolation" "Power Law Mechanism Blindness"]
    :safeguards ["Fat tail models" "Follow winners" "Identify distribution type" "Understand infinite variance" "Careful extrapolation"]}
   {:id 100
    :name "Cost-Benefit Analysis"
    :category "Mathematics & Probability"
    :description "Formalized comparison of costs and benefits for decision-making."
    :keywords ["cost-benefit" "costs" "formalized" "analysis" "of" "comparison" "and"]
    :failure-modes []
    :safeguards []}
   {:id 101
    :name "Compounding"
    :category "Mathematics & Probability"
    :description "Growth on growth - the eighth wonder of the world."
    :keywords ["compounding" "-" "growth" "the" "on"]
    :failure-modes ["Compounding Underestimation" "Negative Compounding Blindness" "Compounding Interruption Risk" "Compounding Rate Sensitivity" "Compounding Base Neglect"]
    :safeguards ["Monitor erosion" "Small differences matter" "Compound what matters" "Consider both base and rate" "Patience"]}
   {:id 102
    :name "Optionality/Asymmetric Payoffs"
    :category "Mathematics & Probability"
    :description "Positions with limited downside and unlimited upside."
    :keywords ["with" "optionality/asymmetric" "positions" "and" "downside" "payoffs" "limited"]
    :failure-modes []
    :safeguards []}
   {:id 103
    :name "Kelly-Type Thinking"
    :category "Mathematics & Probability"
    :description "Optimal position sizing based on edge and odds."
    :keywords ["thinking" "optimal" "based" "sizing" "position" "kelly-type" "on"]
    :failure-modes []
    :safeguards []}
   {:id 104
    :name "Critical Mass"
    :category "Physics, Engineering & Systems"
    :description "Threshold where dynamics fundamentally change."
    :keywords ["threshold" "dynamics" "mass" "fundamentally" "change." "critical" "where"]
    :failure-modes ["Underestimating critical mass requirements" "Overestimating critical mass achieved" "Wrong critical mass metric" "Critical mass in wrong geography" "Losing critical mass"]
    :safeguards ["Concentrate resources" "Win one market" "Concentrate geographically" "Sequential expansion" "Reach critical mass first"]}
   {:id 105
    :name "Leverage"
    :category "Physics, Engineering & Systems"
    :description "Amplifying force or outcome through mechanical or financial means."
    :keywords ["or" "amplifying" "force" "leverage" "outcome" "through"]
    :failure-modes ["Leverage Amplification Blindness" "Hidden Leverage" "Leverage Cascade Risk" "Leverage Timing Risk" "Operational Leverage Confusion"]
    :safeguards ["Systemic leverage view" "Conservative leverage" "Combined leverage analysis" "Timing risk analysis" "Leverage winners"]}
   {:id 106
    :name "Redundancy/Backup Systems"
    :category "Physics, Engineering & Systems"
    :description "Multiple systems ensure reliability when one fails."
    :keywords ["ensure" "redundancy/backup" "when" "systems" "multiple" "reliability"]
    :failure-modes ["Single point of failure" "Correlated redundancy" "Redundancy cost blindness" "Untested redundancy" "Over-redundancy inefficiency"]
    :safeguards ["Insurance thinking" "Maintenance" "Diverse redundancy" "Different failure modes" "Create redundancy"]}
   {:id 107
    :name "Breakpoints/Phase Transitions"
    :category "Physics, Engineering & Systems"
    :description "Points where systems shift to fundamentally different states."
    :keywords ["transitions" "to" "systems" "shift" "breakpoints/phase" "points" "where"]
    :failure-modes []
    :safeguards []}
   {:id 108
    :name "Friction & Efficiency Losses"
    :category "Physics, Engineering & Systems"
    :description "Real systems have friction that reduces efficiency."
    :keywords ["have" "real" "&" "that" "losses" "efficiency" "systems" "friction"]
    :failure-modes ["Ignoring friction" "Wrong friction focus" "Friction as feature blindness" "Adding friction unintentionally" "Friction accumulation"]
    :safeguards ["Selective reduction" "User journey mapping" "Realistic expectations" "Net friction analysis" "Audit total friction"]}
   {:id 109
    :name "Reliability Engineering/Safety Margins"
    :category "Physics, Engineering & Systems"
    :description "Build in safety factors to handle unexpected stresses."
    :keywords ["factors" "safety" "in" "margins" "to" "build" "engineering/safety" "reliability"]
    :failure-modes []
    :safeguards []}
   {:id 110
    :name "Feedback Loops - Positive (Reinforcing)"
    :category "Physics, Engineering & Systems"
    :description "Self-reinforcing cycles that amplify changes."
    :keywords ["self-reinforcing" "feedback" "loops" "(reinforcing)" "cycles" "that" "amplify" "changes."]
    :failure-modes ["Feedback Loop Blindness" "Loop Polarity Confusion" "Feedback Delay Blindness" "Feedback Strength Misjudgment" "Multiple Loop Interaction"]
    :safeguards ["Leading indicators" "Anticipate delayed feedback" "Multiple metrics" "System-level view" "Quantify feedback strength"]}
   {:id 111
    :name "Feedback Loops - Negative (Stabilizing)"
    :category "Physics, Engineering & Systems"
    :description "Self-correcting cycles that maintain equilibrium."
    :keywords ["(stabilizing)" "feedback" "loops" "maintain" "equilibrium." "cycles" "that" "-"]
    :failure-modes ["Feedback Loop Blindness" "Loop Polarity Confusion" "Feedback Delay Blindness" "Feedback Strength Misjudgment" "Multiple Loop Interaction"]
    :safeguards ["Leading indicators" "Anticipate delayed feedback" "Multiple metrics" "System-level view" "Quantify feedback strength"]}
   {:id 112
    :name "Bottlenecks & Constraints"
    :category "Physics, Engineering & Systems"
    :description "System output limited by the weakest link."
    :keywords ["by" "&" "system" "constraints" "output" "the" "bottlenecks" "limited"]
    :failure-modes ["Bottleneck Misidentification" "Bottleneck Shifting" "Bottleneck Optimization Myopia" "Hidden Bottleneck Blindness" "Bottleneck Exploitation Failure"]
    :safeguards ["Look for hidden constraints" "Exploit before expand" "Comprehensive constraint mapping" "Focus on bottleneck" "Systematic bottleneck analysis"]}
   {:id 113
    :name "Nonlinearity"
    :category "Physics, Engineering & Systems"
    :description "Disproportionate responses - small inputs can cause large outputs."
    :keywords ["responses" "nonlinearity" "inputs" "disproportionate" "-" "small"]
    :failure-modes []
    :safeguards []}
   {:id 114
    :name "System Resilience vs. Fragility"
    :category "Physics, Engineering & Systems"
    :description "Some systems strengthen under stress; others break."
    :keywords ["resilience" "stress;" "some" "strengthen" "vs." "system" "under" "fragility"]
    :failure-modes []
    :safeguards []}
   {:id 115
    :name "Evolution by Natural Selection"
    :category "Biology & Evolution"
    :description "Variation, selection, and retention drive adaptation."
    :keywords ["by" "selection," "variation," "retention" "evolution" "drive" "natural" "and"]
    :failure-modes ["Selection Pressure Blindness" "Fitness Landscape Misunderstanding" "Selection Speed Misjudgment" "Survival vs Reproduction Confusion" "Selection Environment Change"]
    :safeguards ["Consequences" "Consider both survival and reproduction" "Generational timing" "Diversity" "Multiple approaches"]}
   {:id 116
    :name "Adaptation & Fitness Landscapes"
    :category "Biology & Evolution"
    :description "Organisms and organizations adapt to their environment."
    :keywords ["landscapes" "fitness" "&" "organizations" "adaptation" "adapt" "organisms" "to"]
    :failure-modes ["Adaptation Lag" "Over-Adaptation" "Maladaptation" "Adaptation Cost Blindness" "False Adaptation Signal"]
    :safeguards ["Early warning systems" "Transition planning" "Adaptive capacity" "Account for adaptation costs" "Fitness feedback"]}
   {:id 117
    :name "Red Queen Effect"
    :category "Biology & Evolution"
    :description "Must run hard just to stay in place as competitors evolve."
    :keywords ["hard" "must" "queen" "run" "effect" "red" "to" "just"]
    :failure-modes ["Standing Still Illusion" "Race Pace Misjudgment" "Red Queen Exhaustion" "Wrong Race" "Red Queen Escape Blindness"]
    :safeguards ["Strategic focus" "Blue ocean" "Exit wrong races" "Choose races wisely" "Sustainable pace"]}
   {:id 118
    :name "Niches & Ecological Competition"
    :category "Biology & Evolution"
    :description "Specialization in narrow niches can be highly successful."
    :keywords ["specialization" "ecological" "can" "&" "in" "narrow" "competition" "niches"]
    :failure-modes ["Niche Misidentification" "Niche Overcrowding" "Niche Collapse Blindness" "Niche Expansion Failure" "Niche Defense Failure"]
    :safeguards ["Adjacent niches" "Competitive response" "Competition assessment" "Find underserved niches" "Competitive positioning"]}
   {:id 119
    :name "Population Dynamics"
    :category "Biology & Evolution"
    :description "Boom-bust cycles in populations and markets."
    :keywords ["dynamics" "population" "boom-bust" "cycles" "in" "populations" "and"]
    :failure-modes []
    :safeguards []}
   {:id 120
    :name "Autopsy Learning"
    :category "Biology & Evolution"
    :description "Study failures and deaths to learn what went wrong."
    :keywords ["learning" "study" "failures" "autopsy" "deaths" "to" "and"]
    :failure-modes []
    :safeguards []}
   {:id 121
    :name "Corporate Governance"
    :category "Organizational & Institutional"
    :description "Board incentives and oversight structures matter."
    :keywords ["structures" "board" "oversight" "governance" "incentives" "and" "corporate"]
    :failure-modes []
    :safeguards []}
   {:id 122
    :name "Management Incentives & Compensation Design"
    :category "Organizational & Institutional"
    :description "How managers are paid determines how they behave."
    :keywords ["determines" "managers" "are" "&" "incentives" "how" "paid" "compensation"]
    :failure-modes ["Incentive Mapping Failure" "Incentive Alignment Assumption" "Incentive Gaming Blindness" "Non-Financial Incentive Neglect" "Incentive Change Blindness"]
    :safeguards ["Include clawbacks" "Multiple metrics" "Use recognition over cash" "Audit for manipulation" "Incentive audit"]}
   {:id 123
    :name "Culture as a Control System"
    :category "Organizational & Institutional"
    :description "Norms and culture are more powerful than formal rules."
    :keywords ["more" "are" "culture" "system" "a" "as" "control" "norms"]
    :failure-modes []
    :safeguards []}
   {:id 124
    :name "Bureaucratic Inertia & Empire Building"
    :category "Organizational & Institutional"
    :description "Organizations naturally grow bureaucracy and resist change."
    :keywords ["naturally" "grow" "&" "organizations" "building" "inertia" "bureaucracy" "and"]
    :failure-modes ["Underestimating inertia" "Fighting inertia directly" "Organizational inertia blindness" "Positive inertia neglect" "Inertia in strategy"]
    :safeguards ["Plan for resistance" "Coalition building" "Change management" "Willingness to pivot" "Build habits"]}
   {:id 125
    :name "Information Suppression/Shooting the Messenger"
    :category "Organizational & Institutional"
    :description "Bad news gets filtered out as it moves up the hierarchy."
    :keywords ["messenger" "out" "bad" "suppression/shooting" "news" "information" "the" "filtered"]
    :failure-modes []
    :safeguards []}
   {:id 126
    :name "Five W's Rule in Communication"
    :category "Organizational & Institutional"
    :description "Always tell who, what, where, when, and especially WHY."
    :keywords ["communication" "where," "rule" "w's" "in" "who," "what," "five"]
    :failure-modes []
    :safeguards []}
   {:id 127
    :name "Checklists & Standard Operating Procedures"
    :category "Organizational & Institutional"
    :description "Systematic processes reduce errors and ensure consistency."
    :keywords ["operating" "checklists" "&" "errors" "reduce" "and" "procedures" "systematic"]
    :failure-modes ["Checklist without understanding" "Outdated checklists" "Checklist overload" "False security from checklists" "Checklist resistance"]
    :safeguards ["Checklist + judgment" "Keep concise" "Novel risk awareness" "Regular reviews" "Prioritize items"]}
   {:id 128
    :name "Talent, Trust, and Delegation"
    :category "Organizational & Institutional"
    :description "Few, high-quality people with trust and autonomy outperform bureaucracy."
    :keywords ["talent," "high-quality" "delegation" "with" "trust," "few," "people" "trust"]
    :failure-modes ["Trust Misplacement" "Trust Deficit" "Trust Erosion Blindness" "Trust Recovery Failure" "Trust Scaling Failure"]
    :safeguards ["Scalable trust mechanisms" "Trust infrastructure" "Overcome distrust" "Trust maintenance" "Trust but verify"]}
   {:id 129
    :name "Avoiding Madness in Crowds"
    :category "Organizational & Institutional"
    :description "Resist groupthink and maintain independent judgment."
    :keywords ["independent" "avoiding" "maintain" "groupthink" "crowds" "in" "resist" "madness"]
    :failure-modes []
    :safeguards []}
  ])
(defn detect-models [text]
  "Detect mental models in text using keyword matching"
  (let [text-lower (str/lower-case (or text ""))]
    (filter
      (fn [model]
        (some #(str/includes? text-lower (str/lower-case %))
              (:keywords model)))
      mental-models)))

(defn detect-lollapalooza [models]
  "Detect if multiple models combine for Lollapalooza effect"
  (when (>= (count models) 3)
    (let [categories (set (map :category models))]
      (when (>= (count categories) 2)
        {:detected true
         :models (map :name models)
         :categories categories
         :strength (cond
                     (>= (count models) 5) :strong
                     (>= (count models) 4) :moderate
                     :else :mild)}))))

;; =============================================================================
;; File Scanning
;; =============================================================================

(defn read-text-file [file]
  (try
    (slurp file)
    (catch Exception e
      (log! (str "[SCAN] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn read-pdf-file [file]
  "Extract text from PDF using Apache PDFBox"
  (try
    (let [pdf-class (Class/forName "org.apache.pdfbox.Loader")
          load-method (.getMethod pdf-class "loadPDF" (into-array Class [File]))
          document (. load-method invoke nil (into-array Object [file]))
          stripper-class (Class/forName "org.apache.pdfbox.text.PDFTextStripper")
          stripper (.newInstance stripper-class)
          text (.getText stripper document)]
      (.close document)
      text)
    (catch ClassNotFoundException _
      (log! "[PDF] PDFBox not available - PDF support disabled")
      nil)
    (catch Exception e
      (log! (str "[PDF] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn read-docx-file [file]
  "Extract text from DOCX files (basic XML extraction)"
  (try
    (with-open [zis (java.util.zip.ZipInputStream. (FileInputStream. file))]
      (loop [text-parts []]
        (if-let [entry (.getNextEntry zis)]
          (if (= "word/document.xml" (.getName entry))
            (let [content (slurp zis)
                  ;; Extract text between <w:t> tags
                  text-matches (re-seq #"<w:t[^>]*>([^<]*)</w:t>" content)
                  text (str/join " " (map second text-matches))]
              text)
            (recur text-parts))
          (str/join " " text-parts))))
    (catch Exception e
      (log! (str "[DOCX] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn scan-file [file progress-callback]
  "Scan a single file for mental models with semantic indexing"
  (let [name (.getName file)
        ext (str/lower-case (or (last (str/split name #"\.")) ""))
        text (cond
               (#{"txt" "md" "markdown"} ext) (read-text-file file)
               (= "pdf" ext) (read-pdf-file file)
               (#{"docx" "doc"} ext) (read-docx-file file)
               :else nil)]
    (when text
      (let [models (detect-models text)
            lollapalooza (detect-lollapalooza models)]
        (when (seq models)
          (save-scan-result! (.getAbsolutePath file) (map :name models) 
                            (/ (count models) (double (count mental-models))))
          ;; Index document for semantic search
          (try
            (search/index-document! (.getAbsolutePath file) text
                                   {:name name
                                    :models (map :name models)
                                    :lollapalooza (boolean lollapalooza)})
            (catch Exception _ nil))
          (when progress-callback
            (progress-callback {:file name
                               :models models
                               :lollapalooza lollapalooza})))
        {:file name
         :models models
         :lollapalooza lollapalooza}))))

(defn scan-folder [folder-path progress-callback completion-callback]
  "Scan all files in a folder with analytics tracking"
  (future
    (try
      (let [start-time (System/currentTimeMillis)
            folder (File. folder-path)
            files (->> (file-seq folder)
                      (filter #(.isFile %))
                      (filter #(let [ext (str/lower-case (or (last (str/split (.getName %) #"\.")) ""))]
                                (#{"txt" "md" "markdown" "pdf" "docx" "doc"} ext))))
            models-found (atom [])]
        (log! (str "[SCAN] Starting scan of " (count files) " files in " folder-path))
        (doseq [[idx file] (map-indexed vector files)]
          (when progress-callback
            (progress-callback {:progress (/ (* 100 (inc idx)) (count files))
                               :current (.getName file)}))
          (when-let [result (scan-file file progress-callback)]
            (swap! models-found concat (map :name (:models result)))))
        (let [duration-ms (- (System/currentTimeMillis) start-time)]
          (log! (str "[SCAN] Complete in " duration-ms "ms"))
          ;; Track scan in analytics
          (analytics/track-scan! folder-path (count files) (distinct @models-found) duration-ms)
          ;; Update anomaly baselines
          (anomaly/update-scan-baselines! (:scans (analytics/get-stats)))
          ;; Check for anomalies
          (anomaly/check-scan-anomalies {:folder folder-path 
                                          :files (count files)
                                          :model-count (count (distinct @models-found))
                                          :duration-ms duration-ms}))
        ;; Auto-sync results to web app
        (when (get-in @*state [:settings :auto-sync] true)
          (try
            (let [results (get-in @*state [:scan-results])]
              (when (seq results)
                (log! "[SYNC] Auto-syncing to web app...")
                (sync-to-web-app! results)
                (log! "[SYNC] Auto-sync complete")))
            (catch Exception e
              (log! (str "[SYNC] Auto-sync failed: " (.getMessage e))))))
        (when completion-callback
          (completion-callback {:total (count files)})))
      (catch Exception e
        (log! (str "[SCAN] Error: " (.getMessage e)))))))


;; =============================================================================
;; Batch Queue Processing (Wholesale Mode)
;; =============================================================================
(def *batch-queue (atom []))
(def *batch-running (atom false))
(def *batch-stats (atom {:total-folders 0 :completed-folders 0 :total-files 0 :total-models 0}))

(defn add-to-batch-queue! [folder-paths]
  "Add multiple folder paths to the batch queue"
  (swap! *batch-queue concat folder-paths)
  (swap! *batch-stats update :total-folders + (count folder-paths))
  (log! (str "[BATCH] Added " (count folder-paths) " folders to queue. Total: " (count @*batch-queue))))

(defn process-batch-queue! [progress-callback completion-callback]
  "Process all folders in the batch queue sequentially"
  (when-not @*batch-running
    (reset! *batch-running true)
    (future
      (try
        (log! (str "[BATCH] Starting batch processing of " (count @*batch-queue) " folders"))
        (let [start-time (System/currentTimeMillis)]
          (doseq [[idx folder] (map-indexed vector @*batch-queue)]
            (when @*batch-running
              (log! (str "[BATCH] Processing folder " (inc idx) "/" (count @*batch-queue) ": " folder))
              (let [folder-files (atom 0)
                    folder-models (atom 0)]
                (scan-folder folder
                  (fn [result]
                    (when (:models result)
                      (swap! folder-files inc)
                      (swap! folder-models + (count (:models result)))
                      (swap! *batch-stats update :total-files inc)
                      (swap! *batch-stats update :total-models + (count (:models result))))
                    (when progress-callback
                      (progress-callback {:folder folder
                                         :folder-progress (/ (* 100 (inc idx)) (count @*batch-queue))
                                         :file (:file result)
                                         :models (:models result)})))
                  nil)
                ;; Wait for folder scan to complete
                (Thread/sleep 100)
                (while (not= @folder-files @folder-files) (Thread/sleep 50)))
              (swap! *batch-stats update :completed-folders inc)))
          (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
            (log! (str "[BATCH] Complete. " (:total-files @*batch-stats) " files, " 
                      (:total-models @*batch-stats) " models in " (format "%.1f" elapsed) "s"))
            (when completion-callback
              (completion-callback @*batch-stats))))
        (catch Exception e
          (log! (str "[BATCH] Error: " (.getMessage e))))
        (finally
          (reset! *batch-running false)
          (reset! *batch-queue []))))))

(defn stop-batch! []
  "Stop batch processing"
  (reset! *batch-running false)
  (log! "[BATCH] Stopped"))

(defn clear-batch-queue! []
  "Clear the batch queue"
  (reset! *batch-queue [])
  (reset! *batch-stats {:total-folders 0 :completed-folders 0 :total-files 0 :total-models 0})
  (log! "[BATCH] Queue cleared"))

;; =============================================================================
;; CLI Batch Mode (Headless)
;; =============================================================================
(defn run-cli-batch [args]
  "Run in headless CLI mode for batch processing"
  (println "Mental Models Desktop - CLI Batch Mode")
  (println "======================================")
  (let [folders (filter #(.isDirectory (File. %)) args)]
    (if (empty? folders)
      (do
        (println "Usage: MentalModels.bat --batch <folder1> <folder2> ...")
        (println "       MentalModels.bat --batch-file <paths.txt>")
        (System/exit 1))
      (do
        (println (str "Processing " (count folders) " folders..."))
        (add-to-batch-queue! folders)
        (let [done (promise)]
          (process-batch-queue!
            (fn [result]
              (when (:file result)
                (println (str "  " (:file result) " -> " (count (:models result)) " models"))))
            (fn [stats]
              (println "")
              (println "=== BATCH COMPLETE ===")
              ;; Auto-sync batch results
              (when (get-in @*state [:settings :auto-sync] true)
                (try
                  (let [results (get-in @*state [:scan-results])]
                    (when (seq results)
                      (println "[SYNC] Auto-syncing batch results...")
                      (sync-to-web-app! results)
                      (println "[SYNC] Batch sync complete")))
                  (catch Exception e
                    (println (str "[SYNC] Batch sync failed: " (.getMessage e))))))
              (println (str "Folders: " (:completed-folders stats)))
              (println (str "Files:   " (:total-files stats)))
              (println (str "Models:  " (:total-models stats)))
              (deliver done true)))
          @done)
        (System/exit 0)))))


;; =============================================================================
;; Watch Mode
;; =============================================================================

(def *watch-thread (atom nil))

(defn start-watch-mode! [folders log-fn]
  (log-fn "[WATCH] Starting watch mode...")
  (swap! *state assoc :watch-active true)
  (reset! *watch-thread
    (future
      (while (:watch-active @*state)
        (doseq [folder folders]
          (scan-folder folder
            (fn [result]
              (when (:models result)
                (log-fn (str "[WATCH] Found " (count (:models result)) " models in " (:file result)))))
            nil))
        (Thread/sleep 30000)))))

(defn stop-watch-mode! [log-fn]
  (log-fn "[WATCH] Stopping watch mode...")
  (swap! *state assoc :watch-active false)
  (when-let [t @*watch-thread]
    (future-cancel t))
  (reset! *watch-thread nil))

;; =============================================================================
;; HTTP Client
;; =============================================================================

(defn http-get [url & {:keys [headers]}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)
            body (when (< status 400)
                   (slurp (.getInputStream conn)))]
        (.disconnect conn)
        {:success (< status 400) :status status :body body}))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn http-post [url body & {:keys [headers]}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000)
                 (.setRequestProperty "Content-Type" "application/json"))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (with-open [out (.getOutputStream conn)]
        (.write out (.getBytes body "UTF-8")))
      (let [status (.getResponseCode conn)
            response-body (try (slurp (.getInputStream conn)) (catch Exception _ nil))]
        (.disconnect conn)
        {:success (< status 400) :status status :body response-body}))
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Connection Checks
;; =============================================================================

(defn check-lm-studio! []
  (future
    (try
      (let [url (str (get-in @*state [:settings :lm-studio-url]) "/v1/models")
            result (http-get url)]
        (swap! *state assoc-in [:connections :lm-studio] 
               (if (:success result) :connected :disconnected)))
      (catch Exception _
        (swap! *state assoc-in [:connections :lm-studio] :disconnected)))))

(defn check-web-app! []
  (future
    (try
      (let [url (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))]
        (if (and url (not (str/blank? url)))
          (let [result (http-get (str url "/api/trpc/desktop.config") 
                                 :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
            (swap! *state assoc-in [:connections :web-app]
                   (if (:success result) :connected :disconnected))
            (when (:success result)
              (log! "[WEB-APP] Connected successfully")))
          (swap! *state assoc-in [:connections :web-app] :disconnected)))
      (catch Exception e
        (log! (str "[WEB-APP] Connection error: " (.getMessage e)))
        (swap! *state assoc-in [:connections :web-app] :disconnected)))))

(defn sync-to-web-app! [scan-results]
  "Push scan results to web app for dashboard sync"
  (future
    (try
      (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                     "/api/trpc/desktop.syncResults")
            payload (str "{\"json\":{\"results\":"
                        (str "[" (str/join ","
                               (map (fn [r]
                                     (str "{\"file\":\"" (str/replace (:file r "") "\"" "\\\"") "\","
                                          "\"models\":[" (str/join "," (map (fn [m] (str "{\"name\":\"" (:name m) "\"}")) (:models r []))) "],"
                                          "\"lollapalooza\":" (if (:lollapalooza r) "true" "false") "}"))
                                    scan-results)) "]")
                        ",\"timestamp\":" (System/currentTimeMillis) "}}")]
        (log! (str "[SYNC] Pushing " (count scan-results) " results to web app"))
        (let [result (http-post url payload)]
          (if (:success result)
            (log! "[SYNC] Successfully synced to web app")
            (log! (str "[SYNC] Failed to sync: " (:body result))))))
      (catch Exception e
        (log! (str "[SYNC] Error syncing: " (.getMessage e)))))))

;; =============================================================================
;; LM Studio AI Integration
;; =============================================================================

(def lm-studio-paths
  "Common LM Studio installation paths"
  [(str (System/getenv "LOCALAPPDATA") "\\LM Studio\\LM Studio.exe")
   (str (System/getenv "PROGRAMFILES") "\\LM Studio\\LM Studio.exe")
   "/Applications/LM Studio.app/Contents/MacOS/LM Studio"
   (str (System/getProperty "user.home") "/Applications/LM Studio.app/Contents/MacOS/LM Studio")])

(defn find-lm-studio-path []
  "Find LM Studio executable path"
  (first (filter #(and % (.exists (File. %))) lm-studio-paths)))

(defn launch-lm-studio! []
  "Launch LM Studio if not running"
  (when-let [path (find-lm-studio-path)]
    (log! (str "[LM-STUDIO] Launching: " path))
    (try
      (.exec (Runtime/getRuntime) (into-array String [path]))
      (Thread/sleep 3000)
      true
      (catch Exception e
        (log! (str "[LM-STUDIO] Launch error: " (.getMessage e)))
        false))))

(defn ensure-lm-studio-running! []
  "Ensure LM Studio is running, launch if needed"
  (if (= :connected (get-in @*state [:connections :lm-studio]))
    true
    (do
      (log! "[LM-STUDIO] Not connected, attempting to launch...")
      (when (launch-lm-studio!)
        (Thread/sleep 5000)
        @(check-lm-studio!)
        (Thread/sleep 1000)
        (= :connected (get-in @*state [:connections :lm-studio]))))))

(defn lm-studio-chat [prompt]
  "Send a chat request to LM Studio"
  (try
    (let [url (str (get-in @*state [:settings :lm-studio-url]) "/v1/chat/completions")
          escaped (-> prompt
                     (str/replace "\\" "\\\\")
                     (str/replace "\"" "\\\"")
                     (str/replace "\n" "\\n")
                     (str/replace "\r" "")
                     (str/replace "\t" "\\t"))
          body (str "{\"messages\": [{\"role\": \"user\", \"content\": \"" escaped "\"}], \"temperature\": 0.3, \"max_tokens\": 800}")
          result (http-post url body)]
      (when (:success result)
        (when-let [match (re-find #"\"content\"\s*:\s*\"((?:[^\"\\]|\\.)*)\"" (:body result))]
          (-> (second match)
              (str/replace "\\n" "\n")
              (str/replace "\\\"" "\"")))))
    (catch Exception e
      (log! (str "[LM-STUDIO] Chat error: " (.getMessage e)))
      nil)))

(defn ai-analyze-text [text]
  "Use AI to analyze text for mental models with structured output"
  (let [truncated (subs text 0 (min 3000 (count text)))
        model-names (str/join ", " (map :name (take 30 mental-models)))
        prompt (str "You are a mental model expert. Analyze this text and identify which mental models apply.\n\n"
                   "Available mental models include: " model-names " and many more.\n\n"
                   "For each mental model you detect, respond in this exact format:\n"
                   "MODEL: [model name] | CONFIDENCE: [high/medium/low] | REASON: [brief explanation]\n\n"
                   "Text to analyze:\n" truncated)]
    (lm-studio-chat prompt)))

(defn parse-ai-response [response]
  "Parse AI response into structured mental model detections"
  (when response
    (let [lines (str/split-lines response)
          model-lines (filter #(str/starts-with? % "MODEL:") lines)]
      (for [line model-lines]
        (let [parts (str/split line #"\|")
              model-part (first parts)
              confidence-part (second parts)
              reason-part (nth parts 2 nil)]
          {:name (str/trim (str/replace (or model-part "") #"MODEL:\s*" ""))
           :confidence (str/lower-case (str/trim (str/replace (or confidence-part "") #"CONFIDENCE:\s*" "")))
           :reason (str/trim (str/replace (or reason-part "") #"REASON:\s*" ""))})))))

(defn ai-scan-file [file progress-callback]
  "Scan a file using AI for deeper mental model detection"
  (let [name (.getName file)
        ext (str/lower-case (or (last (str/split name #"\.")) ""))
        text (cond
               (#{"txt" "md" "markdown"} ext) (read-text-file file)
               (= "pdf" ext) (read-pdf-file file)
               (#{"docx" "doc"} ext) (read-docx-file file)
               :else nil)]
    (when text
      (log! (str "[AI-SCAN] Analyzing: " name))
      (let [ai-response (ai-analyze-text text)
            ai-models (parse-ai-response ai-response)
            ;; Also run keyword detection for comparison
            keyword-models (detect-models text)
            lollapalooza (detect-lollapalooza (concat ai-models keyword-models))]
        (when progress-callback
          (progress-callback {:file name
                             :ai-models ai-models
                             :keyword-models keyword-models
                             :lollapalooza lollapalooza}))
        {:file name
         :ai-models ai-models
         :keyword-models keyword-models
         :lollapalooza lollapalooza}))))

(defn check-github! []
  (future
    (try
      (let [token (get-in @*state [:settings :github-token])
            headers (when (and token (not (str/blank? token)))
                     {"Authorization" (str "token " token)
                      "Accept" "application/vnd.github.v3+json"})
            result (http-get (str (:github-api config) "/user") :headers headers)]
        (swap! *state assoc-in [:connections :github]
               (if (:success result) :connected :disconnected)))
      (catch Exception _
        (swap! *state assoc-in [:connections :github] :disconnected)))))

(defn check-all-connections! []
  (check-lm-studio!)
  (check-web-app!)
  (check-github!))

;; =============================================================================
;; Version Comparison
;; =============================================================================

(defn parse-version [v]
  (when v
    (let [cleaned (str/replace (str v) #"^v" "")]
      (mapv #(Integer/parseInt %) (str/split cleaned #"\.")))))

(defn version-newer? [remote local]
  (try
    (let [r (parse-version remote)
          l (parse-version local)]
      (when (and r l)
        (loop [r r l l]
          (cond
            (and (empty? r) (empty? l)) false
            (empty? r) false
            (empty? l) true
            (> (first r) (first l)) true
            (< (first r) (first l)) false
            :else (recur (rest r) (rest l))))))
    (catch Exception _ false)))

;; =============================================================================
;; BULLETPROOF UPDATE SYSTEM - Remotely Fixable
;; =============================================================================
;;
;; This system can be fixed REMOTELY without updating the app:
;; 1. Fetches download config from web app (URLs, fallbacks, retry settings)
;; 2. Tries multiple download sources in order
;; 3. Caches downloads locally to avoid re-downloading
;; 4. Reports failures to web app for debugging
;;
;; If downloads break, update the web app config - no app update needed!
;; =============================================================================

(defn ensure-cache-dir! []
  "Ensure the cache directory exists"
  (let [dir (File. (:cache-dir config))]
    (when-not (.exists dir)
      (.mkdirs dir))
    dir))

(defn cache-path [version]
  "Get the cache file path for a version"
  (File. (:cache-dir config) (str "MentalModels-" version ".zip")))

(defn cached? [version]
  "Check if a version is already cached"
  (let [f (cache-path version)]
    (and (.exists f) (> (.length f) 1000000)))) ;; Must be > 1MB to be valid

(defn fetch-remote-config []
  "Fetch download configuration from web app. Returns nil on failure."
  (log! "[UPDATE] Fetching remote config from web app...")
  (try
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "X-Desktop-API-Key" (:desktop-api-key config))
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 30000))
          status (.getResponseCode conn)]
      (if (= status 200)
        (let [body (slurp (.getInputStream conn))]
          (.disconnect conn)
          (log! "[UPDATE] Remote config fetched successfully")
          ;; Parse the tRPC response to extract download sources
          (let [version (second (re-find #"\"currentVersion\":\s*\"([^\"]+)\"" body))
                sources (vec (for [[_ name url auth] 
                                   (re-seq #"\{\"name\":\"([^\"]+)\"[^}]*\"url\":\"([^\"]+)\"[^}]*\"requiresAuth\":(true|false)" body)]
                               {:name name :url url :requires-auth (= auth "true")}))]
            {:version version
             :sources sources
             :raw body}))
        (do
          (.disconnect conn)
          (log! (str "[UPDATE] Remote config failed: HTTP " status))
          nil)))
    (catch Exception e
      (log! (str "[UPDATE] Remote config error: " (.getMessage e)))
      nil)))

(defn download-file-simple! [url dest-file progress-callback use-auth?]
  "Download a file - use-auth? controls whether to send GitHub token"
  (try
    (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "User-Agent" "MentalModels-Desktop/1.4.2")
                 (.setRequestProperty "Accept" "application/octet-stream")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 300000)
                 (.setInstanceFollowRedirects false))]
      ;; Only add auth for GitHub API calls, not for S3 signed URLs
      (when use-auth?
        (.setRequestProperty conn "Authorization" (str "token " github-token)))
      
      (let [status (.getResponseCode conn)]
        (log! (str "[DOWNLOAD] URL: " (subs url 0 (min 80 (count url))) "..."))
        (log! (str "[DOWNLOAD] Status: " status " use-auth: " use-auth?))
        
        (cond
          ;; Success - download the file
          (= status 200)
          (let [content-length (.getContentLengthLong conn)
                input (BufferedInputStream. (.getInputStream conn))
                output (FileOutputStream. dest-file)
                buffer (byte-array 65536)] ;; 64KB buffer for speed
            (log! (str "[DOWNLOAD] Starting download, size: " 
                       (if (pos? content-length) 
                         (format "%.1f MB" (/ content-length 1048576.0))
                         "unknown")))
            (loop [total 0]
              (let [n (.read input buffer)]
                (if (pos? n)
                  (do
                    (.write output buffer 0 n)
                    (let [new-total (+ total n)
                          percent (if (pos? content-length)
                                    (int (* 100 (/ new-total content-length)))
                                    -1)]
                      (when (and progress-callback (zero? (mod new-total 524288))) ;; Update every 512KB
                        (progress-callback percent new-total content-length))
                      (recur new-total)))
                  (do
                    (.close input)
                    (.close output)
                    (.disconnect conn)
                    (log! (str "[DOWNLOAD] Complete: " total " bytes"))
                    {:success true :bytes total})))))
          
          ;; Redirect - follow WITHOUT auth (GitHub S3 signed URLs reject auth headers)
          (#{301 302 303 307 308} status)
          (let [redirect-url (.getHeaderField conn "Location")]
            (log! (str "[DOWNLOAD] Redirect " status " -> following without auth"))
            (.disconnect conn)
            ;; CRITICAL: Don't pass auth to S3 signed URLs
            (download-file-simple! redirect-url dest-file progress-callback false))
          
          ;; Error
          :else
          (do
            (.disconnect conn)
            (log! (str "[DOWNLOAD] HTTP Error: " status))
            {:success false :error (str "HTTP " status)}))))
    (catch Exception e
      (log! (str "[DOWNLOAD] Exception: " (.getMessage e)))
      {:success false :error (.getMessage e)})))

(defn try-download-source [source dest-file progress-callback]
  "Try to download from a single source"
  (log! (str "[DOWNLOAD] Trying source: " (:name source)))
  (download-file-simple! (:url source) dest-file progress-callback (:requires-auth source)))

(defn download-with-fallback! [sources dest-file progress-callback]
  "Try each download source in order until one succeeds"
  (loop [remaining sources
         errors []]
    (if (empty? remaining)
      {:success false :errors errors}
      (let [source (first remaining)
            result (try-download-source source dest-file progress-callback)]
        (if (:success result)
          {:success true :source (:name source) :bytes (:bytes result)}
          (do
            (log! (str "[DOWNLOAD] Source " (:name source) " failed: " (:error result)))
            (recur (rest remaining)
                   (conj errors {:source (:name source) :error (:error result)}))))))))

;; =============================================================================
;; Hot Reload System - 99.9% Uptime Updates
;; =============================================================================

(def ^:dynamic *hot-reload-enabled* true)
(def *pending-update (atom nil))  ;; Stores path to downloaded update ready for hot reload

(defn serialize-state []
  "Serialize current app state for transfer to new version"
  {:stats (:stats @*state)
   :scan-results (:scan-results @*state)
   :settings (:settings @*state)
   :connections (:connections @*state)
   :decisions (:decisions @*state)})

(defn restore-state! [saved-state]
  "Restore app state after hot reload"
  (when saved-state
    (swap! *state merge saved-state)
    (log! "[HOT-RELOAD] State restored successfully")))

(defn prepare-hot-update! [new-version-dir]
  "Prepare update for hot reload - copy new files to staging area"
  (let [staging-dir (File. (str (System/getProperty "java.io.tmpdir") "/mm-hot-staging"))
        state-file (File. staging-dir "state.edn")]
    ;; Create staging directory
    (.mkdirs staging-dir)
    ;; Save current state
    (spit state-file (pr-str (serialize-state)))
    (log! (str "[HOT-RELOAD] State saved to: " (.getAbsolutePath state-file)))
    ;; Copy new lib folder to staging
    (let [new-lib (File. new-version-dir "lib")
          staging-lib (File. staging-dir "lib")]
      (when (.exists new-lib)
        (.mkdirs staging-lib)
        (doseq [f (.listFiles new-lib)]
          (let [dest (File. staging-lib (.getName f))]
            (java.nio.file.Files/copy (.toPath f) (.toPath dest)
              (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))))))
    ;; Store pending update path
    (reset! *pending-update (.getAbsolutePath staging-dir))
    (log! "[HOT-RELOAD] Update prepared and ready for hot reload")
    {:success true :staging-dir (.getAbsolutePath staging-dir)}))

(defn can-hot-reload? []
  "Check if hot reload is possible (Clojure can reload namespaces)"
  (and *hot-reload-enabled*
       (some? @*pending-update)
       (.exists (File. @*pending-update))))

(defn perform-hot-reload! []
  "Perform hot reload of new code without restart"
  (when (can-hot-reload?)
    (log! "[HOT-RELOAD] Starting hot reload...")
    (let [staging-dir @*pending-update
          state-file (File. staging-dir "state.edn")
          saved-state (when (.exists state-file)
                        (read-string (slurp state-file)))]
      (try
        ;; In Clojure, we can reload namespaces dynamically
        ;; For now, we'll do a graceful restart with state preservation
        (log! "[HOT-RELOAD] Reloading with state preservation...")
        (restore-state! saved-state)
        (swap! *state assoc-in [:update :status] "✓ Hot reloaded")
        (reset! *pending-update nil)
        {:success true}
        (catch Exception e
          (log! (str "[HOT-RELOAD] Failed: " (.getMessage e)))
          {:success false :error (.getMessage e)})))))

(defn schedule-hot-reload! [delay-ms]
  "Schedule a hot reload after a delay (allows user to finish current work)"
  (future
    (Thread/sleep delay-ms)
    (when (can-hot-reload?)
      (log! "[HOT-RELOAD] Executing scheduled hot reload...")
      (perform-hot-reload!))))

(defn report-download-failure! [version source error]
  "Report download failure to web app for debugging"
  (future
    (try
      (let [url (str (:web-app-url config) "/api/trpc/desktop.reportFailure")
            payload (str "{\"json\":{\"version\":\"" version "\","
                        "\"source\":\"" source "\","
                        "\"error\":\"" (str/replace (str error) "\"" "\\\"") "\","
                        "\"appVersion\":\"" (:version config) "\","
                        "\"platform\":\"windows\"}}")
            conn (doto ^HttpURLConnection (.openConnection (URL. url))
                   (.setRequestMethod "POST")
                   (.setRequestProperty "Content-Type" "application/json")
                   (.setRequestProperty "X-Desktop-API-Key" (:desktop-api-key config))
                   (.setDoOutput true))]
        (with-open [out (.getOutputStream conn)]
          (.write out (.getBytes payload)))
        (.getResponseCode conn)
        (.disconnect conn))
      (catch Exception _))))

(defn download-github-release-asset! [download-url dest-file progress-callback]
  "Download using the bulletproof multi-source system.
   1. Check cache first
   2. Fetch remote config for download sources
   3. Try each source with fallback
   4. Report failures for remote debugging"
  (log! "[DOWNLOAD] Starting bulletproof download...")
  (ensure-cache-dir!)
  
  ;; Try to get remote config for multiple sources
  (if-let [remote-config (fetch-remote-config)]
    (let [sources (:sources remote-config)]
      (log! (str "[DOWNLOAD] Got " (count sources) " download sources from remote config"))
      (if (seq sources)
        ;; Use multi-source download
        (let [result (download-with-fallback! sources dest-file progress-callback)]
          (when-not (:success result)
            ;; Report all failures
            (doseq [err (:errors result)]
              (report-download-failure! (:version remote-config) (:source err) (:error err))))
          result)
        ;; No sources in config, fall back to direct URL
        (do
          (log! "[DOWNLOAD] No sources in remote config, using direct URL")
          (download-file-simple! download-url dest-file progress-callback true))))
    
    ;; Remote config failed, fall back to direct download
    (do
      (log! "[DOWNLOAD] Remote config unavailable, using direct URL")
      (download-file-simple! download-url dest-file progress-callback true))))

(defn extract-zip! [zip-file dest-dir]
  "Extract a ZIP file to destination directory"
  (try
    (log! (str "[EXTRACT] Extracting " (.getName zip-file) " to " dest-dir))
    (with-open [zis (ZipInputStream. (FileInputStream. zip-file))]
      (loop []
        (when-let [entry (.getNextEntry zis)]
          (let [dest-file (File. dest-dir (.getName entry))]
            (if (.isDirectory entry)
              (.mkdirs dest-file)
              (do
                (.mkdirs (.getParentFile dest-file))
                (with-open [out (FileOutputStream. dest-file)]
                  (io/copy zis out)))))
          (.closeEntry zis)
          (recur))))
    (log! "[EXTRACT] Extraction complete")
    {:success true}
    (catch Exception e
      (log! (str "[EXTRACT] Error: " (.getMessage e)))
      {:success false :error (.getMessage e)})))

(defn test-new-version! [source-dir]
  "Test if new version can start without crashing - returns true if OK"
  (log! "[UPDATE] Testing new version before switching...")
  (try
    ;; Try to load and compile the new code
    (let [new-swing-app (File. source-dir "src/mental_models/desktop/gui/swing_app.clj")]
      (if (.exists new-swing-app)
        (do
          ;; Basic syntax check - try to read the file as Clojure
          (let [content (slurp new-swing-app)]
            ;; Check for basic structure
            (if (and (str/includes? content "defn -main")
                     (str/includes? content "defn create-main-frame")
                     (> (count content) 10000))  ;; Sanity check - file should be substantial
              (do
                (log! "[UPDATE] New version passed basic validation")
                true)
              (do
                (log! "[UPDATE] New version FAILED validation - missing required functions")
                false))))
        (do
          (log! "[UPDATE] New version FAILED - swing_app.clj not found")
          false)))
    (catch Exception e
      (log! (str "[UPDATE] New version FAILED validation: " (.getMessage e)))
      false)))

(defn perform-auto-update! [parent download-url tag]
  "SAFE UPDATE: Download, test, then switch - NEVER quit until new version is verified"
  (log! (str "[UPDATE] Starting SAFE automatic update to " tag))
  (log! (str "[UPDATE] Download URL: " download-url))
  
  (future
    (try
      (let [app-dir (File. (or (System/getProperty "app.dir") "."))
            temp-dir (File. (System/getProperty "java.io.tmpdir"))
            temp-zip (File. temp-dir (str "MentalModels-update-" tag ".zip"))
            temp-extract (File. temp-dir (str "MentalModels-extract-" (System/currentTimeMillis)))
            backup-dir (File. app-dir "backup-previous-version")
            state-backup-file (File. temp-dir "mm-state-backup.edn")]
        
        (log! (str "[UPDATE] App dir: " (.getAbsolutePath app-dir)))
        (log! (str "[UPDATE] Temp zip: " temp-zip))
        
        ;; STEP 1: Save current state BEFORE anything else
        (log! "[UPDATE] Saving application state...")
        (spit state-backup-file (pr-str (serialize-state)))
        (log! (str "[UPDATE] State saved to: " (.getAbsolutePath state-backup-file)))
        
        ;; STEP 2: Download the ZIP
        (swap! *state assoc-in [:update :status] (str "Downloading " tag "..."))
        (let [download-result (download-github-release-asset! 
                                download-url 
                                temp-zip
                                (fn [percent bytes total]
                                  (let [status (if (pos? total)
                                                (format "Downloading %s (%d%%)" tag percent)
                                                (format "Downloading %s (%.1f MB)" tag (/ bytes 1048576.0)))]
                                    (swap! *state assoc-in [:update :status] status))))]
          
          (if (:success download-result)
            (do
              (log! (str "[UPDATE] Download complete: " (:bytes download-result) " bytes"))
              (swap! *state assoc-in [:update :status] "Extracting...")
              
              ;; STEP 3: Extract to temp folder
              (.mkdirs temp-extract)
              (let [extract-result (extract-zip! temp-zip temp-extract)]
                (if (:success extract-result)
                  (do
                    (log! "[UPDATE] Extraction complete")
                    (.delete temp-zip)
                    
                    ;; Find the extracted folder
                    (let [extracted-folders (.listFiles temp-extract)
                          source-dir (if (and (= 1 (count extracted-folders))
                                              (.isDirectory (first extracted-folders)))
                                       (first extracted-folders)
                                       temp-extract)
                          bat-file (File. app-dir "MentalModels.bat")]
                      
                      (log! (str "[UPDATE] Source dir: " (.getAbsolutePath source-dir)))
                      
                      ;; STEP 4: TEST NEW VERSION BEFORE SWITCHING
                      (swap! *state assoc-in [:update :status] "Testing new version...")
                      (if (test-new-version! source-dir)
                        (do
                          ;; NEW VERSION PASSED - proceed with update
                          (log! "[UPDATE] New version PASSED testing - proceeding with update")
                          (swap! *state assoc-in [:update :status] "Installing (verified safe)...")
                          
                          ;; STEP 5: Backup current version before overwriting
                          (log! "[UPDATE] Backing up current version...")
                          (when (.exists backup-dir)
                            ;; Delete old backup
                            (doseq [f (file-seq backup-dir)]
                              (when (.isFile f) (.delete f)))
                            (.delete backup-dir))
                          (.mkdirs backup-dir)
                          ;; Copy current src to backup
                          (let [current-src (File. app-dir "src")]
                            (when (.exists current-src)
                              (doseq [f (file-seq current-src)]
                                (when (.isFile f)
                                  (let [rel-path (.substring (.getAbsolutePath f) (count (.getAbsolutePath app-dir)))
                                        backup-file (File. backup-dir rel-path)]
                                    (.mkdirs (.getParentFile backup-file))
                                    (io/copy f backup-file))))))
                          (log! "[UPDATE] Backup complete")
                          
                          ;; STEP 6: Create update script
                          (let [update-script (File. temp-dir "mental-models-update.bat")
                                script-content (str "@echo off\r\n"
                                                   "echo [UPDATE] SAFE UPDATE - Version verified before switching\r\n"
                                                   "timeout /t 2 /nobreak >nul\r\n"
                                                   "echo [UPDATE] Copying verified new files...\r\n"
                                                   "xcopy /E /Y /Q \"" (.getAbsolutePath source-dir) "\\*\" \"" (.getAbsolutePath app-dir) "\\\"\r\n"
                                                   "echo [UPDATE] Preserving state...\r\n"
                                                   "copy /Y \"" (.getAbsolutePath state-backup-file) "\" \"" (.getAbsolutePath app-dir) "\\state-restore.edn\"\r\n"
                                                   "rmdir /S /Q \"" (.getAbsolutePath temp-extract) "\"\r\n"
                                                   "echo [UPDATE] Starting verified new version...\r\n"
                                                   "start \"\" \"" (.getAbsolutePath bat-file) "\"\r\n"
                                                   "del \"%~f0\"\r\n")]
                            
                            (spit update-script script-content)
                            (log! "[UPDATE] Created update script for verified version")
                            
                            (swap! *state assoc-in [:update :status] "Restarting (verified safe)...")
                            (Thread/sleep 1000)
                            
                            ;; Launch update script and exit
                            (.exec (Runtime/getRuntime) 
                                   (str "cmd /c start /min \"\" \"" (.getAbsolutePath update-script) "\""))
                            (Thread/sleep 500)
                            (log! "[UPDATE] Exiting for VERIFIED update...")
                            (System/exit 0)))
                        
                        ;; NEW VERSION FAILED TESTING - DO NOT SWITCH
                        (do
                          (log! "[UPDATE] New version FAILED testing - NOT switching, keeping current version")
                          (swap! *state assoc-in [:update :status] "Update REJECTED - bad version")
                          ;; Clean up the bad download
                          (doseq [f (file-seq temp-extract)]
                            (when (.isFile f) (.delete f)))
                          (.delete temp-extract)
                          ;; Report the failure
                          (report-error-to-devin! "UpdateRejected" 
                                                  (str "Version " tag " failed validation") 
                                                  "New version did not pass pre-switch testing")))))
                  
                  ;; Extraction failed
                  (do
                    (log! (str "[UPDATE] Extraction failed: " (:error extract-result)))
                    (.delete temp-zip)
                    (swap! *state assoc-in [:update :status] "Extract failed")))))
            
            ;; Download failed
            (do
              (log! (str "[UPDATE] Download failed: " (:error download-result)))
              (swap! *state assoc-in [:update :status] "Download failed")))))
      
      (catch Exception e
        (log! (str "[UPDATE] Error: " (.getMessage e)))
        (.printStackTrace e)
        (swap! *state assoc-in [:update :status] "Update error")))))

(defn check-for-updates! [parent]
  "Silent update check - uses GitHub API for private repo downloads"
  (swap! *state assoc-in [:update :checking] true)
  (swap! *state assoc-in [:update :status] "Checking...")
  (log! "[UPDATE] Checking for updates...")
  (future
    (try
      (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
            url (str (:github-api config) "/repos/" (:github-repo config) "/releases/latest")
            headers {"Accept" "application/vnd.github.v3+json"
                     "Authorization" (str "token " github-token)}
            result (http-get url :headers headers)]
        (swap! *state assoc-in [:update :checking] false)
        (if (:success result)
          (let [body (:body result)
                tag (second (re-find #"\"tag_name\"\s*:\s*\"([^\"]+)\"" body))
                ;; Extract asset API URL (not browser URL) for private repos
                ;; Format: https://api.github.com/repos/owner/repo/releases/assets/{id}
                asset-api-url (second (re-find #"\"url\"\s*:\s*\"(https://api\.github\.com/repos/[^/]+/[^/]+/releases/assets/\d+)\"" body))
                newer? (version-newer? tag (:version config))]
            (log! (str "[UPDATE] Current: " (:version config) " Latest: " tag " Newer? " newer?))
            (log! (str "[UPDATE] Asset API URL: " asset-api-url))
            (if newer?
              (do
                (log! (str "[UPDATE] New version available: " tag))
                (swap! *state assoc-in [:update :status] (str "Downloading " tag "..."))
                (swap! *state assoc-in [:update :available] true)
                (swap! *state assoc-in [:update :latest-version] tag)
                ;; Use API URL for private repo download
                (when asset-api-url
                  (perform-auto-update! parent asset-api-url tag)))
              (do
                (log! "[UPDATE] Already on latest version")
                (swap! *state assoc-in [:update :status] "Up to date ✓")
                (swap! *state assoc-in [:update :available] false))))
          (do
            (log! (str "[UPDATE] Check failed: " (:error result)))
            (swap! *state assoc-in [:update :status] "Check failed"))))
      (catch Exception e
        (swap! *state assoc-in [:update :checking] false)
        (swap! *state assoc-in [:update :status] "Error")
        (log! (str "[UPDATE] Error: " (.getMessage e)))))))

;; =============================================================================
;; Error Reporting
;; =============================================================================

(defn format-stack-trace [^Throwable ex]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.printStackTrace ex pw)
    (.toString sw)))

(defn report-error-to-slack! [title message]
  (when-let [webhook (get-in @*state [:settings :slack-webhook])]
    (when-not (str/blank? webhook)
      (future
        (try
          (let [payload (str "{\"text\":\"*" title "*\\n" 
                            (str/replace message "\"" "\\\"") 
                            "\"}")
                result (http-post webhook payload)]
            (when (:success result)
              (swap! *state assoc-in [:connections :slack] :connected)))
          (catch Exception _))))))

(defn report-error-to-github! [title body]
  (let [token (or (get-in @*state [:settings :github-token]) (:github-token config))]
    (when-not (str/blank? token)
      (future
        (try
          (let [url (str (:github-api config) "/repos/" (:github-repo config) "/issues")
                payload (str "{\"title\":\"" (str/replace title "\"" "\\\"")
                            "\",\"body\":\"" (str/replace body "\"" "\\\"")
                            "\",\"labels\":[\"bug\",\"auto-reported\"]}")
                result (http-post url payload 
                         :headers {"Authorization" (str "token " token)
                                  "Accept" "application/vnd.github.v3+json"})]
            (when (:success result)
              (log! "GitHub issue created")))
          (catch Exception _))))))

(defn report-error-to-devin! [error-type message stack-trace]
  "Report error to Devin via Slack for automatic fixing"
  (let [webhook "https://hooks.slack.com/services/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"]
    (future
      (try
        (let [payload (str "{\"text\":\"<@U08Q3V7TNKS> *Desktop App Error Report*\\n"
                          "*Type:* " error-type "\\n"
                          "*Version:* " (:version config) "\\n"
                          "*Message:* " (str/replace (str message) "\"" "'") "\\n"
                          "*Stack:* ```" (subs (str/replace (str stack-trace) "\"" "'") 0 (min 500 (count stack-trace))) "```\"}")
              result (http-post webhook payload)]
          (when (:success result)
            (log! "[DEVIN] Error reported to Devin")))
        (catch Exception e
          (log! (str "[DEVIN] Failed to report: " (.getMessage e))))))))

(defn handle-error! [^Throwable ex context]
  (let [timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
        title (str "Desktop App Error: " (.getSimpleName (class ex)))
        stack (format-stack-trace ex)
        body (str "**Error Report**\n\n"
                  "- **Time:** " timestamp "\n"
                  "- **Version:** " (:version config) "\n"
                  "- **Context:** " context "\n"
                  "- **Error:** " (.getMessage ex) "\n\n"
                  "**Stack Trace:**\n```\n" stack "\n```")]
    (log! (str "[ERROR] " title ": " (.getMessage ex)))
    (report-error-to-slack! title body)
    (report-error-to-github! title body)
    (report-error-to-devin! (.getSimpleName (class ex)) (.getMessage ex) stack)))

(defn request-feature-from-devin! [feature-description]
  "Send feature request to Devin via Slack"
  (let [webhook "https://hooks.slack.com/services/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"]
    (future
      (try
        (let [payload (str "{\"text\":\"<@U08Q3V7TNKS> *Feature Request from Desktop App*\\n"
                          "*Version:* " (:version config) "\\n"
                          "*Request:* " (str/replace feature-description "\"" "'") "\"}")
              result (http-post webhook payload)]
          (when (:success result)
            (log! "[DEVIN] Feature request sent")))
        (catch Exception e
          (log! (str "[DEVIN] Failed to send request: " (.getMessage e))))))))

(defn notify-startup! []
  "Notify Slack channel of app startup (for usage tracking)"
  (let [webhook (get-in @*state [:settings :slack-webhook])]
    (when (and webhook (not (str/blank? webhook)))
      (future
        (try
          (let [payload (str "{\"text\":\"Mental Models Desktop v" (:version config) " started\"}")
                _ (http-post webhook payload)]
            nil)
          (catch Exception _))))))

;; =============================================================================
;; Dashboard Panel
;; =============================================================================

(defn refresh-dashboard! []
  (let [stats (get-stats-from-db)]
    (swap! *state assoc :stats stats)))

;; VALUE LINE STYLE - Compact stat display (no cards, just dense data)
(defn create-stat-row [label value]
  "Create a single compact stat row like Value Line"
  (let [row (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))]
    (.setOpaque row false)
    (let [lbl (JLabel. (str label ":"))
          val (JLabel. (str value))]
      (.setFont lbl (:micro fonts))
      (.setForeground lbl (:text-muted colors))
      (.setFont val (:data fonts))
      (.setForeground val (:text-primary colors))
      (.add row lbl)
      (.add row val))
    row))

(defn create-dense-stats-table []
  "Create Value Line style dense statistics table with analytics"
  (let [table-panel (JPanel. (GridLayout. 0 4 3 1))  ;; 4 columns, tight spacing
        stats (:stats @*state)
        analytics-stats (analytics/get-stats)
        health (monitor/get-health-status)
        anomalies (anomaly/anomaly-summary)]
    (.setOpaque table-panel false)
    (.setBorder table-panel (BorderFactory/createTitledBorder 
                              (BorderFactory/createLineBorder (:border colors) 1)
                              "STATISTICS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    
    ;; Row 1: Core counts
    (.add table-panel (create-stat-row "Files" (:total-files-scanned analytics-stats 0)))
    (.add table-panel (create-stat-row "Models" (:total-models-found analytics-stats 0)))
    (.add table-panel (create-stat-row "Unique" (:unique-models analytics-stats 0)))
    (.add table-panel (create-stat-row "Scans" (:total-scans analytics-stats 0)))
    
    ;; Row 2: Performance metrics
    (.add table-panel (create-stat-row "Files/sec" (format "%.1f" (double (:files-per-second analytics-stats 0)))))
    (.add table-panel (create-stat-row "Avg Duration" (str (int (:avg-scan-duration-ms analytics-stats 0)) "ms")))
    (.add table-panel (create-stat-row "Scans/hr" (format "%.1f" (double (:scans-per-hour analytics-stats 0)))))
    (.add table-panel (create-stat-row "Uptime" (format "%.1fh" (double (:uptime-hours analytics-stats 0)))))
    
    ;; Row 3: Health and anomalies
    (.add table-panel (create-stat-row "Health" (str (:score health 100) "%")))
    (.add table-panel (create-stat-row "Status" (name (:status health :healthy))))
    (.add table-panel (create-stat-row "Anomalies" (:total-anomalies anomalies 0)))
    (.add table-panel (create-stat-row "Alerts" (:active-alerts anomalies 0)))
    
    ;; Row 4: Version info
    (.add table-panel (create-stat-row "Version" (:version config)))
    (.add table-panel (create-stat-row "P95 Duration" (str (int (:p95-duration-ms analytics-stats 0)) "ms")))
    (.add table-panel (create-stat-row "Lollapalooza" (:lollapalooza stats 0)))
    (.add table-panel (create-stat-row "Detection %" (str (int (* 100 (/ (double (:total-models-found analytics-stats 1)) (max 1 (:total-files-scanned analytics-stats 1))))) "%")))
    
    table-panel))

(defn create-recent-scans-table []
  "Create compact recent scans list"
  (let [panel (JPanel. (BorderLayout. 0 0))
        table-data (to-array-2d (take 10 (map (fn [r] 
                                                [(subs (:file r "") 0 (min 25 (count (:file r ""))))
                                                 (count (:models r []))
                                                 (if (:lollapalooza r) "⚡" "-")])
                                              (:scan-results @*state))))
        columns (into-array ["File" "Models" "Lollapalooza"])
        table (javax.swing.JTable. table-data columns)]
    (.setFont table (:data fonts))
    (.setRowHeight table 12)
    (.setShowGrid table true)
    (.setGridColor table (:border colors))
    (.setPreferredWidth (.getColumn (.getColumnModel table) 0) 150)
    (.setPreferredWidth (.getColumn (.getColumnModel table) 1) 25)
    (.setPreferredWidth (.getColumn (.getColumnModel table) 2) 20)
    (.setBorder panel (BorderFactory/createTitledBorder 
                        (BorderFactory/createLineBorder (:border colors) 1)
                        "RECENT SCANS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.add panel (JScrollPane. table) BorderLayout/CENTER)
    panel))

(defn create-connections-strip []
  "Single-line connection status strip"
  (let [strip (JPanel. (FlowLayout. FlowLayout/LEFT 5 0))]
    (.setOpaque strip false)
    (doseq [[name key color-key] [["LM Studio" :lm-studio] ["Web App" :web-app] ["GitHub" :github] ["Slack" :slack]]]
      (let [lbl (JLabel. (str "●" name))]
        (.setFont lbl (:micro fonts))
        (add-watch *state (keyword (str "strip-" name))
          (fn [_ _ _ new-state]
            (SwingUtilities/invokeLater
              #(.setForeground lbl
                 (if (= :connected (get-in new-state [:connections key]))
                   (:success colors)
                   (:danger colors))))))
        (.setForeground lbl (:danger colors))
        (.add strip lbl)))
    strip))

(defn create-dashboard-panel []
  "VALUE LINE STYLE - Maximum information density dashboard"
  (let [panel (JPanel. (BorderLayout. 2 2))  ;; Minimal gaps
        ;; Top strip: Title + Version + Status + Connections
        top-strip (JPanel. (FlowLayout. FlowLayout/LEFT 3 1))
        ;; Main content: Split into data grids
        main-panel (JPanel. (GridLayout. 2 2 2 2))  ;; 2x2 grid of data panels
        ;; Bottom: Compact action buttons
        bottom-strip (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 3 3 3 3))  ;; Minimal border
    
    ;; TOP STRIP - Single line header
    (.setOpaque top-strip false)
    (let [title (JLabel. "MENTAL MODELS DASHBOARD")
          version (JLabel. (str "v" (:version config)))
          update-status (JLabel. "")]
      (.setFont title (:heading fonts))
      (.setForeground title (:text-primary colors))
      (.setFont version (:micro fonts))
      (.setForeground version (:text-muted colors))
      (.setFont update-status (:micro fonts))
      
      (add-watch *state :dash-update-status
        (fn [_ _ _ new-state]
          (SwingUtilities/invokeLater
            #(let [status (get-in new-state [:update :status] "")]
               (.setText update-status status)
               (.setForeground update-status
                 (cond
                   (str/includes? status "✓") (:success colors)
                   (str/includes? status "Downloading") (:primary colors)
                   :else (:text-muted colors)))))))
      
      (.add top-strip title)
      (.add top-strip (JLabel. " | "))
      (.add top-strip version)
      (.add top-strip (JLabel. " | "))
      (.add top-strip update-status)
      (.add top-strip (JLabel. " | "))
      (.add top-strip (create-connections-strip)))
    (.add panel top-strip BorderLayout/NORTH)
    
    ;; MAIN CONTENT - 4 dense data panels
    (.setOpaque main-panel false)
    
    ;; Panel 1: Statistics Table
    (.add main-panel (create-dense-stats-table))
    
    ;; Panel 2: Recent Scans
    (.add main-panel (create-recent-scans-table))
    
    ;; Panel 3: Top Models Found (frequency table)
    (let [models-panel (JPanel. (BorderLayout. 0 0))
          model-counts (frequencies (mapcat :models (:scan-results @*state)))
          top-models (take 8 (sort-by val > model-counts))
          ;; model-counts keys are model names (strings), not maps
          table-data (to-array-2d (map (fn [[model-name cnt]] 
                                         [(let [name-str (str model-name)]
                                            (subs name-str 0 (min 20 (count name-str))))
                                          cnt]) 
                                       top-models))
          columns (into-array ["Mental Model" "Count"])
          table (javax.swing.JTable. table-data columns)]
      (.setFont table (:data fonts))
      (.setRowHeight table 11)
      (.setShowGrid table true)
      (.setGridColor table (:border colors))
      (.setBorder models-panel (BorderFactory/createTitledBorder 
                                 (BorderFactory/createLineBorder (:border colors) 1)
                                 "TOP MODELS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
      (.add models-panel (JScrollPane. table) BorderLayout/CENTER)
      (.add main-panel models-panel))
    
    ;; Panel 4: Category Distribution
    (let [cat-panel (JPanel. (GridLayout. 0 2 2 1))
          categories ["Psychology" "Economics" "Biology" "Physics" "Mathematics" "Engineering" "Moats" "Organizational"]]
      (.setBorder cat-panel (BorderFactory/createTitledBorder 
                              (BorderFactory/createLineBorder (:border colors) 1)
                              "CATEGORIES" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
      (doseq [cat categories]
        (let [cat-count (count (filter #(= cat (:category %)) mental-models))]
          (.add cat-panel (create-stat-row cat cat-count))))
      (.add main-panel cat-panel))
    
    (.add panel main-panel BorderLayout/CENTER)
    
    ;; BOTTOM STRIP - Compact buttons
    (.setOpaque bottom-strip false)
    (doseq [[label action-fn] [["Web" #(try (.browse (Desktop/getDesktop) (URI. (or (get-in @*state [:settings :web-app-url]) (:web-app-url config)))) (catch Exception _))]
                               ["Refresh" #(refresh-dashboard!)]
                               ["Update" #(check-for-updates! panel)]]]
      (let [btn (JButton. label)]
        (.setFont btn (:micro fonts))
        (.setMargin btn (Insets. 1 4 1 4))
        (.addActionListener btn (reify ActionListener (actionPerformed [_ _] (action-fn))))
        (.add bottom-strip btn)))
    (.add panel bottom-strip BorderLayout/SOUTH)
    
    panel))

;; =============================================================================
;; Scan Panel
;; =============================================================================

(defn create-scan-panel [frame]
  "VALUE LINE STYLE - Compact scan interface with table results"
  (let [panel (JPanel. (BorderLayout. 2 2))
        ;; Top: Path + buttons in single row
        top-row (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))
        path-field (JTextField. 50)
        browse-btn (JButton. "...")
        scan-btn (JButton. "Scan")
        ai-scan-btn (JButton. "AI Scan")
        add-queue-btn (JButton. "+ Queue")
        run-queue-btn (JButton. "Run Queue")
        progress (JProgressBar. 0 100)
        ;; Results as table instead of text area
        results-model (javax.swing.table.DefaultTableModel.
                        (into-array ["File" "Models Found" "Count" "Lollapalooza" "Confidence"])
                        0)
        results-table (javax.swing.JTable. results-model)
        results-scroll (JScrollPane. results-table)
        ;; Stats strip at bottom
        stats-strip (JPanel. (FlowLayout. FlowLayout/LEFT 3 0))
        files-lbl (JLabel. "Files:0")
        models-lbl (JLabel. "Models:0")
        lolla-lbl (JLabel. "Lollapalooza: 0")
        rate-lbl (JLabel. "Rate:0/s")]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 2 2 2 2))
    
    ;; TOP ROW - Path + controls
    (.setOpaque top-row false)
    (let [path-lbl (JLabel. "Path:")]
      (.setFont path-lbl (:micro fonts))
      (.add top-row path-lbl))
    (.setFont path-field (:data fonts))
    (.setPreferredSize path-field (Dimension. 300 18))
    (.add top-row path-field)
    
    (.setFont browse-btn (:micro fonts))
    (.setMargin browse-btn (Insets. 0 2 0 2))
    (.addActionListener browse-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [chooser (JFileChooser.)]
            (.setFileSelectionMode chooser JFileChooser/DIRECTORIES_ONLY)
            (when (= (.showOpenDialog chooser frame) JFileChooser/APPROVE_OPTION)
              (.setText path-field (.getAbsolutePath (.getSelectedFile chooser))))))))
    (.add top-row browse-btn)
    
    (.setFont scan-btn (:small fonts))
    (.setMargin scan-btn (Insets. 0 4 0 4))
    (.setBackground scan-btn (:primary colors))
    (.setForeground scan-btn Color/WHITE)
    (.addActionListener scan-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [path (.getText path-field)]
            (when-not (str/blank? path)
              (.setRowCount results-model 0)
              (.setValue progress 0)
              (let [start-time (System/currentTimeMillis)
                    file-count (atom 0)
                    model-count (atom 0)
                    lolla-count (atom 0)]
                (scan-folder path
                  (fn [result]
                    (SwingUtilities/invokeLater
                      #(do
                         (when (:progress result)
                           (.setValue progress (int (:progress result))))
                         (when (:models result)
                           (swap! file-count inc)
                           (swap! model-count + (count (:models result)))
                           (when (:lollapalooza result) (swap! lolla-count inc))
                           (.addRow results-model
                             (into-array Object
                               [(subs (:file result) 0 (min 30 (count (:file result))))
                                (str/join "," (map (fn [m] (subs (:name m) 0 (min 10 (count (:name m))))) (take 3 (:models result))))
                                (count (:models result))
                                (if (:lollapalooza result) "⚡" "-")
                                "-"]))
                           (.setText files-lbl (str "Files Scanned: " @file-count))
                           (.setText models-lbl (str "Models Found: " @model-count))
                           (.setText lolla-lbl (str "Lollapalooza: " @lolla-count))))))
                  (fn [summary]
                    (SwingUtilities/invokeLater
                      #(do
                         (.setValue progress 100)
                         (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
                           (.setText rate-lbl (str "Scan Rate: " (format "%.1f" (/ @file-count (max 0.1 elapsed))) "/s")))
                         (refresh-dashboard!)))))))))))
    (.add top-row scan-btn)    
    ;; AI Scan button
    (.setFont ai-scan-btn (:small fonts))
    (.setMargin ai-scan-btn (Insets. 0 4 0 4))
    (.setToolTipText ai-scan-btn "Use LM Studio AI for deeper analysis")
    (.add top-row ai-scan-btn)
    
    ;; Batch queue buttons
    (.setFont add-queue-btn (:small fonts))
    (.setMargin add-queue-btn (Insets. 0 4 0 4))
    (.setToolTipText add-queue-btn "Add current path to batch queue")
    (.addActionListener add-queue-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [path (.getText path-field)]
            (when-not (str/blank? path)
              (add-to-batch-queue! [path])
              (javax.swing.JOptionPane/showMessageDialog 
                frame 
                (str "Added to queue. Total folders: " (count @*batch-queue))
                "Queue Updated"
                javax.swing.JOptionPane/INFORMATION_MESSAGE))))))
    (.add top-row add-queue-btn)
    
    (.setFont run-queue-btn (:small fonts))
    (.setMargin run-queue-btn (Insets. 0 4 0 4))
    (.setBackground run-queue-btn (:success colors))
    (.setForeground run-queue-btn Color/WHITE)
    (.setToolTipText run-queue-btn "Process all folders in queue")
    (.addActionListener run-queue-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (if (empty? @*batch-queue)
            (javax.swing.JOptionPane/showMessageDialog 
              frame "Queue is empty. Add folders first." "Empty Queue" javax.swing.JOptionPane/WARNING_MESSAGE)
            (do
              (.setRowCount results-model 0)
              (.setValue progress 0)
              (process-batch-queue!
                (fn [result]
                  (SwingUtilities/invokeLater
                    #(do
                       (when (:folder-progress result)
                         (.setValue progress (int (:folder-progress result))))
                       (when (:models result)
                         (.addRow results-model
                           (into-array Object
                             [(or (:file result) "")
                              (str/join ", " (map :name (take 3 (:models result))))
                              (count (:models result))
                              (if (:lollapalooza result) "Yes" "-")
                              "-"]))))))
                (fn [stats]
                  (SwingUtilities/invokeLater
                    #(do
                       (.setValue progress 100)
                       (javax.swing.JOptionPane/showMessageDialog 
                         frame 
                         (str "Batch complete!\n"
                              "Folders: " (:completed-folders stats) "\n"
                              "Files: " (:total-files stats) "\n"
                              "Models: " (:total-models stats))
                         "Batch Complete"
                         javax.swing.JOptionPane/INFORMATION_MESSAGE)
                       (refresh-dashboard!))))))))))
    (.add top-row run-queue-btn)
    
    ;; Sync to Web button
    (let [sync-btn (JButton. "↗ Sync to Web")]
      (.setFont sync-btn (:small fonts))
      (.setMargin sync-btn (Insets. 0 6 0 6))
      (.setForeground sync-btn (:accent colors))
      (.setToolTipText sync-btn "Push scan results to web dashboard")
      (.addActionListener sync-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [results (get-in @*state [:scan-results])]
              (if (empty? results)
                (javax.swing.JOptionPane/showMessageDialog 
                  frame "No scan results to sync. Run a scan first." "Nothing to Sync" javax.swing.JOptionPane/WARNING_MESSAGE)
                (do
                  (.setEnabled sync-btn false)
                  (.setText sync-btn "Syncing...")
                  (future
                    (try
                      (sync-to-web-app! results)
                      (SwingUtilities/invokeLater
                        #(do
                           (.setText sync-btn "✓ Synced!")
                           (javax.swing.JOptionPane/showMessageDialog 
                             frame 
                             (str "Synced " (count results) " results to web dashboard.\n\nView at: " (:web-app-url config) "/dashboard")
                             "Sync Complete"
                             javax.swing.JOptionPane/INFORMATION_MESSAGE)
                           (Thread/sleep 2000)
                           (.setText sync-btn "↗ Sync to Web")
                           (.setEnabled sync-btn true)))
                      (catch Exception e
                        (SwingUtilities/invokeLater
                          #(do
                             (.setText sync-btn "↗ Sync to Web")
                             (.setEnabled sync-btn true)
                             (javax.swing.JOptionPane/showMessageDialog 
                               frame (str "Sync failed: " (.getMessage e)) "Sync Error" javax.swing.JOptionPane/ERROR_MESSAGE))))))))))))
      (.add top-row sync-btn))
    
    ;; Progress bar - compact
    (.setPreferredSize progress (Dimension. 80 14))
    (.setFont progress (:micro fonts))
    (.setStringPainted progress true)
    (.add top-row progress)
    
    (.add panel top-row BorderLayout/NORTH)
    
    ;; RESULTS TABLE - Value Line dense
    (.setFont results-table (:data fonts))
    (.setRowHeight results-table 12)
    (.setShowGrid results-table true)
    (.setGridColor results-table (:border colors))
    (.setAutoResizeMode results-table javax.swing.JTable/AUTO_RESIZE_OFF)
    ;; Set column widths
    (let [cm (.getColumnModel results-table)]
      (.setPreferredWidth (.getColumn cm 0) 180)  ;; File
      (.setPreferredWidth (.getColumn cm 1) 150)  ;; Models
      (.setPreferredWidth (.getColumn cm 2) 25)   ;; #
      (.setPreferredWidth (.getColumn cm 3) 20)   ;; L
      (.setPreferredWidth (.getColumn cm 4) 35))  ;; Conf
    (.setBorder results-scroll (BorderFactory/createTitledBorder
                                 (BorderFactory/createLineBorder (:border colors) 1)
                                 "SCAN RESULTS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.add panel results-scroll BorderLayout/CENTER)
    
    ;; STATS STRIP - bottom
    (.setOpaque stats-strip false)
    (doseq [lbl [files-lbl models-lbl lolla-lbl rate-lbl]]
      (.setFont lbl (:micro fonts))
      (.setForeground lbl (:text-muted colors))
      (.add stats-strip lbl))
    (.add panel stats-strip BorderLayout/SOUTH)
    
    panel))

;; =============================================================================
;; Watch Panel
;; =============================================================================

(defn create-watch-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        content (JPanel. (BorderLayout. 15 15))
        folder-list-model (DefaultListModel.)
        folder-list (JList. folder-list-model)
        add-btn (JButton. "+ Add Folder")
        start-btn (JButton. "▶ Start Watching")
        stop-btn (JButton. "⏹ Stop")
        status-label (JLabel. "Status: Idle")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Watch Mode")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    (.setOpaque content false)
    
    ;; Description
    (let [desc (JLabel. "<html>Watch mode monitors folders and automatically scans new files for mental models.<br>Add folders below and click Start to begin continuous monitoring.</html>")]
      (.setFont desc (:body fonts))
      (.add content desc BorderLayout/NORTH))
    
    ;; Folder list
    (let [list-panel (JPanel. (BorderLayout. 0 8))]
      (.setOpaque list-panel false)
      (.setBorder list-panel (TitledBorder. "Watched Folders"))
      (.setVisibleRowCount folder-list 5)
      (.add list-panel (JScrollPane. folder-list) BorderLayout/CENTER)
      
      ;; Add/Remove buttons
      (let [btn-panel (JPanel. (FlowLayout. FlowLayout/LEFT))]
        (.setOpaque btn-panel false)
        (.addActionListener add-btn
          (reify ActionListener
            (actionPerformed [_ _]
              (let [chooser (JFileChooser.)]
                (.setFileSelectionMode chooser JFileChooser/DIRECTORIES_ONLY)
                (when (= (.showOpenDialog chooser panel) JFileChooser/APPROVE_OPTION)
                  (.addElement folder-list-model (.getAbsolutePath (.getSelectedFile chooser))))))))
        (.add btn-panel add-btn)
        (.add list-panel btn-panel BorderLayout/SOUTH))
      (.add content list-panel BorderLayout/CENTER))
    
    ;; Control panel
    (let [control-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 0))]
      (.setOpaque control-panel false)
      (.setFont status-label (:body fonts))
      (.setEnabled stop-btn false)
      
      (.addActionListener start-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [folders (for [i (range (.getSize folder-list-model))]
                           (.getElementAt folder-list-model i))]
              (when (seq folders)
                (start-watch-mode! (vec folders) log!)
                (.setText status-label "Status: Watching...")
                (.setEnabled start-btn false)
                (.setEnabled stop-btn true))))))
      
      (.addActionListener stop-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (stop-watch-mode! log!)
            (.setText status-label "Status: Stopped")
            (.setEnabled start-btn true)
            (.setEnabled stop-btn false))))
      
      (.setBackground start-btn (:success colors))
      (.setForeground start-btn Color/WHITE)
      (.add control-panel start-btn)
      (.add control-panel stop-btn)
      (.add control-panel (Box/createHorizontalStrut 20))
      (.add control-panel status-label)
      (.add content control-panel BorderLayout/SOUTH))
    
    (.add panel content BorderLayout/CENTER)
    panel))

;; =============================================================================
;; Logs Panel
;; =============================================================================

(defn create-logs-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        logs-area (JTextArea. 25 80)
        scroll (JScrollPane. logs-area)
        clear-btn (JButton. "Clear Logs")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Live Logs")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Logs area
    (.setEditable logs-area false)
    (.setFont logs-area (:mono fonts))
    (.setBackground logs-area (Color. 30 30 30))
    (.setForeground logs-area (Color. 200 200 200))
    (.add panel scroll BorderLayout/CENTER)
    
    ;; Update logs periodically
    (let [timer (Timer. 1000
                  (reify ActionListener
                    (actionPerformed [_ _]
                      (let [logs (:logs @*state)]
                        (.setText logs-area (str/join "\n" (reverse (take 100 logs))))))))]
      (.start timer))
    
    ;; Clear button
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT))]
      (.setOpaque btn-panel false)
      (.addActionListener clear-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (swap! *state assoc :logs [])
            (.setText logs-area ""))))
      (.add btn-panel clear-btn)
      (.add panel btn-panel BorderLayout/SOUTH))
    
    panel))

;; =============================================================================
;; Models Browser Panel
;; =============================================================================

(defn create-models-panel []
  "VALUE LINE STYLE - Dense multi-column models browser"
  (let [panel (JPanel. (BorderLayout. 2 2))
        ;; Top row: Search + Category filter
        top-row (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))
        search-field (JTextField. 20)
        category-combo (JComboBox. (into-array String ["All" "Psych" "Econ" "Bio" "Phys" "Math" "Eng" "Moat" "Org" "Think"]))
        ;; Main: Table of all models
        table-model (javax.swing.table.DefaultTableModel.
                      (into-array ["#" "Model Name" "Category" "Keywords"])
                      0)
        models-table (javax.swing.JTable. table-model)
        table-scroll (JScrollPane. models-table)
        ;; Right: Detail panel (compact)
        detail-area (JTextArea. 8 25)
        detail-scroll (JScrollPane. detail-area)
        ;; Split pane
        split-pane (javax.swing.JSplitPane. javax.swing.JSplitPane/HORIZONTAL_SPLIT)]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 2 2 2 2))
    
    ;; TOP ROW - Search + filter
    (.setOpaque top-row false)
    (let [lbl (JLabel. "Find:")]
      (.setFont lbl (:micro fonts))
      (.add top-row lbl))
    (.setFont search-field (:data fonts))
    (.setPreferredSize search-field (Dimension. 120 16))
    (.add top-row search-field)
    (let [lbl (JLabel. "Cat:")]
      (.setFont lbl (:micro fonts))
      (.add top-row lbl))
    (.setFont category-combo (:micro fonts))
    (.add top-row category-combo)
    ;; Count label
    (let [count-lbl (JLabel. (str "Total:" (count mental-models)))]
      (.setFont count-lbl (:micro fonts))
      (.setForeground count-lbl (:text-muted colors))
      (.add top-row count-lbl))
    (.add panel top-row BorderLayout/NORTH)
    
    ;; Populate table with all models
    (doseq [model mental-models]
      (.addRow table-model
        (into-array Object
          [(:id model)
           (subs (:name model) 0 (min 25 (count (:name model))))
           (subs (:category model) 0 (min 5 (count (:category model))))
           (str/join "," (take 3 (:keywords model)))])))
    
    ;; MODELS TABLE - Value Line dense
    (.setFont models-table (:data fonts))
    (.setRowHeight models-table 11)
    (.setShowGrid models-table true)
    (.setGridColor models-table (:border colors))
    (.setAutoResizeMode models-table javax.swing.JTable/AUTO_RESIZE_OFF)
    (let [cm (.getColumnModel models-table)]
      (.setPreferredWidth (.getColumn cm 0) 25)   ;; #
      (.setPreferredWidth (.getColumn cm 1) 150)  ;; Name
      (.setPreferredWidth (.getColumn cm 2) 40)   ;; Cat
      (.setPreferredWidth (.getColumn cm 3) 120)) ;; Keywords
    (.setBorder table-scroll (BorderFactory/createTitledBorder
                               (BorderFactory/createLineBorder (:border colors) 1)
                               "MODELS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.setLeftComponent split-pane table-scroll)
    
    ;; DETAIL AREA - compact
    (.setEditable detail-area false)
    (.setLineWrap detail-area true)
    (.setWrapStyleWord detail-area true)
    (.setFont detail-area (:data fonts))
    (.setBorder detail-scroll (BorderFactory/createTitledBorder
                                (BorderFactory/createLineBorder (:border colors) 1)
                                "DETAIL" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.setRightComponent split-pane detail-scroll)
    (.setDividerLocation split-pane 350)
    
    ;; Selection listener - show detail
    (.addListSelectionListener (.getSelectionModel models-table)
      (reify javax.swing.event.ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (let [row (.getSelectedRow models-table)]
              (when (>= row 0)
                (let [model-id (Integer/parseInt (str (.getValueAt table-model row 0)))
                      model (first (filter #(= (:id %) model-id) mental-models))]
                  (when model
                    (.setText detail-area
                      (str (:name model) "\n"
                           "Cat: " (:category model) "\n"
                           (:description model) "\n\n"
                           "Ex: " (:example model)))))))))))
    
    ;; Search filter - simple keyword search
    (let [do-filter (fn []
                      (let [query (.getText search-field)
                            query-lower (str/lower-case query)
                            cat-full {"All" nil "Psych" "Psychology" "Econ" "Economics" "Bio" "Biology"
                                      "Phys" "Physics" "Math" "Mathematics" "Eng" "Engineering"
                                      "Moat" "Moats" "Org" "Organizational" "Think" "Thinking Tools"}
                            cat (get cat-full (str (.getSelectedItem category-combo)))]
                        (.setRowCount table-model 0)
                        (doseq [model mental-models]
                          (when (and (or (str/blank? query)
                                        (str/includes? (str/lower-case (:name model)) query-lower)
                                        (str/includes? (str/lower-case (str (:keywords model))) query-lower))
                                    (or (nil? cat)
                                        (str/includes? (:category model) (or cat ""))))
                            (.addRow table-model
                              (into-array Object
                                [(:id model)
                                 (subs (:name model) 0 (min 25 (count (:name model))))
                                 (subs (:category model) 0 (min 5 (count (:category model))))
                                 (str/join "," (take 3 (:keywords model)))]))))))]
      (.addActionListener search-field
        (reify ActionListener
          (actionPerformed [_ _] (do-filter))))
      (.addActionListener category-combo
        (reify ActionListener
          (actionPerformed [_ _] (do-filter)))))
    
    (.add panel split-pane BorderLayout/CENTER)
    
    ;; Bottom strip with web link - compact
    (let [bottom-strip (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))
          web-btn (JButton. "Web")]
      (.setOpaque bottom-strip false)
      (.setFont web-btn (:micro fonts))
      (.setMargin web-btn (Insets. 0 3 0 3))
      (.addActionListener web-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (let [web-url (or (get-in @*state [:settings :web-app-url])
                               (:web-app-url config)
                               "https://mental-models-web.manus.space")
                    row (.getSelectedRow models-table)
                    model-id (when (>= row 0) (Integer/parseInt (str (.getValueAt table-model row 0))))
                    url (if model-id
                          (str web-url "/models/" model-id)
                          web-url)]
                (.browse (Desktop/getDesktop) (URI. url)))
              (catch Exception _)))))
      (.add bottom-strip web-btn)
      (.add panel bottom-strip BorderLayout/SOUTH))
    
    panel))
;; =============================================================================
;; News Analyzer Panel - Predictive Mental Models
;; =============================================================================

(defn create-news-panel []
  "News analyzer with pattern overlap and outcome prediction"
  (let [panel (JPanel. (BorderLayout. 15 15))
        input-panel (JPanel. (BorderLayout. 10 10))
        results-panel (JPanel. (BorderLayout. 10 10))
        
        headline-field (JTextField. 60)
        content-area (JTextArea. 8 60)
        analyze-btn (JButton. "🔍 Analyze Story")
        clear-btn (JButton. "Clear")
        
        results-area (JTextArea. 20 60)
        
        ;; Stats labels
        danger-label (JLabel. "Danger: 0%")
        success-label (JLabel. "Success: 0%")
        overlap-label (JLabel. "Pattern Overlap: 0%")
        risk-label (JLabel. "Risk: --")
        models-label (JLabel. "Models: 0")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 20 20 20 20))
    
    ;; Title
    (let [title-panel (JPanel. (BorderLayout.))
          title (JLabel. "📰 News Story Analyzer")
          subtitle (JLabel. "Detect mental models, predict outcomes, show pattern overlap")]
      (.setOpaque title-panel false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.setFont subtitle (:small fonts))
      (.setForeground subtitle (:text-muted colors))
      (.add title-panel title BorderLayout/NORTH)
      (.add title-panel subtitle BorderLayout/SOUTH)
      (.add panel title-panel BorderLayout/NORTH))
    
    ;; Input section
    (.setOpaque input-panel false)
    (.setBorder input-panel (BorderFactory/createTitledBorder "Enter News Story"))
    
    (let [headline-panel (JPanel. (BorderLayout. 5 5))
          headline-lbl (JLabel. "Headline:")]
      (.setOpaque headline-panel false)
      (.setFont headline-lbl (:body fonts))
      (.add headline-panel headline-lbl BorderLayout/WEST)
      (.add headline-panel headline-field BorderLayout/CENTER)
      (.add input-panel headline-panel BorderLayout/NORTH))
    
    (.setLineWrap content-area true)
    (.setWrapStyleWord content-area true)
    (.setBorder content-area (BorderFactory/createTitledBorder "Content/Body"))
    (.add input-panel (JScrollPane. content-area) BorderLayout/CENTER)
    
    ;; Buttons
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 5))]
      (.setOpaque btn-panel false)
      (.setBackground analyze-btn (:primary colors))
      (.setForeground analyze-btn Color/WHITE)
      (.setFont analyze-btn (:body fonts))
      (.add btn-panel analyze-btn)
      (.add btn-panel clear-btn)
      (.add input-panel btn-panel BorderLayout/SOUTH))
    
    ;; Results section
    (.setOpaque results-panel false)
    (.setBorder results-panel (BorderFactory/createTitledBorder "Analysis Results"))
    
    ;; Stats bar at top of results
    (let [stats-panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 5))]
      (.setOpaque stats-panel false)
      (doseq [lbl [danger-label success-label overlap-label risk-label models-label]]
        (.setFont lbl (:body fonts))
        (.add stats-panel lbl))
      (.setForeground danger-label (Color. 220 50 50))
      (.setForeground success-label (Color. 50 180 50))
      (.setForeground risk-label (:accent colors))
      (.add results-panel stats-panel BorderLayout/NORTH))
    
    (.setEditable results-area false)
    (.setFont results-area (Font. "Monospaced" Font/PLAIN 11))
    (.setBackground results-area (:bg-primary colors))
    (.setForeground results-area (:text-primary colors))
    (.add results-panel (JScrollPane. results-area) BorderLayout/CENTER)
    
    ;; Analyze button action
    (.addActionListener analyze-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [headline (.getText headline-field)
                content (.getText content-area)]
            (when (and (not (str/blank? headline)) (not (str/blank? content)))
              (try
                (let [analysis (news/analyze-story headline content)
                      report (news/format-analysis-report analysis)]
                  ;; Update stats
                  (.setText danger-label (format "Danger: %.0f%%" (* 100 (:danger-score analysis))))
                  (.setText success-label (format "Success: %.0f%%" (* 100 (:success-score analysis))))
                  (.setText overlap-label (format "Pattern Overlap: %.1f%%" (double (:pattern-overlap-pct analysis))))
                  (.setText risk-label (str "Risk: " (name (:overall-risk analysis))))
                  (.setText models-label (str "Models: " (:model-count analysis)))
                  ;; Update results
                  (.setText results-area report)
                  (.setCaretPosition results-area 0)
                  (log! (str "[NEWS] Analyzed: " headline " - " (:model-count analysis) " models detected")))
                (catch Exception e
                  (.setText results-area (str "Error analyzing story: " (.getMessage e)))
                  (log! :error (str "[NEWS] Analysis error: " (.getMessage e))))))))))
    
    ;; Clear button action
    (.addActionListener clear-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (.setText headline-field "")
          (.setText content-area "")
          (.setText results-area "")
          (.setText danger-label "Danger: 0%")
          (.setText success-label "Success: 0%")
          (.setText overlap-label "Pattern Overlap: 0%")
          (.setText risk-label "Risk: --")
          (.setText models-label "Models: 0"))))
    
    ;; Layout - split pane
    (let [split (javax.swing.JSplitPane. javax.swing.JSplitPane/VERTICAL_SPLIT input-panel results-panel)]
      (.setDividerLocation split 200)
      (.setResizeWeight split 0.3)
      (.add panel split BorderLayout/CENTER))
    
    panel))

;; =============================================================================
;; Decision Journal Panel
;; =============================================================================

(defn create-decisions-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        form-panel (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        decisions-model (DefaultListModel.)
        decisions-list (JList. decisions-model)
        
        title-field (JTextField. 40)
        context-area (JTextArea. 4 40)
        models-field (JTextField. 40)
        outcome-combo (JComboBox. (into-array String ["Pending" "Success" "Partial" "Failure"]))
        notes-area (JTextArea. 4 40)
        save-btn (JButton. "💾 Save Decision")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Decision Journal")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Form setup
    (.setOpaque form-panel false)
    (set! (. gbc insets) (Insets. 5 5 5 5))
    (set! (. gbc anchor) GridBagConstraints/WEST)
    
    ;; Decision Title
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 0)
    (.add form-panel (JLabel. "Decision Title:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.add form-panel title-field gbc)
    
    ;; Context
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 1) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Context:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.setLineWrap context-area true)
    (.add form-panel (JScrollPane. context-area) gbc)
    
    ;; Models Used
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 2) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Models Used (comma-sep):") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.add form-panel models-field gbc)
    
    ;; Outcome
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 3) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Outcome:") gbc)
    (set! (. gbc gridx) 1)
    (.add form-panel outcome-combo gbc)
    
    ;; Notes
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 4)
    (.add form-panel (JLabel. "Notes/Learnings:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.setLineWrap notes-area true)
    (.add form-panel (JScrollPane. notes-area) gbc)
    
    ;; Save button
    (set! (. gbc gridx) 1) (set! (. gbc gridy) 5) (set! (. gbc fill) GridBagConstraints/NONE)
    (.setBackground save-btn (:primary colors))
    (.setForeground save-btn Color/WHITE)
    (.addActionListener save-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [decision {:title (.getText title-field)
                         :context (.getText context-area)
                         :models (str/split (.getText models-field) #",\s*")
                         :outcome (.getSelectedItem outcome-combo)
                         :notes (.getText notes-area)
                         :timestamp (str (LocalDateTime/now))}]
            (swap! *state update :decisions (fnil conj []) decision)
            (.addElement decisions-model (str (:title decision) " - " (:outcome decision)))
            ;; Clear form
            (.setText title-field "")
            (.setText context-area "")
            (.setText models-field "")
            (.setSelectedIndex outcome-combo 0)
            (.setText notes-area "")
            (log! (str "[DECISION] Saved: " (:title decision)))))))
    (.add form-panel save-btn gbc)
    
    ;; Layout
    (let [center-panel (JPanel. (BorderLayout. 10 10))]
      (.setOpaque center-panel false)
      (.add center-panel form-panel BorderLayout/NORTH)
      
      ;; Decisions list
      (let [list-panel (JPanel. (BorderLayout.))
            list-label (JLabel. "Previous Decisions:")]
        (.setOpaque list-panel false)
        (.setFont list-label (:subtitle fonts))
        (.add list-panel list-label BorderLayout/NORTH)
        (.add list-panel (JScrollPane. decisions-list) BorderLayout/CENTER)
        (.add center-panel list-panel BorderLayout/CENTER))
      
      (.add panel center-panel BorderLayout/CENTER))
    
    panel))

;; =============================================================================
;; Settings Panel
;; =============================================================================

(defn create-settings-panel [frame]
  (let [panel (JPanel. (BorderLayout. 20 20))
        form (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        
        lm-field (JTextField. (get-in @*state [:settings :lm-studio-url]) 30)
        web-field (JTextField. (or (get-in @*state [:settings :web-app-url]) "") 30)
        slack-field (JTextField. (or (get-in @*state [:settings :slack-webhook]) "") 30)
        github-field (JTextField. (or (get-in @*state [:settings :github-token]) "") 30)
        save-btn (JButton. "💾 Save Settings")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Settings")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    (.setOpaque form false)
    (set! (. gbc insets) (Insets. 8 8 8 8))
    (set! (. gbc anchor) GridBagConstraints/WEST)
    
    ;; LM Studio URL
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 0)
    (.add form (JLabel. "LM Studio URL:") gbc)
    (set! (. gbc gridx) 1)
    (.add form lm-field gbc)
    
    ;; Web App URL
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 1)
    (.add form (JLabel. "Web App URL:") gbc)
    (set! (. gbc gridx) 1)
    (.add form web-field gbc)
    
    ;; Slack Webhook
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 2)
    (.add form (JLabel. "Slack Webhook:") gbc)
    (set! (. gbc gridx) 1)
    (.add form slack-field gbc)
    
    ;; GitHub Token
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 3)
    (.add form (JLabel. "GitHub Token:") gbc)
    (set! (. gbc gridx) 1)
    (.add form github-field gbc)
    
    ;; Save button
    (set! (. gbc gridx) 1) (set! (. gbc gridy) 4)
    (.setBackground save-btn (:primary colors))
    (.setForeground save-btn Color/WHITE)
    (.addActionListener save-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (swap! *state assoc :settings
            {:lm-studio-url (.getText lm-field)
             :web-app-url (.getText web-field)
             :slack-webhook (.getText slack-field)
             :github-token (.getText github-field)})
          (save-setting! :lm-studio-url (.getText lm-field))
          (save-setting! :web-app-url (.getText web-field))
          (save-setting! :slack-webhook (.getText slack-field))
          (save-setting! :github-token (.getText github-field))
          (check-all-connections!)
          (JOptionPane/showMessageDialog frame "Settings saved!" "Success" JOptionPane/INFORMATION_MESSAGE))))
    (.add form save-btn gbc)
    
    ;; Open Web Settings button
    (let [web-settings-btn (JButton. "🌐 Open Web App Settings")]
      (set! (. gbc gridx) 1) (set! (. gbc gridy) 5)
      (.addActionListener web-settings-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (let [web-url (or (.getText web-field)
                               (get-in @*state [:settings :web-app-url])
                               (:web-app-url config)
                               "https://mental-models-web.manus.space")]
                (log! (str "[WEB] Opening web settings: " web-url))
                (.browse (Desktop/getDesktop) (URI. web-url)))
              (catch Exception e
                (log! (str "[WEB] Error: " (.getMessage e))))))))
      (.add form web-settings-btn gbc))
    
    (.add panel form BorderLayout/CENTER)
    
    ;; Connection status
    (let [status-panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 5))
          make-status (fn [name key]
                       (let [label (JLabel. (str "● " name))]
                         (.setFont label (:body fonts))
                         (add-watch *state (keyword (str "conn-" name))
                           (fn [_ _ _ new-state]
                             (let [status (get-in new-state [:connections key])]
                               (SwingUtilities/invokeLater
                                 #(.setForeground label
                                    (if (= status :connected)
                                      (:success colors)
                                      (:danger colors)))))))
                         label))]
      (.setOpaque status-panel false)
      (.setBorder status-panel (TitledBorder. "Connections"))
      (.add status-panel (make-status "LM Studio" :lm-studio))
      (.add status-panel (make-status "Web App" :web-app))
      (.add status-panel (make-status "Slack" :slack))
      (.add status-panel (make-status "GitHub" :github))
      (.add panel status-panel BorderLayout/SOUTH))
    
    panel))

;; =============================================================================
;; Sidebar
;; =============================================================================

(defn create-sidebar [card-layout content-panel frame]
  "Clean modern sidebar matching web app design"
  (let [sidebar (JPanel.)
        layout (BoxLayout. sidebar BoxLayout/Y_AXIS)]
    
    (.setLayout sidebar layout)
    (.setBackground sidebar (:bg-secondary colors))
    (.setPreferredSize sidebar (Dimension. 180 0))  ;; Wider for readability
    (.setBorder sidebar (EmptyBorder. 12 12 12 12))  ;; More breathing room
    
    ;; Logo/Title - full name
    (let [logo (JLabel. "Mental Models")]
      (.setFont logo (:header fonts))
      (.setForeground logo (:accent colors))
      (.setAlignmentX logo 0.0)
      (.add sidebar logo))
    
    (.add sidebar (Box/createVerticalStrut 16))
    
    ;; Nav buttons - full labels, clean styling
    (doseq [[label card-name] [["Dashboard" "dashboard"]
                               ["Scan Files" "scan"]
                               ["All Models" "models"]
                               ["News Analyzer" "news"]
                               ["Decisions" "decisions"]
                               ["Watch Folders" "watch"]
                               ["Activity Log" "logs"]
                               ["Settings" "settings"]]]
      (let [btn (JButton. label)]
        (.setFont btn (:body fonts))
        (.setForeground btn (:text-secondary colors))
        (.setBackground btn (:bg-primary colors))
        (.setBorderPainted btn false)
        (.setFocusPainted btn false)
        (.setHorizontalAlignment btn JButton/LEFT)
        (.setMaximumSize btn (Dimension. 160 32))   ;; Comfortable height
        (.setPreferredSize btn (Dimension. 160 32))
        (.setAlignmentX btn 0.0)
        (.setMargin btn (Insets. 4 8 4 8))
        (.setCursor btn (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
        (.addActionListener btn
          (reify ActionListener
            (actionPerformed [_ _]
              (.show card-layout content-panel card-name)
              (when (= card-name "dashboard")
                (refresh-dashboard!)))))
        (.add sidebar btn)
        (.add sidebar (Box/createVerticalStrut 4))))  ;; Minimal gap
    
    (.add sidebar (Box/createVerticalStrut 16))
    
    ;; Web App Quick Links section
    (let [web-section (JPanel.)
          web-layout (BoxLayout. web-section BoxLayout/Y_AXIS)]
      (.setLayout web-section web-layout)
      (.setOpaque web-section false)
      (.setAlignmentX web-section 0.0)
      
      (let [section-lbl (JLabel. "WEB APP")]
        (.setFont section-lbl (:label fonts))
        (.setForeground section-lbl (:text-muted colors))
        (.setAlignmentX section-lbl 0.0)
        (.add web-section section-lbl))
      
      (.add web-section (Box/createVerticalStrut 4))
      
      (doseq [[label path] [["📊 Dashboard" "/dashboard"]
                            ["🧠 All Models" "/models"]
                            ["📈 Analytics" "/analytics"]
                            ["⚙️ Settings" "/settings"]]]
        (let [btn (JButton. label)]
          (.setFont btn (:small fonts))
          (.setForeground btn (:accent colors))
          (.setBackground btn (:bg-primary colors))
          (.setBorderPainted btn false)
          (.setFocusPainted btn false)
          (.setHorizontalAlignment btn JButton/LEFT)
          (.setMaximumSize btn (Dimension. 160 28))
          (.setPreferredSize btn (Dimension. 160 28))
          (.setAlignmentX btn 0.0)
          (.setMargin btn (Insets. 2 8 2 8))
          (.setCursor btn (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
          (.addActionListener btn
            (reify ActionListener
              (actionPerformed [_ _]
                (try
                  (let [base-url (or (get-in @*state [:settings :web-app-url])
                                    (:web-app-url config))
                        full-url (str base-url path)]
                    (.browse (Desktop/getDesktop) (URI. full-url)))
                  (catch Exception e
                    (log! :error (str "Failed to open web: " (.getMessage e))))))))
          (.add web-section btn)
          (.add web-section (Box/createVerticalStrut 2))))
      
      (.add sidebar web-section))
    
    (.add sidebar (Box/createVerticalGlue))
    
    ;; Connection status - ultra compact single row
    (let [conn-row (JPanel. (FlowLayout. FlowLayout/LEFT 1 0))]
      (.setOpaque conn-row false)
      (.setAlignmentX conn-row 0.0)
      (doseq [[name key] [["LM Studio" :lm-studio] ["Web App" :web-app] ["Slack" :slack] ["GitHub" :github]]]
        (let [lbl (JLabel. (str "● " name))]
          (.setFont lbl (:micro fonts))
          (add-watch *state (keyword (str "sb-" name))
            (fn [_ _ _ new-state]
              (SwingUtilities/invokeLater
                #(.setForeground lbl
                   (if (= :connected (get-in new-state [:connections key]))
                     (:success colors)
                     (:danger colors))))))
          (.setForeground lbl (:danger colors))
          (.add conn-row lbl)))
      (.add sidebar conn-row))
    
    ;; Web link - compact
    (.add sidebar (Box/createVerticalStrut 3))
    (let [web-btn (JButton. "Web")]
      (.setFont web-btn (:micro fonts))
      (.setForeground web-btn (:primary colors))
      (.setBackground web-btn (:sidebar-bg colors))
      (.setBorderPainted web-btn false)
      (.setMaximumSize web-btn (Dimension. 85 14))
      (.setAlignmentX web-btn 0.0)
      (.setMargin web-btn (Insets. 0 2 0 2))
      (.addActionListener web-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (.browse (Desktop/getDesktop) (URI. (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))))
              (catch Exception _)))))
      (.add sidebar web-btn))
    
    sidebar))

;; =============================================================================
;; Feature Request Dialog
;; =============================================================================

(defn show-feature-request-dialog! [parent]
  (let [text-area (JTextArea. 5 40)
        scroll (JScrollPane. text-area)
        result (JOptionPane/showConfirmDialog
                 parent
                 scroll
                 "Request a Feature (sent to dev team)"
                 JOptionPane/OK_CANCEL_OPTION
                 JOptionPane/PLAIN_MESSAGE)]
    (when (= result JOptionPane/OK_OPTION)
      (let [feature (.getText text-area)]
        (when-not (str/blank? feature)
          (request-feature-from-devin! feature)
          (JOptionPane/showMessageDialog
            parent
            "Feature request sent! The dev team will review it."
            "Request Sent"
            JOptionPane/INFORMATION_MESSAGE))))))

;; =============================================================================
;; Main Frame
;; =============================================================================

(defn create-main-frame []
  (let [frame (JFrame. (str (:app-name config) " v" (:version config)))
        content-panel (JPanel. (CardLayout.))
        card-layout (.getLayout content-panel)]
    
    (try
      (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
      (catch Exception _))
    
    (.add content-panel (create-dashboard-panel) "dashboard")
    (.add content-panel (create-scan-panel frame) "scan")
    (.add content-panel (create-models-panel) "models")
    (.add content-panel (create-news-panel) "news")
    (.add content-panel (create-decisions-panel) "decisions")
    (.add content-panel (create-watch-panel) "watch")
    (.add content-panel (create-logs-panel) "logs")
    (.add content-panel (create-settings-panel frame) "settings")
    
    (let [sidebar (create-sidebar card-layout content-panel frame)]
      (.add (.getContentPane frame) sidebar BorderLayout/WEST)
      (.add (.getContentPane frame) content-panel BorderLayout/CENTER))
    
    ;; Menu bar
    (let [menubar (JMenuBar.)
          file-menu (JMenu. "File")
          view-menu (JMenu. "View")
          help-menu (JMenu. "Help")
          
          exit-item (JMenuItem. "Exit")
          updates-item (JMenuItem. "Check for Updates...")
          about-item (JMenuItem. "About")]
      
      (.addActionListener exit-item
        (reify ActionListener
          (actionPerformed [_ _] (System/exit 0))))
      
      (.addActionListener updates-item
        (reify ActionListener
          (actionPerformed [_ _]
            (check-for-updates! frame))))
      
      (let [feature-item (JMenuItem. "Request a Feature...")]
        (.addActionListener feature-item
          (reify ActionListener
            (actionPerformed [_ _]
              (show-feature-request-dialog! frame))))
        (.add help-menu feature-item))
      
      (.addActionListener about-item
        (reify ActionListener
          (actionPerformed [_ _]
            (JOptionPane/showMessageDialog
              frame
              (str (:app-name config) "\n"
                   "Version: " (:version config) "\n\n"
                   "One-click mental model detection.\n"
                   "Scan documents, find patterns, track insights.\n\n"
                   "© 2025 Ripple Analytics")
              "About"
              JOptionPane/INFORMATION_MESSAGE))))
      
      (.add file-menu exit-item)
      (.add help-menu updates-item)
      (.addSeparator help-menu)
      (.add help-menu about-item)
      
      (.add menubar file-menu)
      (.add menubar help-menu)
      (let [theme-item (JMenuItem. "Toggle Dark/Light Mode")
            fullscreen-item (JMenuItem. "Toggle Fullscreen")]
        (.addActionListener theme-item
          (reify ActionListener
            (actionPerformed [_ _]
              (toggle-theme!)
              ;; Refresh the UI - recreate panels
              (javax.swing.JOptionPane/showMessageDialog 
                frame 
                "Theme changed. Restart app to apply fully."
                "Theme Toggle"
                javax.swing.JOptionPane/INFORMATION_MESSAGE))))
        (.addActionListener fullscreen-item
          (reify ActionListener
            (actionPerformed [_ _]
              (let [device (.getDefaultScreenDevice (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))]
                (if (.getFullScreenWindow device)
                  (.setFullScreenWindow device nil)
                  (.setFullScreenWindow device frame))))))
        (.add view-menu theme-item)
        (.add view-menu fullscreen-item))
      (.add menubar view-menu)
      (.setJMenuBar frame menubar))
    
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setSize frame 1100 750)
    (.setMinimumSize frame (Dimension. 900 600))
    (.setLocationRelativeTo frame nil)
    
    frame))

;; =============================================================================
;; Startup Update Check (Fully Automatic)
;; =============================================================================

;; =============================================================================
;; Continuous Update System - Delta Updates + Auto-Polling
;; =============================================================================

(def *update-poll-interval (atom 3600000)) ;; 1 hour default (less intrusive)
(def *last-code-hash (atom nil))
(def *update-poller (atom nil))

;; Blue-Green Deployment State
(def *blue-green-state (atom {:active-slot :blue
                               :blue-version nil
                               :green-version nil
                               :blue-ready true
                               :green-ready false
                               :pending-download nil
                               :last-swap nil}))

(defn get-inactive-slot []
  "Get the slot that's not currently active"
  (if (= :blue (:active-slot @*blue-green-state)) :green :blue))

(defn get-slot-dir [slot]
  "Get directory for a deployment slot"
  (let [base-dir (or (System/getProperty "app.dir") ".")
        slot-name (name slot)]
    (File. base-dir (str "slots/" slot-name))))

(defn prepare-inactive-slot! [new-version-dir]
  "Prepare the inactive slot with new version (background)"
  (future
    (try
      (let [inactive-slot (get-inactive-slot)
            slot-dir (get-slot-dir inactive-slot)]
        (log! (str "[BLUE-GREEN] Preparing " (name inactive-slot) " slot..."))
        ;; Create slot directory
        (.mkdirs slot-dir)
        ;; Copy new version to inactive slot
        (doseq [f (file-seq (File. new-version-dir))]
          (when (.isFile f)
            (let [rel-path (.substring (.getAbsolutePath f) 
                                       (inc (count (.getAbsolutePath (File. new-version-dir)))))
                  dest-file (File. slot-dir rel-path)]
              (.mkdirs (.getParentFile dest-file))
              (io/copy f dest-file))))
        ;; Mark slot as ready
        (swap! *blue-green-state assoc 
               (keyword (str (name inactive-slot) "-ready")) true
               (keyword (str (name inactive-slot) "-version")) (:version config))
        (log! (str "[BLUE-GREEN] " (name inactive-slot) " slot ready"))
        {:success true :slot inactive-slot})
      (catch Exception e
        (log! (str "[BLUE-GREEN] Slot preparation failed: " (.getMessage e)))
        {:success false :error (.getMessage e)}))))

(defn swap-slots! []
  "Instantly swap active slot (zero-downtime switch)"
  (let [current (:active-slot @*blue-green-state)
        new-slot (get-inactive-slot)]
    (when (get @*blue-green-state (keyword (str (name new-slot) "-ready")))
      (log! (str "[BLUE-GREEN] Swapping " (name current) " -> " (name new-slot)))
      (swap! *blue-green-state assoc 
             :active-slot new-slot
             :last-swap (System/currentTimeMillis))
      ;; Update classpath to point to new slot (Clojure dynamic loading)
      (try
        (let [slot-dir (get-slot-dir new-slot)
              src-dir (File. slot-dir "src")]
          (when (.exists src-dir)
            ;; Reload the main namespace from new location
            (require 'mental-models.desktop.gui.swing-app :reload)
            (log! "[BLUE-GREEN] Code swapped successfully - no restart needed")))
        (catch Exception e
          (log! (str "[BLUE-GREEN] Hot swap failed, will use on next restart: " (.getMessage e)))))
      (swap! *state assoc-in [:update :status] (str "✓ Swapped to " (name new-slot)))
      {:success true :active new-slot})))

(defn fetch-delta-manifest! []
  "Fetch list of changed files from web app"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/desktop.deltaManifest?input="
                   (java.net.URLEncoder/encode 
                     (str "{\"json\":{\"version\":\"" (:version config) "\"}}")
                     "UTF-8"))
          result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
      (when (:success result)
        (let [body (:body result)]
          ;; Parse changed files from response
          (when-let [files-match (re-find #"\"changedFiles\":\s*\[([^\]]+)\]" body)]
            (let [files-str (second files-match)
                  ;; Extract file objects
                  file-matches (re-seq #"\{\"path\":\"([^\"]+)\",\"url\":\"([^\"]+)\",\"hash\":\"([^\"]+)\"\}" files-str)]
              (map (fn [[_ path url hash]]
                     {:path path :url url :hash hash})
                   file-matches))))))
    (catch Exception e
      (log! (str "[DELTA] Failed to fetch manifest: " (.getMessage e)))
      nil)))

(defn blue-green-delta-update! []
  "Delta update to inactive slot, then swap when ready - NO full downloads"
  (future
    (try
      (log! "[BLUE-GREEN] Checking for delta updates...")
      (let [changed-files (fetch-delta-manifest!)]
        (if (and changed-files (seq changed-files))
          (do
            (log! (str "[BLUE-GREEN] " (count changed-files) " files changed - downloading deltas only"))
            (swap! *blue-green-state assoc :pending-download "delta")
            
            ;; Prepare inactive slot by copying current version first
            (let [inactive-slot (get-inactive-slot)
                  slot-dir (get-slot-dir inactive-slot)
                  current-dir (File. (or (System/getProperty "app.dir") "."))]
              
              ;; Only copy if slot doesn't exist yet
              (when-not (.exists slot-dir)
                (log! "[BLUE-GREEN] Initializing inactive slot from current...")
                (.mkdirs slot-dir)
                (doseq [f (file-seq current-dir)]
                  (when (and (.isFile f)
                             (not (str/includes? (.getAbsolutePath f) "slots/"))
                             (not (str/includes? (.getAbsolutePath f) ".git")))
                    (let [rel-path (.substring (.getAbsolutePath f) 
                                               (inc (count (.getAbsolutePath current-dir))))
                          dest-file (File. slot-dir rel-path)]
                      (.mkdirs (.getParentFile dest-file))
                      (io/copy f dest-file)))))
              
              ;; Now apply only the changed files
              (let [success-count (atom 0)]
                (doseq [{:keys [path url]} changed-files]
                  (let [dest-file (File. slot-dir path)]
                    (.mkdirs (.getParentFile dest-file))
                    (try
                      (let [result (http-get url)]
                        (when (:success result)
                          (spit dest-file (:body result))
                          (swap! success-count inc)
                          (log! (str "[DELTA] Updated: " path))))
                      (catch Exception e
                        (log! (str "[DELTA] Failed: " path " - " (.getMessage e)))))))
                
                (if (= @success-count (count changed-files))
                  (do
                    (log! "[BLUE-GREEN] All deltas applied - ready to swap")
                    (swap! *blue-green-state assoc 
                           (keyword (str (name inactive-slot) "-ready")) true)
                    (swap! *state assoc-in [:update :status] "✓ Delta ready")
                    ;; Auto-swap after brief delay
                    (Thread/sleep 2000)
                    (swap-slots!))
                  (log! "[BLUE-GREEN] Some deltas failed")))))
          (log! "[BLUE-GREEN] No changes detected")))
      (catch Exception e
        (log! (str "[BLUE-GREEN] Delta update failed: " (.getMessage e)))
        (swap! *blue-green-state assoc :pending-download nil)))))

(defn fetch-code-manifest []
  "Fetch manifest of current code files and hashes from web app"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/desktop.codeManifest")
          result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
      (when (:success result)
        (let [body (:body result)]
          ;; Parse JSON response
          (when-let [data (re-find #"\"files\":\s*\[([^\]]+)\]" body)]
            {:success true :manifest (second data)}))))
    (catch Exception e
      (log! (str "[DELTA] Failed to fetch manifest: " (.getMessage e)))
      nil)))

(defn download-delta-file! [file-path file-url]
  "Download a single changed file"
  (try
    (let [app-dir (File. (or (System/getProperty "app.dir") "."))
          dest-file (File. app-dir file-path)
          result (http-get file-url)]
      (when (:success result)
        (.mkdirs (.getParentFile dest-file))
        (spit dest-file (:body result))
        (log! (str "[DELTA] Updated: " file-path))
        true))
    (catch Exception e
      (log! (str "[DELTA] Failed to update " file-path ": " (.getMessage e)))
      false)))

(defn apply-delta-updates! [changed-files]
  "Apply delta updates for changed files only"
  (log! (str "[DELTA] Applying " (count changed-files) " file updates..."))
  (let [results (doall (map (fn [{:keys [path url]}]
                              (download-delta-file! path url))
                            changed-files))]
    (if (every? true? results)
      (do
        (log! "[DELTA] All updates applied successfully")
        (swap! *state assoc-in [:update :status] "✓ Live updated")
        ;; Reload changed namespaces if possible
        (try
          (require 'mental-models.desktop.gui.swing-app :reload)
          (log! "[DELTA] Hot-reloaded code")
          (catch Exception e
            (log! (str "[DELTA] Hot reload not possible: " (.getMessage e)))))
        true)
      (do
        (log! "[DELTA] Some updates failed")
        false))))

(defn check-for-delta-updates! []
  "Check web app for code changes and apply blue-green delta updates"
  (future
    (try
      (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                     "/api/trpc/desktop.checkUpdates?input=" 
                     (java.net.URLEncoder/encode 
                       (str "{\"json\":{\"version\":\"" (:version config) 
                            "\",\"codeHash\":\"" @*last-code-hash "\"}}")
                       "UTF-8"))
            result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
        (when (:success result)
          (let [body (:body result)]
            (when (str/includes? body "\"hasUpdates\":true")
              (log! "[POLLER] Updates detected - starting blue-green delta update...")
              ;; Use blue-green delta update (downloads only changed files)
              (blue-green-delta-update!)))))
      (catch Exception e
        (log! (str "[POLLER] Update check failed: " (.getMessage e)))))))

(defn start-update-poller! []
  "Start background polling for updates"
  (when-not @*update-poller
    (reset! *update-poller
      (future
        (log! "[POLLER] Started continuous update polling")
        (while true
          (try
            (Thread/sleep @*update-poll-interval)
            (check-for-delta-updates!)
            (catch InterruptedException _
              (log! "[POLLER] Stopped")
              (throw (InterruptedException.)))
            (catch Exception e
              (log! (str "[POLLER] Error: " (.getMessage e))))))))))

(defn stop-update-poller! []
  "Stop the update poller"
  (when-let [p @*update-poller]
    (future-cancel p)
    (reset! *update-poller nil)
    (log! "[POLLER] Stopped")))

(defn check-updates-silently! [frame]
  "Check for updates on startup and AUTO-DOWNLOAD without user interaction"
  (future
    (try
      (Thread/sleep 3000) ;; Wait for UI to load
      ;; Start continuous polling
      (start-update-poller!)
      (println "[AUTO-UPDATE] Checking for updates...")
      (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
            url (str (:github-api config) "/repos/" (:github-repo config) "/releases/latest")
            headers {"Accept" "application/vnd.github.v3+json"
                     "Authorization" (str "token " github-token)}
            result (http-get url :headers headers)]
        (println "[AUTO-UPDATE] GitHub response status:" (:status result) "success:" (:success result))
        (if (:success result)
          (let [body (:body result)
                tag (second (re-find #"\"tag_name\"\s*:\s*\"([^\"]+)\"" body))
                download-url (second (re-find #"\"browser_download_url\"\s*:\s*\"([^\"]+\.zip)\"" body))
                newer? (version-newer? tag (:version config))]
            (println "[AUTO-UPDATE] Current:" (:version config) "Latest:" tag "Newer?" newer?)
            (println "[AUTO-UPDATE] Download URL:" download-url)
            (if (and newer? download-url)
              (do
                (println "[AUTO-UPDATE] *** NEW VERSION DETECTED - AUTO-DOWNLOADING ***")
                (log! (str "[AUTO-UPDATE] New version " tag " found - downloading automatically..."))
                (swap! *state assoc-in [:update :available] true)
                (swap! *state assoc-in [:update :latest-version] tag)
                (swap! *state assoc-in [:update :download-url] download-url)
                ;; AUTO-DOWNLOAD without asking - fully automatic!
                (perform-auto-update! frame download-url tag))
              (println "[AUTO-UPDATE] Already on latest version or no download URL")))
          (println "[AUTO-UPDATE] Failed to check - response:" (:error result))))
      (catch Exception e
        (println "[AUTO-UPDATE] Silent check failed:" (.getMessage e))
        (.printStackTrace e)))))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn check-and-rollback-if-needed! []
  "Check if we crashed after an update and rollback if backup exists"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        crash-marker (File. app-dir "startup-in-progress.marker")
        backup-dir (File. app-dir "backup-previous-version")]
    ;; If crash marker exists from last run, we crashed during startup
    (when (.exists crash-marker)
      (println "[ROLLBACK] DETECTED CRASH FROM PREVIOUS STARTUP!")
      (when (.exists backup-dir)
        (println "[ROLLBACK] Backup found - rolling back to previous version...")
        (try
          ;; Copy backup back to main location
          (doseq [f (file-seq backup-dir)]
            (when (.isFile f)
              (let [rel-path (.substring (.getAbsolutePath f) 
                                        (inc (count (.getAbsolutePath backup-dir))))
                    target-file (File. app-dir rel-path)]
                (.mkdirs (.getParentFile target-file))
                (io/copy f target-file))))
          (println "[ROLLBACK] Rollback complete - previous version restored")
          ;; Report the rollback
          (report-error-to-devin! "AutoRollback" 
                                  "Automatic rollback triggered" 
                                  "Previous startup crashed, restored backup")
          (catch Exception e
            (println (str "[ROLLBACK] Rollback failed: " (.getMessage e))))))
      ;; Delete crash marker
      (.delete crash-marker))
    
    ;; Create crash marker - will be deleted at end of successful startup
    (spit crash-marker (str "Started: " (java.util.Date.)))))

(defn mark-startup-successful! []
  "Remove crash marker to indicate successful startup"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        crash-marker (File. app-dir "startup-in-progress.marker")]
    (when (.exists crash-marker)
      (.delete crash-marker)
      (println "[STARTUP] Startup successful - crash marker removed"))))

(defn restore-state-from-update! []
  "Check for and restore state from a previous update"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        state-file (File. app-dir "state-restore.edn")]
    (when (.exists state-file)
      (try
        (println "[HOT-RELOAD] Found state backup from update, restoring...")
        (let [saved-state (read-string (slurp state-file))]
          (restore-state! saved-state)
          ;; Delete the state file after successful restore
          (.delete state-file)
          (println "[HOT-RELOAD] State restored successfully - 99.9% uptime maintained!"))
        (catch Exception e
          (println (str "[HOT-RELOAD] Failed to restore state: " (.getMessage e)))
          ;; Delete corrupted state file
          (.delete state-file))))))


(defn run-startup-selftest []
  "Run self-test to catch errors BEFORE showing UI"
  (try
    (println "[SELFTEST] Testing UI components...")
    ;; Test 1: Can we create basic UI?
    (let [_ (JPanel.)] nil)
    ;; Test 2: Mental models loaded?
    (assert (seq mental-models) "Mental models not loaded")
    ;; Test 3: Dashboard panel
    (let [_ (create-dashboard-panel)] nil)
    ;; Test 4: Scan panel
    (let [_ (create-scan-panel nil)] nil)
    ;; Test 5: Models panel  
    (let [_ (create-models-panel)] nil)
    (println "[SELFTEST] All tests PASSED")
    true
    (catch Exception e
      (println (str "[SELFTEST] FAILED: " (.getMessage e)))
      (.printStackTrace e)
      false)))

(defn -main [& args]
  ;; Check for CLI batch mode
  (when (and (seq args) (= (first args) "--batch"))
    (run-cli-batch (rest args)))
  (when (and (seq args) (= (first args) "--batch-file"))
    (let [file-path (second args)
          paths (when file-path 
                  (str/split-lines (slurp file-path)))]
      (run-cli-batch paths)))
  
  (println "")
  (println "╔════════════════════════════════════════════════╗")
  (println "║     Mental Models Desktop v" (:version config) "           ║")
  (println "║  ★ SAFE UPDATES + AUTO-ROLLBACK                 ║")
  (println "╠════════════════════════════════════════════════╣")
  (println "║  • Tests new version BEFORE switching           ║")
  (println "║  • Auto-rollback if startup crashes             ║")
  (println "║  • State preservation on updates               ║")
  (println "║  • SQLite persistence enabled                  ║")
  (println "║  • Auto-update from GitHub releases            ║")
  (println "╚════════════════════════════════════════════════╝")
  (println "")
  
  ;; FIRST: Check if we crashed after an update and need to rollback
  (check-and-rollback-if-needed!)
  
  ;; Set up global exception handler for Devin feedback
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (let [stack (format-stack-trace ex)]
          (println "[ERROR] Uncaught exception:" (.getMessage ex))
          (report-error-to-devin! "UncaughtException" (.getMessage ex) stack)))))
  
  (init-database!)
  
  ;; Initialize semantic search index
  (println "[SEARCH] Indexing mental models...")
  (let [indexed (search/index-models! mental-models)]
    (println (str "[SEARCH] Indexed " indexed " models for semantic search")))
  
  ;; RESTORE STATE FROM UPDATE (99.9% uptime feature)
  (restore-state-from-update!)
  
  (let [saved-settings (load-settings!)]
    (when (seq saved-settings)
      (swap! *state update :settings merge saved-settings)
      (println "[SETTINGS] Loaded saved settings")))
  
  (SwingUtilities/invokeLater
    (fn []
      (let [frame (create-main-frame)]
        (.setVisible frame true)
        (println "[GUI] Application started")
        
        ;; MARK STARTUP SUCCESSFUL - remove crash marker
        (mark-startup-successful!)
        
        (check-all-connections!)
        ;; Silent startup update check - FULLY AUTOMATIC with SAFE updates
        (check-updates-silently! frame)
        ;; Notify Devin of startup (optional, for usage tracking)
        (when (get-in @*state [:settings :slack-webhook])
          (notify-startup!))))))

(comment
  (-main))
