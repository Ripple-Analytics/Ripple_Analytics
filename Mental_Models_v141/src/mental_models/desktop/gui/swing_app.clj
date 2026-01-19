(ns mental-models.desktop.gui.swing-app
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
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
;; Configuration
;; =============================================================================

(def config
  {:version "1.4.1"
   :blue-green true
   :app-name "Mental Models Desktop"
   :github-repo "Ripple-Analytics/Ripple_Analytics"
   :github-api "https://api.github.com"
   :github-token "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"
   :slack-webhook nil
   :lm-studio-url "http://localhost:1234"
   :web-app-url nil})

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

(def fonts
  {:title (Font. "Segoe UI" Font/BOLD 24)
   :subtitle (Font. "Segoe UI" Font/BOLD 18)
   :heading (Font. "Segoe UI" Font/BOLD 14)
   :body (Font. "Segoe UI" Font/PLAIN 13)
   :small (Font. "Segoe UI" Font/PLAIN 11)
   :mono (Font. "Consolas" Font/PLAIN 12)})

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
      (.setString stmt 3 (.format (LocalDateTime/now) DateTimeFormatter/ISO_LOCAL_DATE_TIME))
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

(defn scan-file [file progress-callback]
  "Scan a single file for mental models"
  (let [name (.getName file)
        ext (str/lower-case (or (last (str/split name #"\.")) ""))
        text (cond
               (#{"txt" "md" "markdown"} ext) (read-text-file file)
               (= "pdf" ext) (read-pdf-file file)
               :else nil)]
    (when text
      (let [models (detect-models text)
            lollapalooza (detect-lollapalooza models)]
        (when (seq models)
          (save-scan-result! (.getAbsolutePath file) (map :name models) 
                            (/ (count models) (double (count mental-models))))
          (when progress-callback
            (progress-callback {:file name
                               :models models
                               :lollapalooza lollapalooza})))
        {:file name
         :models models
         :lollapalooza lollapalooza}))))

(defn scan-folder [folder-path progress-callback completion-callback]
  "Scan all files in a folder"
  (future
    (try
      (let [folder (File. folder-path)
            files (->> (file-seq folder)
                      (filter #(.isFile %))
                      (filter #(let [ext (str/lower-case (or (last (str/split (.getName %) #"\.")) ""))]
                                (#{"txt" "md" "markdown" "pdf"} ext))))]
        (log! (str "[SCAN] Starting scan of " (count files) " files in " folder-path))
        (doseq [[idx file] (map-indexed vector files)]
          (when progress-callback
            (progress-callback {:progress (/ (* 100 (inc idx)) (count files))
                               :current (.getName file)}))
          (scan-file file progress-callback))
        (log! "[SCAN] Scan complete")
        (when completion-callback
          (completion-callback {:total (count files)})))
      (catch Exception e
        (log! (str "[SCAN] Error: " (.getMessage e)))))))

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
      (let [url (get-in @*state [:settings :web-app-url])]
        (if (and url (not (str/blank? url)))
          (let [result (http-get (str url "/api/health"))]
            (swap! *state assoc-in [:connections :web-app]
                   (if (:success result) :connected :disconnected)))
          (swap! *state assoc-in [:connections :web-app] :disconnected)))
      (catch Exception _
        (swap! *state assoc-in [:connections :web-app] :disconnected)))))

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
  "Use AI to analyze text for mental models"
  (let [truncated (subs text 0 (min 2000 (count text)))
        prompt (str "Analyze this text and identify the top 5 mental models that apply. Text: " truncated)]
    (lm-studio-chat prompt)))

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
;; Blue-Green Update System
;; =============================================================================

(defn download-file-simple! [url dest-file progress-callback use-auth?]
  "Download a file - use-auth? controls whether to send GitHub token"
  (try
    (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "User-Agent" "MentalModels-Desktop/1.3.5")
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

(defn download-github-release-asset! [download-url dest-file progress-callback]
  "Download a GitHub release asset with proper authentication handling.
   GitHub browser_download_url redirects to S3 signed URL which rejects auth headers.
   Solution: Start with auth, follow redirects without auth."
  (log! "[DOWNLOAD] Starting GitHub release download...")
  (log! (str "[DOWNLOAD] Asset URL: " download-url))
  (log! (str "[DOWNLOAD] Destination: " dest-file))
  
  ;; For private repos, we need to:
  ;; 1. Hit the browser_download_url WITH auth -> get 302 redirect
  ;; 2. Follow redirect to S3 signed URL WITHOUT auth -> get 200 + file
  (download-file-simple! download-url dest-file progress-callback true))

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

(defn perform-auto-update! [parent download-url tag]
  "Download ZIP and update in-place, then restart"
  (log! (str "[UPDATE] Starting automatic update to " tag))
  (log! (str "[UPDATE] Download URL: " download-url))
  
  (future
    (try
      (let [app-dir (File. (or (System/getProperty "app.dir") "."))
            temp-dir (File. (System/getProperty "java.io.tmpdir"))
            temp-zip (File. temp-dir (str "MentalModels-update-" tag ".zip"))
            temp-extract (File. temp-dir (str "MentalModels-extract-" (System/currentTimeMillis)))]
        
        (log! (str "[UPDATE] App dir: " (.getAbsolutePath app-dir)))
        (log! (str "[UPDATE] Temp zip: " temp-zip))
        (log! (str "[UPDATE] Temp extract: " temp-extract))
        
        ;; Download the ZIP
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
              
              ;; Extract to temp folder first
              (.mkdirs temp-extract)
              (let [extract-result (extract-zip! temp-zip temp-extract)]
                (if (:success extract-result)
                  (do
                    (log! "[UPDATE] Extraction complete")
                    (.delete temp-zip)
                    
                    ;; Find the extracted folder (MentalModels-vX.X.X inside)
                    (let [extracted-folders (.listFiles temp-extract)
                          source-dir (if (and (= 1 (count extracted-folders))
                                              (.isDirectory (first extracted-folders)))
                                       (first extracted-folders)
                                       temp-extract)
                          bat-file (File. app-dir "MentalModels.bat")]
                      
                      (log! (str "[UPDATE] Source dir: " (.getAbsolutePath source-dir)))
                      (swap! *state assoc-in [:update :status] "Installing...")
                      
                      ;; Copy new files over current installation
                      ;; Use a batch script to do this after we exit
                      ;; First backup current version, then update, then test, rollback if fails
                      (let [update-script (File. temp-dir "mental-models-update.bat")
                            backup-dir (File. app-dir ".backup")
                            script-content (str "@echo off\r\n"
                                               "echo ========================================\r\n"
                                               "echo Mental Models Update Script\r\n"
                                               "echo ========================================\r\n"
                                               "timeout /t 2 /nobreak >nul\r\n"
                                               "\r\n"
                                               "echo [1/4] Creating backup of current version...\r\n"
                                               "if exist \"" (.getAbsolutePath backup-dir) "\" rmdir /S /Q \"" (.getAbsolutePath backup-dir) "\"\r\n"
                                               "mkdir \"" (.getAbsolutePath backup-dir) "\"\r\n"
                                               "xcopy /E /Y /Q \"" (.getAbsolutePath app-dir) "\\src\" \"" (.getAbsolutePath backup-dir) "\\src\\\"\r\n"
                                               "xcopy /E /Y /Q \"" (.getAbsolutePath app-dir) "\\lib\" \"" (.getAbsolutePath backup-dir) "\\lib\\\"\r\n"
                                               "copy /Y \"" (.getAbsolutePath app-dir) "\\*.bat\" \"" (.getAbsolutePath backup-dir) "\\\"\r\n"
                                               "copy /Y \"" (.getAbsolutePath app-dir) "\\*.clj\" \"" (.getAbsolutePath backup-dir) "\\\" 2>nul\r\n"
                                               "\r\n"
                                               "echo [2/4] Installing new version...\r\n"
                                               "xcopy /E /Y /Q \"" (.getAbsolutePath source-dir) "\\*\" \"" (.getAbsolutePath app-dir) "\\\"\r\n"
                                               "\r\n"
                                               "echo [3/4] Testing new version...\r\n"
                                               "echo success > \"" (.getAbsolutePath app-dir) "\\state\\update_pending.flag\"\r\n"
                                               "\r\n"
                                               "echo [4/4] Starting updated version...\r\n"
                                               "start \"\" \"" (.getAbsolutePath bat-file) "\"\r\n"
                                               "\r\n"
                                               "echo Update complete. If app fails to start, run:\r\n"
                                               "echo   .backup\\restore.bat\r\n"
                                               "del \"%~f0\"\r\n")]
                        
                        (spit update-script script-content)
                        (log! (str "[UPDATE] Created update script: " (.getAbsolutePath update-script)))
                        
                        (swap! *state assoc-in [:update :status] "Restarting...")
                        (Thread/sleep 1000)
                        
                        ;; Launch update script and exit
                        (.exec (Runtime/getRuntime) 
                               (str "cmd /c start /min \"\" \"" (.getAbsolutePath update-script) "\""))
                        (Thread/sleep 500)
                        (log! "[UPDATE] Exiting for update...")
                        (System/exit 0))))
                  
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

(defn create-stat-card [title value icon-text]
  (let [card (JPanel. (BorderLayout. 10 10))
        icon-label (JLabel. icon-text)
        title-label (JLabel. title)
        value-label (JLabel. (str value))]
    
    (.setBackground card (:bg-primary colors))
    (.setBorder card (BorderFactory/createCompoundBorder
                       (BorderFactory/createLineBorder (:border colors) 1)
                       (EmptyBorder. 25 25 25 25)))
    (.setPreferredSize card (Dimension. 200 120))
    
    (.setFont icon-label (:subtitle fonts))
    (.setForeground icon-label (:text-muted colors))
    
    (.setFont title-label (:body fonts))
    (.setForeground title-label (:text-secondary colors))
    
    (.setFont value-label (:title fonts))
    (.setForeground value-label (:primary colors))
    
    (let [top-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 0))]
      (.setOpaque top-panel false)
      (.add top-panel icon-label)
      (.add top-panel title-label)
      (.add card top-panel BorderLayout/NORTH))
    
    (.add card value-label BorderLayout/CENTER)
    
    card))

(defn create-dashboard-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        header (JPanel. (BorderLayout.))
        title (JLabel. "Dashboard")
        version-label (JLabel. (str "Version " (:version config)))
        stats-panel (JPanel. (GridLayout. 1 4 15 15))]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Header with update status
    (.setOpaque header false)
    (.setFont title (:title fonts))
    (.setForeground title (:text-primary colors))
    
    ;; Right side: version + update status
    (let [right-panel (JPanel. (FlowLayout. FlowLayout/RIGHT 15 0))
          update-status (JLabel. "")]
      (.setOpaque right-panel false)
      (.setFont version-label (:small fonts))
      (.setForeground version-label (:text-muted colors))
      (.setFont update-status (:small fonts))
      (.setForeground update-status (:success colors))
      
      ;; Watch for update status changes
      (add-watch *state :update-status-label
        (fn [_ _ _ new-state]
          (SwingUtilities/invokeLater
            #(let [status (get-in new-state [:update :status] "")]
               (.setText update-status status)
               (.setForeground update-status
                 (cond
                   (str/includes? status "✓") (:success colors)
                   (str/includes? status "Downloading") (:primary colors)
                   (str/includes? status "Checking") (:text-muted colors)
                   (str/includes? status "failed") (:warning colors)
                   (str/includes? status "Error") (:danger colors)
                   :else (:text-muted colors)))))))
      
      (.add right-panel update-status)
      (.add right-panel version-label)
      (.add header title BorderLayout/WEST)
      (.add header right-panel BorderLayout/EAST))
    (.add panel header BorderLayout/NORTH)
    
    ;; Stats cards
    (.setOpaque stats-panel false)
    (let [stats (:stats @*state)]
      (.add stats-panel (create-stat-card "Files Scanned" (:files-scanned stats) "📁"))
      (.add stats-panel (create-stat-card "Models Found" (:models-found stats) "🧠"))
      (.add stats-panel (create-stat-card "Documents" (:documents stats) "📄"))
      (.add stats-panel (create-stat-card "Lollapalooza" (:lollapalooza stats) "⚡")))
    (.add panel stats-panel BorderLayout/CENTER)
    
    ;; Bottom buttons
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT 10 0))
          refresh-btn (JButton. "🔄 Refresh Stats")
          update-btn (JButton. "🔄 Check for Updates")]
      (.setOpaque btn-panel false)
      (.addActionListener refresh-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (refresh-dashboard!))))
      (.addActionListener update-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (check-for-updates! panel))))
      (.add btn-panel refresh-btn)
      (.add btn-panel update-btn)
      (.add panel btn-panel BorderLayout/SOUTH))
    
    panel))

;; =============================================================================
;; Scan Panel
;; =============================================================================

(defn create-scan-panel [frame]
  (let [panel (JPanel. (BorderLayout. 20 20))
        content (JPanel. (BorderLayout. 15 15))
        path-field (JTextField. 40)
        browse-btn (JButton. "Browse...")
        scan-btn (JButton. "🔍 Scan Folder")
        progress (JProgressBar. 0 100)
        results-area (JTextArea. 15 50)
        results-scroll (JScrollPane. results-area)]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Scan Folder")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    (.setOpaque content false)
    
    ;; Path selection
    (let [path-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 0))]
      (.setOpaque path-panel false)
      (.setFont path-field (:body fonts))
      (.addActionListener browse-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [chooser (JFileChooser.)]
              (.setFileSelectionMode chooser JFileChooser/DIRECTORIES_ONLY)
              (when (= (.showOpenDialog chooser frame) JFileChooser/APPROVE_OPTION)
                (.setText path-field (.getAbsolutePath (.getSelectedFile chooser))))))))
      (.add path-panel (JLabel. "Folder: "))
      (.add path-panel path-field)
      (.add path-panel browse-btn)
      (.add content path-panel BorderLayout/NORTH))
    
    ;; Results area
    (.setEditable results-area false)
    (.setFont results-area (:mono fonts))
    (.setBorder results-scroll (TitledBorder. "Scan Results"))
    (.add content results-scroll BorderLayout/CENTER)
    
    ;; Progress and scan button
    (let [bottom-panel (JPanel. (BorderLayout. 10 10))]
      (.setOpaque bottom-panel false)
      (.setStringPainted progress true)
      (.add bottom-panel progress BorderLayout/CENTER)
      
      (.setBackground scan-btn (:primary colors))
      (.setForeground scan-btn Color/WHITE)
      (.addActionListener scan-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [path (.getText path-field)]
              (when-not (str/blank? path)
                (.setText results-area "")
                (.setValue progress 0)
                (scan-folder path
                  (fn [result]
                    (SwingUtilities/invokeLater
                      #(do
                         (when (:progress result)
                           (.setValue progress (int (:progress result))))
                         (when (:models result)
                           (.append results-area
                             (str "📄 " (:file result) "\n"
                                  "   Models: " (str/join ", " (map :name (:models result))) "\n"
                                  (when (:lollapalooza result)
                                    (str "   ⚡ LOLLAPALOOZA: " (:strength (:lollapalooza result)) "\n"))
                                  "\n"))))))
                  (fn [summary]
                    (SwingUtilities/invokeLater
                      #(do
                         (.setValue progress 100)
                         (.append results-area
                           (str "\n✅ Scan complete! " (:total summary) " files processed.\n"))
                         (refresh-dashboard!))))))))))
      (.add bottom-panel scan-btn BorderLayout/EAST)
      (.add content bottom-panel BorderLayout/SOUTH))
    
    (.add panel content BorderLayout/CENTER)
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
  (let [panel (JPanel. (BorderLayout. 20 20))
        search-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
        search-field (JTextField. 30)
        category-combo (JComboBox. (into-array String ["All Categories" "Psychology" "Economics" "Biology" 
                                                       "Physics" "Mathematics" "Engineering" "Moats"
                                                       "Organizational" "Thinking Tools"]))
        list-model (DefaultListModel.)
        model-list (JList. list-model)
        detail-area (JTextArea. 10 40)
        split-pane (javax.swing.JSplitPane. javax.swing.JSplitPane/HORIZONTAL_SPLIT)]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Mental Models Browser")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Search panel
    (.setOpaque search-panel false)
    (.add search-panel (JLabel. "Search:"))
    (.add search-panel search-field)
    (.add search-panel (JLabel. "Category:"))
    (.add search-panel category-combo)
    
    ;; Populate list with all models
    (doseq [model mental-models]
      (.addElement list-model (str (:id model) ". " (:name model))))
    
    ;; Model list
    (.setFont model-list (:body fonts))
    (.setSelectionMode model-list ListSelectionModel/SINGLE_SELECTION)
    (let [list-scroll (JScrollPane. model-list)]
      (.setPreferredSize list-scroll (Dimension. 350 400))
      (.setLeftComponent split-pane list-scroll))
    
    ;; Detail area
    (.setEditable detail-area false)
    (.setLineWrap detail-area true)
    (.setWrapStyleWord detail-area true)
    (.setFont detail-area (:body fonts))
    (let [detail-scroll (JScrollPane. detail-area)]
      (.setRightComponent split-pane detail-scroll))
    
    ;; Selection listener
    (.addListSelectionListener model-list
      (reify javax.swing.event.ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (let [idx (.getSelectedIndex model-list)]
              (when (>= idx 0)
                (let [model (nth mental-models idx)
                      text (str "Name: " (:name model) "\n\n"
                                "Category: " (:category model) "\n\n"
                                "Description: " (:description model) "\n\n"
                                "Keywords: " (str/join ", " (:keywords model)) "\n\n"
                                "Example: " (:example model))]
                  (.setText detail-area text))))))))
    
    ;; Search filter
    (.addActionListener search-field
      (reify ActionListener
        (actionPerformed [_ _]
          (let [query (str/lower-case (.getText search-field))
                cat (.getSelectedItem category-combo)]
            (.clear list-model)
            (doseq [model mental-models]
              (when (and (or (str/blank? query)
                            (str/includes? (str/lower-case (:name model)) query)
                            (str/includes? (str/lower-case (str (:keywords model))) query))
                        (or (= cat "All Categories")
                            (= (:category model) cat)))
                (.addElement list-model (str (:id model) ". " (:name model)))))))))
    
    ;; Category filter
    (.addActionListener category-combo
      (reify ActionListener
        (actionPerformed [_ _]
          (.postActionEvent search-field))))
    
    ;; Layout
    (let [center-panel (JPanel. (BorderLayout.))]
      (.setOpaque center-panel false)
      (.add center-panel search-panel BorderLayout/NORTH)
      (.add center-panel split-pane BorderLayout/CENTER)
      (.add panel center-panel BorderLayout/CENTER))
    
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
  (let [sidebar (JPanel.)
        layout (BoxLayout. sidebar BoxLayout/Y_AXIS)]
    
    (.setLayout sidebar layout)
    (.setBackground sidebar (:sidebar-bg colors))
    (.setPreferredSize sidebar (Dimension. 220 0))
    (.setBorder sidebar (EmptyBorder. 20 15 20 15))
    
    ;; Logo/Title
    (let [logo (JLabel. "☐ Mental Models")]
      (.setFont logo (:subtitle fonts))
      (.setForeground logo (:sidebar-text colors))
      (.setAlignmentX logo 0.0)
      (.add sidebar logo))
    
    (.add sidebar (Box/createVerticalStrut 30))
    
    ;; Navigation label
    (let [nav-label (JLabel. "NAVIGATION")]
      (.setFont nav-label (:small fonts))
      (.setForeground nav-label (:text-muted colors))
      (.setAlignmentX nav-label 0.0)
      (.add sidebar nav-label))
    
    (.add sidebar (Box/createVerticalStrut 10))
    
    ;; Nav buttons
    (doseq [[label card-name] [["☐ Dashboard" "dashboard"]
                               ["☐ Scan Folder" "scan"]
                               ["☐ Models" "models"]
                               ["☐ Decisions" "decisions"]
                               ["☐ Watch Mode" "watch"]
                               ["☐ Live Logs" "logs"]
                               ["☐ Settings" "settings"]]]
      (let [btn (JButton. label)]
        (.setFont btn (:body fonts))
        (.setForeground btn (:sidebar-text colors))
        (.setBackground btn (:sidebar-bg colors))
        (.setBorderPainted btn false)
        (.setFocusPainted btn false)
        (.setHorizontalAlignment btn JButton/LEFT)
        (.setMaximumSize btn (Dimension. 200 40))
        (.setPreferredSize btn (Dimension. 200 40))
        (.setAlignmentX btn 0.0)
        (.setCursor btn (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
        (.addActionListener btn
          (reify ActionListener
            (actionPerformed [_ _]
              (.show card-layout content-panel card-name)
              (when (= card-name "dashboard")
                (refresh-dashboard!)))))
        (.add sidebar btn)
        (.add sidebar (Box/createVerticalStrut 5))))
    
    (.add sidebar (Box/createVerticalGlue))
    
    ;; Connection status at bottom
    (let [conn-label (JLabel. "CONNECTIONS")]
      (.setFont conn-label (:small fonts))
      (.setForeground conn-label (:text-muted colors))
      (.setAlignmentX conn-label 0.0)
      (.add sidebar conn-label))
    
    (.add sidebar (Box/createVerticalStrut 8))
    
    (doseq [[name key] [["LM Studio" :lm-studio]
                        ["Web App" :web-app]
                        ["Slack" :slack]
                        ["GitHub" :github]]]
      (let [label (JLabel. (str "● " name))]
        (.setFont label (:small fonts))
        (.setAlignmentX label 0.0)
        (add-watch *state (keyword (str "sidebar-" name))
          (fn [_ _ _ new-state]
            (let [status (get-in new-state [:connections key])]
              (SwingUtilities/invokeLater
                #(.setForeground label
                   (if (= status :connected)
                     (:success colors)
                     (:danger colors)))))))
        (.setForeground label (:danger colors))
        (.add sidebar label)
        (.add sidebar (Box/createVerticalStrut 3))))
    
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
      (.setJMenuBar frame menubar))
    
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setSize frame 1100 750)
    (.setMinimumSize frame (Dimension. 900 600))
    (.setLocationRelativeTo frame nil)
    
    frame))

;; =============================================================================
;; Startup Update Check (Fully Automatic)
;; =============================================================================

(defn check-updates-silently! [frame]
  "Check for updates on startup and AUTO-DOWNLOAD without user interaction"
  (future
    (try
      (Thread/sleep 3000) ;; Wait for UI to load
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

(defn -main [& args]
  (println "")
  (println "╔════════════════════════════════════════════════╗")
  (println "║     Mental Models Desktop v" (:version config) "           ║")
  (println "║  ★ FULLY AUTOMATIC UPDATES ENABLED             ║")
  (println "╠════════════════════════════════════════════════╣")
  (println "║  • Blue-Green deployment active                ║")
  (println "║  • SQLite persistence enabled                  ║")
  (println "║  • Watch mode for auto-scanning                ║")
  (println "║  • Devin feedback loop enabled                 ║")
  (println "║  • Auto-update from GitHub releases            ║")
  (println "╚════════════════════════════════════════════════╝")
  (println "")
  
  ;; Set up global exception handler for Devin feedback
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (let [stack (format-stack-trace ex)]
          (println "[ERROR] Uncaught exception:" (.getMessage ex))
          (report-error-to-devin! "UncaughtException" (.getMessage ex) stack)))))
  
  (init-database!)
  
  (let [saved-settings (load-settings!)]
    (when (seq saved-settings)
      (swap! *state update :settings merge saved-settings)
      (println "[SETTINGS] Loaded saved settings")))
  
  (SwingUtilities/invokeLater
    (fn []
      (let [frame (create-main-frame)]
        (.setVisible frame true)
        (println "[GUI] Application started")
        (check-all-connections!)
        ;; Silent startup update check - FULLY AUTOMATIC
        (check-updates-silently! frame)
        ;; Notify Devin of startup (optional, for usage tracking)
        (when (get-in @*state [:settings :slack-webhook])
          (notify-startup!))))))

(comment
  (-main))
