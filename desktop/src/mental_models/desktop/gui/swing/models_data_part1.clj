(ns mental-models.desktop.gui.swing.models-data-part1
  "Swing App - Models Data (Part 1)"
  (:require [mental-models.desktop.gui.swing.state :refer [*state log!]]
            [mental-models.desktop.gui.swing.theme :refer [colors fonts]]
            [mental-models.desktop.gui.swing.config :refer [config]])
  (:import [javax.swing JFrame JPanel JLabel JButton JTextField JTextArea JScrollPane
                        JFileChooser JProgressBar JOptionPane SwingUtilities UIManager
                        BorderFactory JList DefaultListModel JMenuBar JMenu JMenuItem
                        JDialog Timer Box BoxLayout JComboBox ListSelectionModel JTable JSplitPane]
           [javax.swing.table DefaultTableModel]
           [javax.swing.border TitledBorder EmptyBorder]
           [java.awt BorderLayout GridLayout FlowLayout Color Font Dimension CardLayout
                     Desktop GridBagLayout GridBagConstraints Insets Cursor Graphics2D RenderingHints BasicStroke]
           [java.awt.event ActionListener WindowAdapter]
           [java.io File]
           [java.net URL HttpURLConnection URI]))

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