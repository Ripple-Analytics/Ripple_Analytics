(ns mental-models.desktop.gui.swing.models-data-part5
  "Swing App - Models Data (Part 5)"
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