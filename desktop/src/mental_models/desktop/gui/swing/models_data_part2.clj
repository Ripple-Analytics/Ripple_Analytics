(ns mental-models.desktop.gui.swing.models-data-part2
  "Swing App - Models Data (Part 2)"
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