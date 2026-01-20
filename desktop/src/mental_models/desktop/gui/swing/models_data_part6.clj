(ns mental-models.desktop.gui.swing.models-data-part6
  "Swing App - Models Data (Part 6)"
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