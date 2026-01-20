(ns mental-models.desktop.gui.swing.models-data-part3
  "Swing App - Models Data (Part 3)"
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