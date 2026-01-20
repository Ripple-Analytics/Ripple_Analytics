(ns mental-models.desktop.gui.swing.models-data-part4
  "Swing App - Models Data (Part 4)"
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