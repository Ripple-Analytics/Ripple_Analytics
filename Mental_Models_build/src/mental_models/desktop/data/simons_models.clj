;; Jim Simons Mental Models
;; Source: Renaissance Technologies - 66% annualized returns over 30 years
;; Date: 2026-01-20

(ns mental-models.desktop.data.simons-models
  "Jim Simons' principles from Renaissance Technologies")

(def simons-models
  [{:id 130
    :name "Beauty and Elegance as Signal"
    :category "Decision Making"
    :description "Elegant solutions and beautiful ideas signal hidden value and profound truth. Beauty in mathematics, science, and business indicates deep structural efficiency, scalability, and robustness. The most profitable trading signals at Renaissance were often the most mathematically elegant ones."
    :keywords ["elegance" "beauty" "signal" "simplicity" "structure" "simons" "renaissance" "elegant" "beautiful"]
    :failure-modes ["Mistaking Complexity for Sophistication" "Ignoring Practical Constraints" "Beauty Without Substance" "Elegance Paralysis" "Context-Free Elegance"]
    :safeguards ["Apply Occam's Razor" "Test explanatory power" "Measure complexity metrics" "Test with real conditions" "Ship minimum viable elegant solution"]}
   
   {:id 131
    :name "Data Over Intuition"
    :category "Decision Making"
    :description "Let data drive decisions rather than human intuition or expert opinion. Renaissance Technologies succeeded by removing human judgment from trading decisions entirely. Simons hired mathematicians and scientists, not Wall Street traders, because they trusted data over gut feelings."
    :keywords ["data" "intuition" "evidence" "quantitative" "empirical" "simons" "renaissance" "scientific" "measurement"]
    :failure-modes ["Data Without Context" "Overfitting to Historical Data" "Measurement Bias" "Data Quality Blindness" "Correlation-Causation Confusion"]
    :safeguards ["Out-of-sample testing" "Multiple data sources" "Understand data generation process" "Test causal hypotheses" "Regular model validation"]}
   
   {:id 132
    :name "Hire Smart People and Let Them Work"
    :category "Organizational & Institutional"
    :description "Hire the smartest people you can find, give them interesting problems, and get out of their way. Renaissance hired PhDs in mathematics, physics, and computer science—not finance people. They created an environment where brilliant minds could collaborate without bureaucracy."
    :keywords ["hiring" "talent" "autonomy" "collaboration" "smart" "simons" "renaissance" "team" "culture"]
    :failure-modes ["Hiring for Credentials Over Ability" "Micromanagement" "Bureaucratic Overhead" "Talent Hoarding" "Culture Mismatch"]
    :safeguards ["Test problem-solving ability" "Minimize hierarchy" "Protect research time" "Foster collaboration" "Align incentives with outcomes"]}
   
   {:id 133
    :name "Secrecy and Information Asymmetry"
    :category "Strategy & Competition"
    :description "Protect valuable information ruthlessly. Renaissance's edge came from proprietary algorithms and data—they maintained extreme secrecy. Employees sign strict NDAs, and the firm rarely discusses methods. Information asymmetry is a sustainable competitive advantage."
    :keywords ["secrecy" "information" "asymmetry" "competitive" "advantage" "simons" "renaissance" "proprietary" "edge"]
    :failure-modes ["Over-Secrecy Hindering Collaboration" "Secrecy Without Substance" "Information Hoarding Within Organization" "Paranoia" "Missing External Insights"]
    :safeguards ["Balance internal sharing vs external secrecy" "Identify truly proprietary information" "Build trust internally" "Monitor for leaks" "Stay connected to external developments"]}
   
   {:id 134
    :name "Continuous Improvement Through Iteration"
    :category "Systems & Processes"
    :description "Success comes from relentless, incremental improvement over time. Renaissance continuously refined their models, adding small improvements that compounded. They didn't seek breakthrough discoveries—they sought consistent, measurable improvements that accumulated."
    :keywords ["iteration" "improvement" "continuous" "incremental" "compound" "simons" "renaissance" "refinement" "optimization"]
    :failure-modes ["Premature Optimization" "Improvement Without Measurement" "Local Maximum Trap" "Change for Change's Sake" "Ignoring Diminishing Returns"]
    :safeguards ["Measure improvement rigorously" "A/B test changes" "Track cumulative gains" "Question assumptions periodically" "Know when to stop optimizing"]}
   
   {:id 135
    :name "Pattern Recognition Across Domains"
    :category "Learning & Knowledge"
    :description "Patterns discovered in one domain often apply to others. Simons applied techniques from code-breaking and speech recognition to financial markets. Renaissance hired scientists from diverse fields because they brought fresh pattern-recognition approaches."
    :keywords ["pattern" "recognition" "cross-domain" "transfer" "analogy" "simons" "renaissance" "interdisciplinary" "synthesis"]
    :failure-modes ["False Pattern Recognition" "Overgeneralization" "Domain Blindness" "Pattern Forcing" "Ignoring Domain-Specific Constraints"]
    :safeguards ["Test patterns in new domains" "Understand domain differences" "Seek disconfirming evidence" "Collaborate with domain experts" "Validate with out-of-sample data"]}
   
   {:id 136
    :name "Embrace Uncertainty with Probability"
    :category "Decision Making"
    :description "Think in probabilities, not certainties. Renaissance's models didn't predict with certainty—they identified slight statistical edges that, repeated thousands of times, generated enormous returns. Accept that you'll be wrong often, but ensure your edge compounds over many decisions."
    :keywords ["probability" "uncertainty" "edge" "statistics" "expected" "value" "simons" "renaissance" "bayesian"]
    :failure-modes ["Certainty Illusion" "Probability Neglect" "Small Sample Overconfidence" "Base Rate Neglect" "Outcome Bias"]
    :safeguards ["Track prediction accuracy" "Think in expected value" "Ensure sufficient sample size" "Update probabilities with new information" "Separate decision quality from outcomes"]}
   
   {:id 137
    :name "Systematic Over Discretionary"
    :category "Systems & Processes"
    :description "Build systems that make decisions consistently, removing human emotion and bias. Renaissance's trading is fully automated—no human decides individual trades. Systematic approaches scale better, are more consistent, and can be improved methodically."
    :keywords ["systematic" "automation" "system" "process" "consistent" "simons" "renaissance" "algorithmic" "rules"]
    :failure-modes ["Over-Automation" "System Rigidity" "Black Box Blindness" "Automation Complacency" "Edge Cases"]
    :safeguards ["Monitor system performance" "Build in circuit breakers" "Understand system logic" "Plan for edge cases" "Regular system audits"]}
   
   {:id 138
    :name "Compounding Small Edges"
    :category "Strategy & Competition"
    :description "Small advantages, consistently applied over time, compound into enormous results. Renaissance's individual trades had tiny edges—often less than 1%—but executed millions of times, these compounded to 66% annual returns. Focus on repeatable small wins, not occasional big wins."
    :keywords ["compounding" "edge" "consistency" "small" "wins" "simons" "renaissance" "accumulation" "repetition"]
    :failure-modes ["Seeking Home Runs" "Ignoring Transaction Costs" "Edge Decay Blindness" "Overtrading" "Inconsistent Application"]
    :safeguards ["Calculate true edge after costs" "Monitor edge persistence" "Ensure sufficient repetitions" "Track compound growth" "Resist temptation for bigger bets"]}
   
   {:id 139
    :name "Scientific Method in Business"
    :category "Learning & Knowledge"
    :description "Apply rigorous scientific methodology to business decisions. Form hypotheses, design experiments, collect data, analyze results, and iterate. Renaissance operated more like a research lab than a hedge fund—every trading idea was a hypothesis to be tested."
    :keywords ["scientific" "method" "hypothesis" "experiment" "test" "simons" "renaissance" "research" "empirical"]
    :failure-modes ["Confirmation Bias in Testing" "P-Hacking" "Publication Bias" "Hypothesis-Free Data Mining" "Ignoring Negative Results"]
    :safeguards ["Pre-register hypotheses" "Use proper statistical methods" "Report all results" "Replicate findings" "Seek peer review"]}])

;; Function to get all Simons models
(defn get-simons-models []
  simons-models)

;; Function to merge with existing models
(defn merge-with-existing [existing-models]
  (concat existing-models simons-models))
