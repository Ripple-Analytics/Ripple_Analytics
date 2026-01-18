;; Iteration 13: High-Magnitude Enhancement
;; Adding Probabilistic Thinking - One of Munger's Core Models

(def probabilistic-thinking-enhanced
  {:name "probabilistic-thinking"
   :category "decision_making"
   :description "Think in terms of probabilities and expected values rather than certainties. Assign numerical probabilities to outcomes and calculate expected values to make better decisions under uncertainty."
   :key-insight "The world is probabilistic, not deterministic. Better decisions come from thinking in bets and expected values, not binary outcomes."
   :application "For important decisions, assign probabilities to different outcomes (e.g., 60% chance of success, 30% moderate success, 10% failure). Calculate expected values. Update probabilities as new information arrives (Bayesian updating)."
   :munger-quote "Take a simple idea and take it seriously. The idea that it is hard to find good investments, that it's hard to find good opportunities, is a very, very powerful idea."
   :failure-modes
   [{:name "binary-thinking"
     :severity "high"
     :description "Thinking in terms of 'will happen' or 'won't happen' rather than probability distributions. Treating uncertain outcomes as certain."
     :mechanism "The brain craves certainty and simplifies complex probability distributions into binary outcomes. Cognitive ease prefers yes/no over probability ranges."
     :psychological-root "Intolerance of ambiguity. Need for cognitive closure. System 1 thinking prefers definite answers over probability distributions."
     :evolutionary-origin "Ancestral environments required fast binary decisions (fight/flee, safe/dangerous). Probability thinking is modern cognitive skill."
     :signals
     ["Using absolute language: 'will happen', 'definitely', 'impossible', 'certain'"
      "No probability estimates in planning documents"
      "Surprise when 'certain' outcomes don't materialize"
      "All-or-nothing thinking in strategy"
      "No contingency plans for alternative outcomes"
      "Treating forecasts as facts"
      "Ignoring base rates and historical frequencies"
      "No confidence intervals on estimates"
      "Failure to distinguish between 60% and 90% confidence"
      "Planning assumes single scenario"]
     :case-studies
     [{:name "Long-Term Capital Management Collapse"
       :year 1998
       :entity "Long-Term Capital Management (LTCM)"
       :description "LTCM's models treated extreme market moves as 'impossible' (10-sigma events). When Russian default triggered correlated moves across markets, the 'impossible' happened and LTCM lost $4.6B in months."
       :quantitative-impact "$4.6B loss, $3.6B bailout required, near-collapse of global financial system"
       :root-cause "Binary thinking: extreme events treated as 'won't happen' rather than 'low probability but catastrophic if they do'"
       :should-have-done "Assign non-zero probabilities to tail events, stress test for correlated moves, maintain margin of safety"
       :sources ["Lowenstein, R. (2000). When Genius Failed", "Federal Reserve Bank of New York reports"]}
      {:name "Fukushima Nuclear Disaster"
       :year 2011
       :entity "Tokyo Electric Power Company (TEPCO)"
       :description "Fukushima plant designed for maximum 5.7m tsunami. Historical records showed 10m+ tsunamis occurred every few centuries. Treated large tsunami as 'won't happen' rather than 'low probability, catastrophic consequence'."
       :quantitative-impact "3 reactor meltdowns, $200B+ in damages, 154,000 evacuated, ongoing contamination"
       :root-cause "Binary thinking: large tsunami treated as impossible rather than low-probability high-impact event"
       :should-have-done "Probabilistic risk assessment, design for 1-in-1000 year events, backup power at higher elevations"
       :sources ["National Diet of Japan Fukushima Nuclear Accident Independent Investigation Commission Report", "IAEA reports"]}
      {:name "2008 Financial Crisis - AAA CDO Ratings"
       :year 2008
       :entity "Credit Rating Agencies (Moody's, S&P, Fitch)"
       :description "Rating agencies assigned AAA ratings (implying <0.01% default probability) to mortgage-backed securities that had 20-30% default rates. Binary thinking: 'housing prices won't fall nationally'."
       :quantitative-impact "$7.4T in stock market losses, 8.7M jobs lost, Great Recession"
       :root-cause "Binary thinking: national housing price decline treated as 'won't happen' rather than low-probability scenario requiring stress testing"
       :should-have-done "Assign non-zero probability to housing decline, stress test CDOs under various scenarios, account for correlation risk"
       :sources ["Financial Crisis Inquiry Commission Report (2011)", "SEC investigations"]}
      {:name "Challenger Space Shuttle Disaster"
       :year 1986
       :entity "NASA and Morton Thiokol"
       :description "Engineers warned O-rings might fail in cold weather. Management treated it as 'will be fine' rather than probabilistic risk. At 36°F launch temperature, O-ring failure probability was estimated 13% but treated as 'won't happen'."
       :quantitative-impact "7 astronauts killed, $4.5B shuttle lost, 32-month program suspension"
       :root-cause "Binary thinking: O-ring failure treated as 'won't happen' rather than 'significant probability given temperature'"
       :should-have-done "Quantify failure probability vs temperature, establish hard go/no-go thresholds, delay launch"
       :sources ["Rogers Commission Report (1986)", "Feynman, R. (1988). What Do You Care What Other People Think?"]}
      {:name "Deepwater Horizon Oil Spill"
       :year 2010
       :entity "BP, Transocean, Halliburton"
       :description "Multiple warning signs of blowout risk (failed pressure tests, gas kicks) treated as 'will be fine' rather than probabilistic indicators of catastrophic failure."
       :quantitative-impact "11 workers killed, 4.9M barrels spilled, $65B in costs and fines, Gulf ecosystem damage"
       :root-cause "Binary thinking: warning signs treated as 'won't lead to disaster' rather than 'elevated probability of catastrophic failure'"
       :should-have-done "Probabilistic risk assessment, treat failed tests as red flags, halt operations until resolved"
       :sources ["National Commission on the BP Deepwater Horizon Oil Spill Report (2011)", "DOJ investigations"]}]
     :quantitative-thresholds
     [{:metric "Probability Language Usage"
       :warning-threshold "< 30% of planning documents include probability estimates"
       :critical-threshold "< 10% include probability estimates"
       :measurement "Percentage of strategic documents with explicit probability assignments"
       :frequency "Quarterly review"
       :example "Business plan says 'will achieve $10M revenue' instead of '60% probability of $8-12M revenue'"}
      {:metric "Scenario Planning Breadth"
       :warning-threshold "< 3 scenarios modeled"
       :critical-threshold "Only 1 scenario (base case)"
       :measurement "Number of distinct outcome scenarios with assigned probabilities"
       :frequency "Per major decision"
       :example "Product launch plan has only 'success' scenario, no moderate success or failure scenarios"}
      {:metric "Confidence Calibration"
       :warning-threshold "Actual outcomes fall outside predicted ranges > 40% of time"
       :critical-threshold "> 60% of time"
       :measurement "Frequency that reality falls outside confidence intervals"
       :frequency "Annual review of past predictions"
       :example "Predicted 'definitely will close deal' but only 50% of 'definite' deals close"}
      {:metric "Tail Event Consideration"
       :warning-threshold "No analysis of < 5% probability events"
       :critical-threshold "No analysis of < 20% probability events"
       :measurement "Whether low-probability high-impact events are modeled"
       :frequency "Per risk assessment"
       :example "No stress testing for market crash despite 10-20% probability in next 5 years"}]
     :environmental-triggers
     ["High pressure for definitive answers"
      "Overconfidence from past successes"
      "Lack of statistical training"
      "Culture that punishes uncertainty"
      "Incentives for confident predictions"
      "Absence of probabilistic thinking norms"
      "Time pressure forcing quick decisions"
      "Complexity that makes probability estimation difficult"]
     :structural-safeguards
     ["Mandatory probability estimates for major decisions"
      "Calibration training for forecasters"
      "Track record of predictions vs outcomes"
      "Reward accurate probability estimates, not confident predictions"
      "Pre-mortem exercises for low-probability disasters"
      "Scenario planning requirements (base, optimistic, pessimistic)"
      "Monte Carlo simulations for complex decisions"
      "Red team challenges to probability estimates"
      "Decision journals with probability assignments"
      "Brier score tracking for forecast accuracy"]
     :cognitive-safeguards
     ["Ask: What's the probability distribution, not the single outcome?"
      "Use reference class forecasting (base rates)"
      "Bayesian updating as new information arrives"
      "Expected value calculations for major bets"
      "Kelly Criterion for sizing bets/investments"
      "Distinguish between confidence and probability"
      "Consider both probability and magnitude of outcomes"
      "Think in bets, not certainties"]
     :social-safeguards
     ["Prediction markets for internal forecasts"
      "Superforecaster teams for critical decisions"
      "External calibration consultants"
      "Peer review of probability estimates"
      "Anonymous forecasting to reduce groupthink"
      "Diverse perspectives in probability estimation"]
     :recovery-protocols
     [{:step 1
       :action "Acknowledge the binary thinking error and its consequences"
       :responsible "Decision owner"
       :timeline "Immediately upon recognizing issue"
       :success-criteria "Clear documentation of what was treated as certain vs actual probability"
       :escalation "If consequences are severe, escalate to executive team"}
      {:step 2
       :action "Conduct probabilistic post-mortem"
       :responsible "Risk management team"
       :timeline "Within 1 week"
       :success-criteria "Retrospective probability distribution showing what should have been estimated"
       :escalation "If systemic pattern found, audit all major decisions"}
      {:step 3
       :action "Implement probabilistic thinking training"
       :responsible "Learning & development"
       :timeline "Within 1 month"
       :success-criteria "All decision-makers trained in probability estimation and calibration"
       :escalation "If resistance to training, mandate from executive level"}
      {:step 4
       :action "Establish probabilistic decision framework"
       :responsible "Strategy team"
       :timeline "Within 2 months"
       :success-criteria "All major decisions require probability estimates and expected value calculations"
       :escalation "If framework not adopted, tie to performance reviews"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["overconfidence-bias" "confirmation-bias" "optimism-bias" "planning-fallacy" "availability-heuristic" "narrative-fallacy"]
     :mitigating-models ["base-rate-neglect" "expected-value" "bayesian-updating" "margin-of-safety" "pre-mortem"]}
    
    {:name "ignoring-base-rates"
     :severity "high"
     :description "Focusing on specific case details while ignoring statistical base rates and historical frequencies. Kahneman's 'base rate neglect'."
     :mechanism "Vivid specific information overwhelms abstract statistical information. The brain weights narrative over numbers."
     :psychological-root "Representativeness heuristic. Specific details feel more relevant than general statistics."
     :evolutionary-origin "Ancestral environments had limited data. Specific observations were more useful than statistics."
     :signals
     ["Ignoring historical success/failure rates"
      "Overweighting unique features of current situation"
      "Believing 'this time is different'"
      "No reference class forecasting"
      "Ignoring industry benchmarks"
      "Focusing on story over statistics"
      "Dismissing relevant comparison cases"
      "No analysis of similar past situations"]
     :case-studies
     [{:name "Startup Success Rate Ignorance"
       :year "Ongoing"
       :entity "New entrepreneurs"
       :description "Base rate: 90% of startups fail within 5 years. Most entrepreneurs believe they'll succeed despite this base rate, focusing on their unique idea rather than statistical reality."
       :quantitative-impact "90% failure rate, billions in lost capital annually"
       :root-cause "Ignoring base rate (90% fail) in favor of specific case details (my idea is different)"
       :should-have-done "Start with base rate, adjust based on specific advantages, require evidence of differentiation"
       :sources ["CB Insights Startup Failure Reports", "Gage, D. (2012). The Venture Capital Secret: 3 Out of 4 Start-Ups Fail. WSJ"]}
      {:name "Mergers & Acquisitions Failure Rate"
       :year "Ongoing"
       :entity "Corporate acquirers"
       :description "Base rate: 70-90% of M&A deals fail to create value. Companies continue pursuing M&A despite this base rate, believing their specific deal is different."
       :quantitative-impact "Trillions in destroyed shareholder value"
       :root-cause "Ignoring base rate (70-90% fail) in favor of specific synergy stories"
       :should-have-done "Start with base rate, require exceptional evidence for success, demand higher margin of safety"
       :sources ["Christensen, C. et al. (2011). The Big Idea: The New M&A Playbook. HBR", "KPMG M&A studies"]}]
     :quantitative-thresholds
     [{:metric "Base Rate Reference"
       :warning-threshold "< 50% of forecasts reference base rates"
       :critical-threshold "< 20% reference base rates"
       :measurement "Percentage of predictions that start with historical base rates"
       :frequency "Per forecast review"
       :example "Predicting product success without referencing category success rates"}]
     :environmental-triggers
     ["Overconfidence in unique situation"
      "Compelling narrative about specific case"
      "Lack of historical data access"
      "Incentives to be optimistic"
      "Pressure to differentiate"]
     :structural-safeguards
     ["Mandatory reference class forecasting"
      "Base rate database for common decisions"
      "Require explicit base rate adjustment justification"
      "Outside view before inside view"]
     :cognitive-safeguards
     ["Start with base rate, then adjust"
      "Ask: What happened in similar situations?"
      "Demand evidence for 'this time is different'"
      "Use Kahneman's outside view"]
     :social-safeguards
     ["External experts provide base rates"
      "Historical case study reviews"
      "Statistical consultants on major decisions"]
     :recovery-protocols
     [{:step 1
       :action "Identify what base rate was ignored"
       :responsible "Analysis team"
       :timeline "Within 48 hours"
       :success-criteria "Clear documentation of relevant base rate"
       :escalation "If pattern of ignoring base rates, systemic review"}
      {:step 2
       :action "Recalculate decision with base rate included"
       :responsible "Strategy team"
       :timeline "Within 1 week"
       :success-criteria "Updated probability estimate starting from base rate"
       :escalation "If decision should change, escalate to decision-maker"}
      {:step 3
       :action "Implement base rate forecasting training"
       :responsible "Learning & development"
       :timeline "Within 2 weeks"
       :success-criteria "Team trained in reference class forecasting"
       :escalation "If resistance, mandate from executive level"}
      {:step 4
       :action "Establish base rate database"
       :responsible "Data team"
       :timeline "Within 1 month"
       :success-criteria "Accessible database of base rates for common decisions"
       :escalation "If not adopted, integrate into decision templates"}]
     :lollapalooza-risk 0.85
     :amplifying-biases ["overconfidence-bias" "optimism-bias" "narrative-fallacy" "availability-heuristic"]
     :mitigating-models ["reference-class-forecasting" "regression-to-mean" "statistical-thinking" "outside-view"]}
    
    ;; Additional 3 failure modes for Probabilistic Thinking
    {:name "poor-probability-calibration"
     :severity "medium-high"
     :description "Systematic over-confidence or under-confidence in probability estimates. When you say '90% confident', you're right only 60% of the time."
     :mechanism "Humans are poorly calibrated forecasters by default. Overconfidence is common for hard questions, underconfidence for easy ones."
     :psychological-root "Overconfidence bias. Dunning-Kruger effect. Lack of feedback on probability estimates."
     :signals
     ["Actual outcomes don't match confidence levels"
      "No tracking of prediction accuracy"
      "Overuse of extreme probabilities (0%, 100%)"
      "Underuse of moderate probabilities (40-60%)"
      "Surprise at outcomes despite 'high confidence'"
      "No calibration training"]
     :case-studies
     [{:name "Expert Forecaster Calibration Studies"
       :year "1980s-present"
       :entity "Political and economic experts"
       :description "Tetlock's studies showed expert forecasters were poorly calibrated. When they said 80% confident, they were right only 45% of the time."
       :quantitative-impact "Expert forecasts barely better than chance"
       :root-cause "No feedback loops, no calibration training, overconfidence"
       :should-have-done "Track predictions vs outcomes, calibration training, Brier score feedback"
       :sources ["Tetlock, P. (2005). Expert Political Judgment", "Tetlock & Gardner (2015). Superforecasting"]}]
     :quantitative-thresholds
     [{:metric "Calibration Score"
       :warning-threshold "Brier score > 0.3"
       :critical-threshold "Brier score > 0.5"
       :measurement "Brier score comparing predicted probabilities to actual outcomes"
       :frequency "Quarterly"
       :example "Predicted 80% probability for 10 events, only 4 occurred (Brier score 0.48)"}]
     :structural-safeguards
     ["Calibration training programs"
      "Track all predictions vs outcomes"
      "Brier score feedback"
      "Superforecaster mentoring"
      "Prediction markets"]
     :cognitive-safeguards
     ["Use frequency formats (7 out of 10) not percentages"
      "Consider reference class"
      "Avoid extreme probabilities unless justified"
      "Update beliefs with new evidence"]
     :recovery-protocols
     [{:step 1
       :action "Calculate calibration metrics"
       :responsible "Analytics team"
       :timeline "Within 1 week"
       :success-criteria "Brier scores for all forecasters"
       :escalation "If scores poor, mandatory training"}
      {:step 2
       :action "Implement calibration training"
       :responsible "Learning & development"
       :timeline "Within 2 weeks"
       :success-criteria "All forecasters complete calibration exercises"
       :escalation "Tie to performance reviews"}
      {:step 3
       :action "Establish ongoing tracking"
       :responsible "Data team"
       :timeline "Within 1 month"
       :success-criteria "Automated tracking of predictions vs outcomes"
       :escalation "If not used, mandate in decision process"}]
     :lollapalooza-risk 0.75
     :amplifying-biases ["overconfidence-bias" "confirmation-bias" "hindsight-bias"]
     :mitigating-models ["calibration-training" "prediction-markets" "brier-scoring" "superforecasting"]}
    
    {:name "expected-value-neglect"
     :severity "medium-high"
     :description "Focusing on probability alone without considering magnitude of outcomes. A 1% chance of $1B is more valuable than 90% chance of $1M."
     :mechanism "The brain focuses on likelihood, neglecting to multiply by outcome magnitude. Expected value = Probability × Magnitude."
     :psychological-root "Probability weighting function: humans overweight small probabilities and underweight large ones."
     :signals
     ["Avoiding low-probability high-payoff opportunities"
      "Pursuing high-probability low-payoff opportunities"
      "No expected value calculations"
      "Focus on 'likely' rather than 'expected value'"
      "Ignoring asymmetric risk/reward"
      "No portfolio thinking"]
     :case-studies
     [{:name "Venture Capital Model"
       :year "Ongoing"
       :entity "Successful VC firms"
       :description "VC model: invest in 20 startups, 15 fail (75% failure rate), 4 return 2-3x, 1 returns 100x+. Expected value is highly positive despite high failure rate."
       :quantitative-impact "Top VC firms return 5-10x+ despite 75% failure rate"
       :root-cause "Expected value thinking: 5% chance of 100x return = 5x expected value, far exceeding 75% chance of 0x"
       :should-have-done "Calculate expected values, not just probabilities. Portfolio approach."
       :sources ["Kaplan, S. & Schoar, A. (2005). Private Equity Performance. Journal of Finance", "Horsley Bridge data"]}]
     :quantitative-thresholds
     [{:metric "Expected Value Analysis"
       :warning-threshold "< 50% of major decisions include EV calculations"
       :critical-threshold "< 20% include EV calculations"
       :measurement "Percentage of decisions with explicit expected value analysis"
       :frequency "Quarterly review"
       :example "Choosing Project A (90% chance, $1M gain) over Project B (10% chance, $20M gain) without calculating EVs"}]
     :structural-safeguards
     ["Mandatory expected value calculations"
      "Portfolio thinking for multiple bets"
      "Kelly Criterion for bet sizing"
      "Asymmetric risk/reward analysis"]
     :cognitive-safeguards
     ["Calculate EV = Probability × Magnitude"
      "Consider both probability and payoff"
      "Think in portfolios, not single bets"
      "Seek asymmetric opportunities"]
     :recovery-protocols
     [{:step 1
       :action "Calculate expected values retrospectively"
       :responsible "Finance team"
       :timeline "Within 1 week"
       :success-criteria "EV analysis of missed opportunity"
       :escalation "If pattern found, mandate EV analysis"}
      {:step 2
       :action "Train team in expected value thinking"
       :responsible "Learning & development"
       :timeline "Within 2 weeks"
       :success-criteria "All decision-makers trained in EV calculations"
       :escalation "If not adopted, integrate into decision templates"}]
     :lollapalooza-risk 0.80
     :amplifying-biases ["loss-aversion" "probability-weighting" "risk-aversion"]
     :mitigating-models ["expected-value" "kelly-criterion" "asymmetric-risk" "portfolio-theory"]}
    
    {:name "static-probabilities"
     :severity "medium"
     :description "Failing to update probability estimates as new information arrives. Not using Bayesian updating."
     :mechanism "Initial probability estimate becomes anchor. New information is insufficiently weighted. Confirmation bias reinforces initial estimate."
     :psychological-root "Anchoring bias. Confirmation bias. Cognitive ease of maintaining existing beliefs."
     :signals
     ["Probability estimates don't change despite new evidence"
      "Ignoring disconfirming information"
      "No formal updating process"
      "Treating initial estimate as final"
      "Surprise when outcomes differ from initial estimate"
      "No Bayesian reasoning"]
     :case-studies
     [{:name "COVID-19 Probability Updates"
       :year 2020
       :entity "Various governments and organizations"
       :description "Many organizations failed to update pandemic probability estimates as evidence accumulated in January-February 2020. Initial 'low risk' assessments persisted despite mounting evidence."
       :quantitative-impact "Delayed responses, millions of deaths, trillions in economic damage"
       :root-cause "Static probabilities: initial 'low probability' estimate not updated despite new evidence"
       :should-have-done "Bayesian updating as evidence accumulated, trigger points for action"
       :sources ["WHO timeline", "Government response analyses", "Taleb, N. (2020). The Pandemic Isn't a Black Swan"]}]
     :quantitative-thresholds
     [{:metric "Probability Update Frequency"
       :warning-threshold "< 1 update per month for ongoing situations"
       :critical-threshold "No updates despite new information"
       :measurement "Frequency of formal probability estimate updates"
       :frequency "Per ongoing decision"
       :example "Market entry probability estimate from 6 months ago not updated despite competitive landscape changes"}]
     :structural-safeguards
     ["Scheduled probability review meetings"
      "Bayesian updating frameworks"
      "Trigger points for reassessment"
      "Red team challenges to stale estimates"]
     :cognitive-safeguards
     ["Bayesian updating: P(H|E) = P(E|H) × P(H) / P(E)"
      "Ask: What would change my mind?"
      "Actively seek disconfirming evidence"
      "Treat probabilities as dynamic, not static"]
     :recovery-protocols
     [{:step 1
       :action "Identify what new information was ignored"
       :responsible "Analysis team"
       :timeline "Within 48 hours"
       :success-criteria "Documentation of ignored evidence"
       :escalation "If systemic, audit all major estimates"}
      {:step 2
       :action "Update probability with Bayesian reasoning"
       :responsible "Strategy team"
       :timeline "Within 1 week"
       :success-criteria "Updated probability incorporating new evidence"
       :escalation "If decision should change, escalate immediately"}
      {:step 3
       :action "Implement dynamic updating process"
       :responsible "Process team"
       :timeline "Within 2 weeks"
       :success-criteria "Scheduled reviews and trigger points for updates"
       :escalation "If not adopted, mandate from executive level"}]
     :lollapalooza-risk 0.70
     :amplifying-biases ["anchoring-bias" "confirmation-bias" "status-quo-bias"]
     :mitigating-models ["bayesian-updating" "red-team-review" "pre-mortem" "scenario-planning"]}]})

;; Register the enhanced model
(models/register-model probabilistic-thinking-enhanced)
