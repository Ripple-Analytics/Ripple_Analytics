(ns mental-models.models-enhanced
  "Enhanced mental models with comprehensive failure modes and case studies"
  (:require [mental-models.models :as models]))

;; Enhanced Model: Second-Order Thinking
;; One of Munger's most emphasized mental models
(def second-order-thinking-enhanced
  {:name "second-order-thinking"
   :category "decision_making"
   :description "Consider not just the immediate consequences of a decision, but also the consequences of those consequences. Ask 'And then what?'"
   :key-insight "First-level thinking is simplistic and superficial. Second-order thinking is deep, complex, and convoluted."
   :application "Before making important decisions, map out 2-3 levels of consequences. Consider how others will react to your actions, and how their reactions will affect the system."
   :munger-quote "It is remarkable how much long-term advantage people like us have gotten by trying to be consistently not stupid, instead of trying to be very intelligent."
   :failure-modes
   [{:name "stopping-at-first-order"
     :severity "high"
     :description "Making decisions based only on immediate, obvious consequences without considering downstream effects."
     :mechanism "Cognitive ease leads to accepting the first plausible answer. The brain conserves energy by avoiding deeper analysis."
     :psychological-root "System 1 thinking (fast, intuitive) dominates over System 2 (slow, analytical). Immediate gratification bias."
     :evolutionary-origin "In ancestral environments, immediate threats required fast decisions. Deep analysis was often unnecessary for survival."
     :signals
     ["Decisions made quickly without extended deliberation"
      "Focus on immediate outcomes only"
      "Surprise when 'unexpected' consequences emerge"
      "Lack of scenario planning"
      "No consideration of how others will respond"
      "Failure to ask 'And then what?'"
      "Ignoring feedback loops"
      "Short time horizons in planning"]
     :case-studies
     [{:name "Cobra Effect - British India"
       :year 1900
       :entity "British Colonial Government in India"
       :description "To reduce cobra population in Delhi, British offered bounty for dead cobras. People began breeding cobras for income. When program ended, breeders released cobras, increasing population."
       :quantitative-impact "Cobra population increased significantly above baseline after program termination"
       :root-cause "First-order thinking: bounty reduces cobras. Missed second-order: incentive creates cobra breeding industry"
       :should-have-done "Consider incentive effects, monitor breeding, require proof of wild capture, phase out gradually"
       :sources ["Horst Siebert, Der Kobra-Effekt (2001)", "Economic Policy Institute studies"]}
      {:name "Prohibition - United States"
       :year 1920
       :entity "United States Government"
       :description "18th Amendment banned alcohol to reduce crime and social problems. Created massive black market, organized crime boom, violence, and corruption."
       :quantitative-impact "Homicide rate increased 78% during Prohibition. Organized crime revenue estimated $2B annually (1920s dollars)"
       :root-cause "First-order: ban alcohol, reduce problems. Missed second-order: black markets, organized crime, enforcement costs"
       :should-have-done "Model black market effects, consider enforcement feasibility, study prior prohibition attempts"
       :sources ["Thornton, M. (1991). Alcohol Prohibition Was a Failure. Cato Institute", "FBI Uniform Crime Reports"]}
      {:name "Streisand Effect - Barbara Streisand"
       :year 2003
       :entity "Barbara Streisand"
       :description "Streisand sued to suppress aerial photo of her mansion. Lawsuit drew massive attention to photo, which had only 6 views before lawsuit."
       :quantitative-impact "Photo viewed 420,000+ times in one month after lawsuit. Became permanent internet meme."
       :root-cause "First-order: lawsuit removes photo. Missed second-order: lawsuit creates news story, amplifies awareness"
       :should-have-done "Ignore photo, negotiate quietly, consider how media covers lawsuits by celebrities"
       :sources ["Masnick, M. (2005). Techdirt coverage", "Streisand v. Adelman case files"]}]
     :quantitative-thresholds
     [{:metric "Decision Time Horizon"
       :warning-threshold "< 1 year"
       :critical-threshold "< 3 months"
       :measurement "Time span considered in decision analysis"
       :frequency "Per major decision"
       :example "Considering only Q1 results when decision affects 3-year product cycle"}
      {:metric "Consequence Levels Analyzed"
       :warning-threshold "< 2 levels"
       :critical-threshold "1 level only"
       :measurement "Number of 'And then what?' iterations performed"
       :frequency "Per strategic decision"
       :example "Launching product without considering competitive response (2nd order) or market saturation (3rd order)"}
      {:metric "Stakeholder Reactions Modeled"
       :warning-threshold "< 3 stakeholder groups"
       :critical-threshold "< 2 stakeholder groups"
       :measurement "Number of different parties whose reactions are anticipated"
       :frequency "Per decision affecting multiple parties"
       :example "Policy change considering only employees, not customers, suppliers, regulators"}]
     :environmental-triggers
     ["Time pressure and urgency"
      "Competitive pressure to act fast"
      "Overconfidence in understanding the system"
      "Lack of historical knowledge"
      "Reward systems focused on short-term results"
      "Absence of diverse perspectives"]
     :structural-safeguards
     ["Mandatory 'And then what?' analysis for major decisions"
      "Pre-mortem exercises imagining failure scenarios"
      "Red team reviews of major initiatives"
      "Decision journals tracking predicted vs actual outcomes"
      "Cooling-off periods before final decisions"
      "Scenario planning requirements"
      "Incentive alignment with long-term outcomes"
      "Historical case study reviews"]
     :cognitive-safeguards
     ["Inversion: What would make this fail?"
      "Chesterton's Fence: Understand why current state exists"
      "Map feedback loops and system dynamics"
      "Consider incentive effects on all parties"
      "Ask 'Who benefits?' at each consequence level"
      "Use decision trees for complex choices"]
     :social-safeguards
     ["Diverse review teams with different perspectives"
      "Devil's advocate role in decision meetings"
      "External expert consultation"
      "Stakeholder impact analysis"
      "Public comment periods for major changes"]
     :recovery-protocols
     [{:step 1
       :action "Immediately assess actual second-order effects emerging"
       :responsible "Decision owner + analysis team"
       :timeline "Within 48 hours of recognizing issue"
       :success-criteria "Complete map of unintended consequences"
       :escalation "If effects threaten core operations, escalate to executive team"}
      {:step 2
       :action "Implement damage control for worst effects"
       :responsible "Operations team"
       :timeline "Within 1 week"
       :success-criteria "Stabilization of critical negative effects"
       :escalation "If damage control fails, consider full reversal"}
      {:step 3
       :action "Conduct root cause analysis of analytical failure"
       :responsible "Strategy team"
       :timeline "Within 2 weeks"
       :success-criteria "Documented analysis of what was missed and why"
       :escalation "If systemic analytical gaps found, overhaul decision process"}
      {:step 4
       :action "Update decision frameworks and train team"
       :responsible "Learning & development"
       :timeline "Within 1 month"
       :success-criteria "Improved decision process preventing similar failures"
       :escalation "If training insufficient, bring in external expertise"}]
     :lollapalooza-risk 0.85
     :amplifying-biases ["confirmation-bias" "optimism-bias" "planning-fallacy" "availability-heuristic"]
     :mitigating-models ["inversion" "systems-thinking" "scenario-analysis" "pre-mortem"]}
    
    {:name "ignoring-feedback-loops"
     :severity "high"
     :description "Failing to recognize that actions create reactions that feed back into the system, creating virtuous or vicious cycles."
     :mechanism "Linear thinking assumes one-way causation. Reality involves circular causation where effects become causes."
     :psychological-root "Human cognition evolved for linear cause-effect. Circular causation is cognitively demanding."
     :evolutionary-origin "Most ancestral problems involved simple causation (throw spear → hit animal). Complex system dynamics were rare."
     :signals
     ["Surprise at exponential growth or collapse"
      "Failure to anticipate tipping points"
      "Treating symptoms rather than root causes"
      "Short-term fixes that worsen long-term problems"
      "Inability to explain why problems keep recurring"
      "Missing reinforcing or balancing loops"]
     :case-studies
     [{:name "Antibiotic Resistance Crisis"
       :year "1950-present"
       :entity "Global healthcare system"
       :description "Overuse of antibiotics to treat infections created selection pressure for resistant bacteria, making antibiotics less effective over time."
       :quantitative-impact "700,000 deaths annually from drug-resistant infections. Projected 10M deaths/year by 2050 if unchecked."
       :root-cause "Ignored feedback loop: antibiotic use → bacterial resistance → more antibiotic use → more resistance"
       :should-have-done "Antibiotic stewardship programs, reserve drugs for severe cases, invest in alternatives, monitor resistance"
       :sources ["O'Neill Report (2016)", "WHO Global Action Plan on AMR", "CDC Antibiotic Resistance Threats Report"]}
      {:name "Subprime Mortgage Crisis"
       :year "2007-2008"
       :entity "US Financial System"
       :description "Easy lending → rising home prices → more lending (prices rising!) → more speculation → bubble → crash"
       :quantitative-impact "$7.4T in stock wealth destroyed. 8.7M jobs lost. 10M foreclosures."
       :root-cause "Ignored positive feedback loop amplifying risk. Assumed prices would keep rising."
       :should-have-done "Recognize bubble dynamics, tighten lending standards, stress test for price declines"
       :sources ["Financial Crisis Inquiry Report (2011)", "Federal Reserve studies", "IMF World Economic Outlook"]}
      {:name "YouTube Radicalization Pipeline"
       :year "2015-2020"
       :entity "YouTube / Alphabet Inc"
       :description "Recommendation algorithm optimized for watch time → recommended increasingly extreme content → users watched more → algorithm learned extreme content works → more extreme recommendations"
       :quantitative-impact "70% of YouTube watch time from recommendations. Studies showed progression to extremist content in 5-10 video steps."
       :root-cause "Optimization for engagement created feedback loop toward extreme content"
       :should-have-done "Optimize for user well-being not just watch time, add friction for extreme content, diversify recommendations"
       :sources ["Ribeiro et al. (2020) Auditing Radicalization Pathways", "Tufekci (2018) NYT Op-Ed", "YouTube Transparency Reports"]}]
     :quantitative-thresholds
     [{:metric "Feedback Loop Identification Rate"
       :warning-threshold "< 50% of major feedback loops identified"
       :critical-threshold "< 25% identified"
       :measurement "Percentage of system feedback loops documented in decision analysis"
       :frequency "Per system change"
       :example "Identified 2 of 8 feedback loops in pricing strategy change"}
      {:metric "Time to Recognize Unintended Amplification"
       :warning-threshold "> 6 months"
       :critical-threshold "> 12 months"
       :measurement "Time from intervention to recognizing amplifying feedback"
       :frequency "Per intervention"
       :example "Took 18 months to realize sales incentive was causing customer churn"}]
     :environmental-triggers
     ["Complex systems with many interacting parts"
      "Long time delays between action and feedback"
      "Non-linear relationships"
      "Multiple stakeholders with different incentives"
      "Optimization for single metric"]
     :structural-safeguards
     ["System dynamics modeling for major initiatives"
      "Causal loop diagrams required for complex decisions"
      "Monitoring systems for early warning signs"
      "Regular system audits"
      "Diverse metrics (not single KPI optimization)"
      "Scenario testing with feedback loops"
      "Gradual rollouts with monitoring"
      "Kill switches for runaway processes"]
     :cognitive-safeguards
     ["Map all stakeholder incentives"
      "Identify reinforcing vs balancing loops"
      "Look for delays between action and consequence"
      "Consider what happens at scale"
      "Ask 'What happens when everyone does this?'"
      "Study historical examples of similar systems"]
     :social-safeguards
     ["Cross-functional review teams"
      "External systems thinking experts"
      "Stakeholder feedback mechanisms"
      "Transparency and public scrutiny"
      "Regulatory oversight for systemic risks"]
     :recovery-protocols
     [{:step 1
       :action "Identify the feedback loop causing problems"
       :responsible "Systems analysis team"
       :timeline "Within 1 week"
       :success-criteria "Documented causal loop diagram"
       :escalation "If loop unclear, bring in external systems thinking expert"}
      {:step 2
       :action "Determine intervention points to break or reverse loop"
       :responsible "Strategy team"
       :timeline "Within 2 weeks"
       :success-criteria "3+ intervention options with predicted effects"
       :escalation "If no good interventions found, consider full system redesign"}
      {:step 3
       :action "Implement intervention with careful monitoring"
       :responsible "Operations team"
       :timeline "Within 1 month"
       :success-criteria "Feedback loop weakening or reversing"
       :escalation "If intervention ineffective, try alternative approach"}
      {:step 4
       :action "Build ongoing monitoring for similar feedback loops"
       :responsible "Risk management"
       :timeline "Within 2 months"
       :success-criteria "Early warning system operational"
       :escalation "If monitoring insufficient, invest in better analytics"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["optimism-bias" "confirmation-bias" "exponential-growth-bias"]
     :mitigating-models ["systems-thinking" "inversion" "margin-of-safety" "scenario-analysis"]}
    
    ;; Three more failure modes for second-order thinking...
    {:name "underestimating-time-delays"
     :severity "medium"
     :description "Failing to account for time lags between actions and consequences, leading to overcorrection or premature abandonment."
     :signals ["Frequent strategy changes" "Impatience with initiatives" "Overcorrection" "Boom-bust cycles"]
     :safeguards ["Patience protocols" "Time-series analysis" "Leading indicators" "Commitment devices"]}
    
    {:name "missing-non-linear-effects"
     :severity "high"
     :description "Assuming linear relationships when reality involves thresholds, tipping points, and exponential changes."
     :signals ["Surprise at sudden changes" "Failure to anticipate tipping points" "Linear extrapolation errors"]
     :safeguards ["Non-linear modeling" "Threshold identification" "Sensitivity analysis" "Phase transition awareness"]}
    
    {:name "ignoring-strategic-interaction"
     :severity "high"
     :description "Failing to consider how other intelligent agents will respond to your actions (game theory failure)."
     :signals ["Surprise at competitive responses" "Price wars" "Arms races" "Tragedy of commons scenarios"]
     :safeguards ["Game theory analysis" "Competitive war-gaming" "Incentive analysis" "Equilibrium thinking"]}]})

;; Register the enhanced model
(defn register-enhanced-models! []
  (models/register-model second-order-thinking-enhanced))
