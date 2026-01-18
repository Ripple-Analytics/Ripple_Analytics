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


;; Enhanced Model: Inversion
;; Munger's most famous principle: "Invert, always invert"
(def inversion-enhanced
  {:name "inversion"
   :category "decision_making"
   :description "Instead of thinking about how to achieve success, think about how to cause failure, then avoid those things. Approach problems from the opposite end."
   :key-insight "It is not enough to think about difficult problems one way. You need to think about them forwards and backwards."
   :application "When solving problems: (1) Define what you want, (2) Invert to what you don't want, (3) Avoid the things that cause what you don't want, (4) What remains is closer to what you want."
   :munger-quote "Invert, always invert. Many hard problems are best solved when they are addressed backward."
   :failure-modes
   [{:name "not-inverting-at-all"
     :severity "high"
     :description "Only thinking forward about success without considering failure modes, leading to blind spots and preventable failures."
     :mechanism "Positive thinking bias and optimism lead to exclusive focus on success paths. Failure scenarios feel pessimistic and are avoided."
     :psychological-root "Optimism bias, confirmation bias, and motivated reasoning. People prefer thinking about success over failure."
     :evolutionary-origin "Optimism provided survival advantage (trying despite odds). Excessive pessimism led to inaction and missed opportunities."
     :signals
     ["Plans focus only on best-case scenarios"
      "No pre-mortem or failure analysis conducted"
      "Surprise when things go wrong"
      "Lack of contingency plans"
      "Dismissing concerns as 'negative thinking'"
      "No discussion of what could go wrong"
      "Overconfidence in success probability"
      "Failure to identify critical failure points"]
     :case-studies
     [{:name "Challenger Space Shuttle Disaster"
       :year 1986
       :entity "NASA"
       :description "Engineers warned about O-ring failure in cold temperatures. Management focused on launch success, not failure prevention. O-rings failed at 36°F, killing 7 astronauts."
       :quantitative-impact "$4.5B shuttle destroyed. 7 lives lost. 32-month program suspension. $196M investigation cost."
       :root-cause "Focused on 'proving it will work' instead of 'what could make it fail'. Ignored inversion."
       :should-have-done "Pre-mortem: 'How could this launch kill astronauts?' Would have identified O-ring cold-temperature risk."
       :sources ["Rogers Commission Report (1986)", "Vaughan, D. The Challenger Launch Decision (1996)", "NASA investigation files"]}
      {:name "Long-Term Capital Management Collapse"
       :year 1998
       :entity "LTCM Hedge Fund"
       :description "Nobel Prize-winning team focused on profit models, not failure scenarios. Didn't ask 'How could we lose everything?' Overleveraged, lost $4.6B in 4 months."
       :quantitative-impact "$4.6B lost (90% of capital). $3.6B bailout required. 14 major banks at risk. Systemic crisis narrowly averted."
       :root-cause "Brilliant forward analysis of profit opportunities. Zero inversion: 'What would destroy us?' Ignored tail risk."
       :should-have-done "Invert: 'How do we go bankrupt?' Answer: extreme leverage + correlated positions + liquidity crisis. Avoid that."
       :sources ["Lowenstein, R. When Genius Failed (2000)", "Federal Reserve Bank of NY reports", "GAO Report GAO/GGD-00-3"]}
      {:name "Blockbuster vs Netflix"
       :year 2000
       :entity "Blockbuster Video"
       :description "Blockbuster focused on 'How do we open more stores?' Not 'How do we become obsolete?' Netflix inverted: 'What do customers hate about video rental?' (Late fees, driving to store, limited selection)"
       :quantitative-impact "Blockbuster: $5B revenue (2004) → bankruptcy (2010). Netflix: $9B → $280B market cap (2021)."
       :root-cause "Blockbuster optimized existing model. Netflix inverted: eliminate everything customers hate."
       :should-have-done "Invert: 'What would make customers never visit our stores?' Would have identified convenience, late fees, selection issues."
       :sources ["Keyes, J. Netflix vs Blockbuster case study", "Antioco, J. Harvard Business Review (2011)", "Company financial statements"]}
      {:name "Theranos Fraud"
       :year 2003-2018
       :entity "Theranos Inc"
       :description "Elizabeth Holmes focused on 'How do we revolutionize blood testing?' Not 'How could this be fraudulent or fail?' Investors failed to invert: 'What would make this impossible?'"
       :quantitative-impact "$700M investor losses. $9B peak valuation → $0. Criminal convictions. Patient harm from inaccurate tests."
       :root-cause "Forward thinking only: 'Imagine if this works!' No inversion: 'What would make this impossible?' (Physics, biology, regulation)"
       :should-have-done "Invert: 'Why hasn't anyone done this before?' 'What would make this fraudulent?' Would have revealed red flags."
       :sources ["Carreyrou, J. Bad Blood (2018)", "SEC complaint (2018)", "US v. Holmes trial transcripts"]}
      {:name "2008 Financial Crisis - Rating Agencies"
       :year 2008
       :entity "Moody's, S&P, Fitch"
       :description "Rating agencies focused on 'How do we rate these securities?' Not 'How could these all fail simultaneously?' Gave AAA ratings to toxic assets."
       :quantitative-impact "$7.4T wealth destroyed. 8.7M jobs lost. 10M foreclosures. Global recession."
       :root-cause "Forward analysis: 'Historical default rates are low.' No inversion: 'What would make everything default at once?'"
       :should-have-done "Invert: 'How could we be catastrophically wrong?' Would have identified correlated risk, housing bubble, fraud."
       :sources ["Financial Crisis Inquiry Report (2011)", "Senate Permanent Subcommittee Investigation (2011)", "DOJ settlements"]}]
     :quantitative-thresholds
     [{:metric "Pre-Mortem Completion Rate"
       :warning-threshold "< 50% of major decisions"
       :critical-threshold "< 25% of major decisions"
       :measurement "Percentage of major decisions with documented pre-mortem analysis"
       :frequency "Per major decision or project"
       :example "Launched 8 products this year, only 2 had pre-mortem sessions"}
      {:metric "Failure Mode Documentation"
       :warning-threshold "< 5 failure modes identified per major initiative"
       :critical-threshold "< 3 failure modes identified"
       :measurement "Number of distinct failure scenarios documented before launch"
       :frequency "Per major initiative"
       :example "Product launch plan identified only 2 failure modes (should be 10+)"}
      {:metric "Contingency Plan Coverage"
       :warning-threshold "< 70% of identified risks have mitigation plans"
       :critical-threshold "< 50% have mitigation plans"
       :measurement "Percentage of identified failure modes with documented contingency plans"
       :frequency "Per project"
       :example "Identified 10 risks, only 4 have contingency plans"}]
     :environmental-triggers
     ["High-pressure, optimistic organizational culture"
      "Tight deadlines discouraging thorough analysis"
      "Charismatic leaders dismissing concerns"
      "Success bias from recent wins"
      "Groupthink and conformity pressure"
      "Lack of psychological safety to voice concerns"]
     :structural-safeguards
     ["Mandatory pre-mortem for all major decisions"
      "Red team / devil's advocate roles"
      "Anonymous concern submission systems"
      "Failure mode checklists"
      "Post-mortem database of past failures"
      "Contingency planning requirements"
      "Risk committees with veto power"
      "Stress testing and scenario analysis"]
     :cognitive-safeguards
     ["Always ask: 'How could this fail catastrophically?'"
      "Invert the goal: 'How would we guarantee failure?'"
      "List assumptions, then invert each one"
      "Consider second-order and third-order failure modes"
      "Study similar failures in history"
      "Map all critical dependencies and single points of failure"]
     :social-safeguards
     ["Diverse teams with different perspectives"
      "External expert review"
      "Whistleblower protections"
      "Culture that rewards identifying problems early"
      "Senior leadership modeling inversion"
      "Psychological safety for dissent"]
     :recovery-protocols
     [{:step 1
       :action "Immediately conduct post-mortem: What failure modes did we miss?"
       :responsible "Decision owner + independent reviewer"
       :timeline "Within 72 hours of failure recognition"
       :success-criteria "Documented list of missed failure modes and why they were missed"
       :escalation "If systematic blindness found, escalate to board/CEO"}
      {:step 2
       :action "Implement immediate damage control and contingency plans"
       :responsible "Crisis management team"
       :timeline "Within 1 week"
       :success-criteria "Failure contained, no further spread"
       :escalation "If damage control insufficient, consider full shutdown/reversal"}
      {:step 3
       :action "Update decision-making process to require inversion"
       :responsible "Strategy/process improvement team"
       :timeline "Within 1 month"
       :success-criteria "Mandatory pre-mortem and failure mode analysis in place"
       :escalation "If process changes insufficient, bring in external consultants"}
      {:step 4
       :action "Train all decision-makers on inversion and pre-mortem techniques"
       :responsible "Learning & development"
       :timeline "Within 2 months"
       :success-criteria "All key decision-makers trained and using inversion"
       :escalation "If adoption low, tie to performance reviews"}]
     :lollapalooza-risk 0.95
     :amplifying-biases ["optimism-bias" "confirmation-bias" "groupthink" "authority-bias" "sunk-cost-fallacy"]
     :mitigating-models ["pre-mortem" "red-team-analysis" "second-order-thinking" "margin-of-safety" "circle-of-competence"]}
    
    {:name "superficial-inversion"
     :severity "medium"
     :description "Going through the motions of inversion without deep analysis. Identifying obvious failure modes but missing subtle, high-impact ones."
     :mechanism "Checklist compliance without genuine critical thinking. Satisficing rather than optimizing failure analysis."
     :psychological-root "Cognitive ease and effort minimization. Once a few failure modes are identified, the brain feels satisfied and stops searching."
     :evolutionary-origin "Satisficing was efficient in ancestral environments. Finding 'good enough' solutions conserved energy."
     :signals
     ["Pre-mortems completed quickly (< 30 minutes for major decisions)"
      "Failure modes are generic and obvious"
      "No deep causal analysis of failure mechanisms"
      "Failure modes not prioritized by severity"
      "No quantitative analysis of failure probability or impact"
      "Contingency plans are vague"
      "No follow-up or refinement of failure analysis"]
     :case-studies
     [{:name "Boeing 737 MAX Crashes"
       :year 2018-2019
       :entity "Boeing"
       :description "Boeing conducted safety analysis but missed critical failure mode: MCAS system with single sensor, no pilot training, could cause uncontrollable dive. 346 deaths in two crashes."
       :quantitative-impact "346 deaths. $20B+ in costs (compensation, lost orders, grounding). 20-month grounding. Criminal charges."
       :root-cause "Superficial safety analysis. Identified some failure modes but missed critical single-point-of-failure in MCAS."
       :should-have-done "Deep inversion: 'What single component failure could cause crash?' Would have identified single-sensor MCAS vulnerability."
       :sources ["House Committee Report (2020)", "NTSB reports", "DOJ deferred prosecution agreement"]}
      {:name "Target Canada Expansion Failure"
       :year 2013-2015
       :entity "Target Corporation"
       :description "Target identified some risks of Canadian expansion but missed critical failure modes: supply chain complexity, inventory system failures, pricing errors. Lost $2B, closed all 133 stores."
       :quantitative-impact "$2B loss. 133 stores closed. 17,600 jobs lost. 2-year failure."
       :root-cause "Superficial risk analysis. Identified obvious risks (competition, real estate) but missed operational complexity."
       :should-have-done "Deep inversion: 'What operational failures would force us to close?' Would have identified supply chain and inventory system risks."
       :sources ["Canadian Business analysis", "Target annual reports", "Post-mortem case studies"]}
      {:name "Knight Capital Trading Glitch"
       :year 2012
       :entity "Knight Capital Group"
       :description "Had risk controls but missed critical failure mode: old code could be accidentally activated, causing $440M loss in 45 minutes. Company nearly collapsed."
       :quantitative-impact "$440M loss in 45 minutes. Stock dropped 75%. Forced sale to avoid bankruptcy."
       :root-cause "Superficial risk analysis. Had some controls but missed 'old code reactivation' failure mode."
       :should-have-done "Deep inversion: 'What code errors could cause catastrophic loss?' Would have identified dormant code risk."
       :sources ["SEC investigation", "Nanex analysis", "Risk management case studies"]}]
     :quantitative-thresholds
     [{:metric "Pre-Mortem Depth Score"
       :warning-threshold "< 6/10 on depth rubric"
       :critical-threshold "< 4/10"
       :measurement "Scored rubric: causal analysis, quantification, prioritization, contingency detail"
       :frequency "Per pre-mortem session"
       :example "Pre-mortem scored 5/10: identified failure modes but no causal analysis or quantification"}
      {:metric "Failure Mode Uniqueness"
       :warning-threshold "> 50% of failure modes are generic"
       :critical-threshold "> 75% are generic"
       :measurement "Percentage of failure modes that are specific to this situation vs generic"
       :frequency "Per failure analysis"
       :example "8 of 10 failure modes could apply to any project (too generic)"}]
     :environmental-triggers
     ["Process compliance culture (checking boxes)"
      "Time pressure to complete analysis quickly"
      "Lack of expertise in failure mode analysis"
      "No examples of high-quality inversion"
      "No review or quality control of failure analysis"]
     :structural-safeguards
     ["Quality rubrics for pre-mortem analysis"
      "Minimum time requirements (e.g., 2 hours for major decisions)"
      "Expert facilitators for critical pre-mortems"
      "Peer review of failure analysis"
      "Benchmarking against historical failures"
      "Quantitative risk assessment requirements"]
     :cognitive-safeguards
     ["Ask: 'What failure modes are we missing?'"
      "Compare to historical failures in similar situations"
      "Drill down: 'What would cause that failure?'"
      "Quantify probability and impact"
      "Prioritize by severity × likelihood"
      "Red team challenge: 'What did they miss?'"]
     :social-safeguards
     ["External expert review for critical decisions"
      "Cross-functional teams (diverse perspectives)"
      "Incentives for thorough analysis, not quick completion"
      "Public sharing of pre-mortem quality examples"]
     :recovery-protocols
     [{:step 1
       :action "Identify which failure modes were missed and why"
       :responsible "Post-mortem team"
       :timeline "Within 1 week"
       :success-criteria "Gap analysis: what we analyzed vs what we should have"
       :escalation "If systematic gaps, overhaul failure analysis process"}
      {:step 2
       :action "Improve failure analysis process and training"
       :responsible "Risk management + L&D"
       :timeline "Within 1 month"
       :success-criteria "Updated process with quality requirements"
       :escalation "If quality still low, mandate external expert review"}
      {:step 3
       :action "Re-analyze all active major initiatives with improved process"
       :responsible "Strategy team"
       :timeline "Within 2 months"
       :success-criteria "All major initiatives have high-quality failure analysis"
       :escalation "If new critical risks found, pause initiatives to address"}
      {:step 4
       :action "Ongoing quality audits of failure analysis"
       :responsible "Internal audit / risk committee"
       :timeline "Quarterly ongoing"
       :success-criteria "Consistent high-quality failure analysis"
       :escalation "If quality regresses, investigate and address root causes"}]
     :lollapalooza-risk 0.75
     :amplifying-biases ["confirmation-bias" "availability-heuristic" "effort-minimization" "false-sense-of-security"]
     :mitigating-models ["second-order-thinking" "first-principles" "red-team-analysis" "systems-thinking"]}
    
    {:name "inversion-paralysis"
     :severity "medium"
     :description "Excessive focus on failure modes leads to paralysis, risk aversion, and inability to act. Perfect becomes enemy of good."
     :mechanism "Identifying too many failure modes without prioritization creates overwhelming complexity and fear."
     :psychological-root "Loss aversion (losses loom larger than gains). Fear of regret and blame."
     :evolutionary-origin "Excessive caution prevented death in dangerous environments. Better to miss opportunities than die."
     :signals
     ["Decisions delayed indefinitely due to risk concerns"
      "Every action seems too risky"
      "Endless analysis without decision"
      "Paralysis by analysis"
      "Missed opportunities due to excessive caution"
      "Culture of fear and risk aversion"
      "No tolerance for any failure"]
     :case-studies
     [{:name "Kodak Digital Camera Delay"
       :year 1975-2000
       :entity "Kodak"
       :description "Kodak invented digital camera in 1975 but feared it would cannibalize film business. Inverted correctly (digital threatens film) but became paralyzed. Competitors moved forward, Kodak went bankrupt."
       :quantitative-impact "Peak $31B market cap → bankruptcy (2012). 145,000 employees → 6,000."
       :root-cause "Correct inversion (digital kills film) but paralysis from fear. Didn't act on insight."
       :should-have-done "Invert, then act: 'If we don't cannibalize ourselves, someone else will.' Embrace digital aggressively."
       :sources ["Lucas, H. & Goh, J. Harvard Business Review (2009)", "Bankruptcy court filings", "Munir, K. & Phillips, N. Organization Science (2005)"]}
      {:name "Yahoo's Missed Acquisitions"
       :year 1998-2008
       :entity "Yahoo"
       :description "Yahoo correctly identified risks in acquisitions (overpaying, integration challenges) but became too cautious. Passed on Google ($1M, 1998), Facebook ($1B, 2006). Both became worth hundreds of billions."
       :quantitative-impact "Missed Google (now $1.7T). Missed Facebook (now $800B). Yahoo sold for $4.8B (2017)."
       :root-cause "Correct risk identification but excessive risk aversion. Paralyzed by fear of overpaying."
       :should-have-done "Balance inversion with opportunity cost. 'What if we're wrong and miss the next big thing?'"
       :sources ["Carlson, N. Business Insider (2011)", "Vise, D. The Google Story (2005)", "Yahoo annual reports"]}]
     :quantitative-thresholds
     [{:metric "Decision Cycle Time"
       :warning-threshold "> 2x industry average"
       :critical-threshold "> 4x industry average"
       :measurement "Time from decision initiation to final decision"
       :frequency "Per major decision"
       :example "Competitors make similar decisions in 3 months, we take 12 months"}
      {:metric "Opportunity Cost Ratio"
       :warning-threshold "> 30% of opportunities missed due to excessive caution"
       :critical-threshold "> 50% missed"
       :measurement "Percentage of viable opportunities passed on due to risk aversion"
       :frequency "Quarterly review"
       :example "Passed on 6 of 10 good opportunities due to risk concerns"}]
     :environmental-triggers
     ["Blame culture punishing any failure"
      "Recent major failure creating fear"
      "Risk-averse leadership"
      "No tolerance for experimentation"
      "Bureaucratic approval processes"]
     :structural-safeguards
     ["Decision deadlines to prevent endless analysis"
      "Reversible decision frameworks (two-way doors)"
      "Small-scale experiments before full commitment"
      "Opportunity cost analysis required"
      "Balanced scorecards (risk AND opportunity)"
      "Portfolio approach (some high-risk bets acceptable)"]
     :cognitive-safeguards
     ["Invert the inversion: 'What if we're too cautious?'"
      "Opportunity cost: 'What do we miss by not acting?'"
      "Regret minimization: 'Will I regret not trying?'"
      "Reversibility: 'Can we undo this if wrong?'"
      "Asymmetric risk: 'Limited downside, unlimited upside?'"]
     :social-safeguards
     ["Culture celebrating intelligent risk-taking"
      "Blameless post-mortems"
      "Rewards for good decisions, not just good outcomes"
      "Leadership modeling calculated risk-taking"]
     :recovery-protocols
     [{:step 1
       :action "Recognize paralysis pattern and opportunity costs"
       :responsible "Leadership team"
       :timeline "Immediate"
       :success-criteria "Acknowledged pattern of excessive caution"
       :escalation "If denial, bring in external advisor to provide perspective"}
      {:step 2
       :action "Implement decision forcing mechanisms"
       :responsible "Strategy team"
       :timeline "Within 2 weeks"
       :success-criteria "Deadlines, reversibility frameworks, portfolio approach in place"
       :escalation "If still paralyzed, consider leadership changes"}
      {:step 3
       :action "Culture change: celebrate intelligent risk-taking"
       :responsible "CEO + HR"
       :timeline "Within 3 months"
       :success-criteria "Visible examples of rewarded risk-taking"
       :escalation "If culture doesn't shift, external culture consultant"}
      {:step 4
       :action "Portfolio rebalancing: add calculated risks"
       :responsible "Strategy team"
       :timeline "Within 6 months"
       :success-criteria "Balanced portfolio of safe and risky bets"
       :escalation "If still too conservative, board intervention"}]
     :lollapalooza-risk 0.70
     :amplifying-biases ["loss-aversion" "regret-aversion" "status-quo-bias" "analysis-paralysis"]
     :mitigating-models ["opportunity-cost" "reversible-decisions" "asymmetric-risk" "regret-minimization" "portfolio-thinking"]}
    
    {:name "false-inversion"
     :severity "high"
     :description "Inverting the wrong problem or inverting incorrectly, leading to solutions that don't address the real issue."
     :mechanism "Misidentifying the core problem before inverting, or inverting in a way that doesn't reveal useful insights."
     :psychological-root "Jumping to solutions before understanding the problem. Confirmation bias in problem definition."
     :evolutionary-origin "Fast problem-solving was adaptive. Taking time to deeply understand problems was often unnecessary."
     :signals
     ["Solutions that don't address root causes"
      "Inverted problem statement doesn't match actual problem"
      "Failure modes identified aren't the real risks"
      "Solutions create new problems"
      "Stakeholders disagree with problem definition"]
     :safeguards ["First principles problem definition" "Stakeholder validation" "Root cause analysis before inversion" "Multiple problem framings"]}
    
    {:name "inversion-without-action"
     :severity "medium"
     :description "Identifying failure modes through inversion but failing to act on the insights to prevent those failures."
     :mechanism "Analysis-action gap. Knowing what to avoid but not implementing safeguards."
     :psychological-root "Intention-action gap. Knowing vs doing. Implementation difficulty."
     :evolutionary-origin "Planning was easier than execution. Execution required sustained effort and resources."
     :signals
     ["Great pre-mortems but no follow-through"
      "Failure modes documented but not monitored"
      "Contingency plans not implemented"
      "Known risks not mitigated"
      "Failures occur that were predicted but not prevented"]
     :safeguards ["Accountability for risk mitigation" "Tracking systems for contingency plan implementation" "Risk dashboards" "Regular risk reviews"]}]})



;; Enhanced Model: Incentives
;; Munger: "Show me the incentives and I'll show you the outcome"
(def incentives-enhanced
  {:name "incentives"
   :category "psychology"
   :description "People respond to incentives. Understand the incentive structures and you can predict behavior. Misaligned incentives lead to perverse outcomes."
   :key-insight "Never, ever, think about something else when you should be thinking about the power of incentives."
   :application "When analyzing any system or decision: (1) Map all stakeholder incentives, (2) Identify misalignments, (3) Predict behavior based on incentives, (4) Design incentives to align with desired outcomes."
   :munger-quote "Show me the incentives and I'll show you the outcome. Never think about anything else when you should be thinking about incentives."
   :failure-modes
   [{:name "ignoring-incentives"
     :severity "high"
     :description "Failing to consider incentive structures when predicting behavior or designing systems, leading to surprise when people act according to their incentives."
     :mechanism "Naive assumption that people will do 'the right thing' regardless of incentives. Moral reasoning overrides incentive analysis."
     :psychological-root "Fundamental attribution error: attributing behavior to character rather than situation. Belief in intrinsic motivation."
     :evolutionary-origin "In small tribes, reputation and reciprocity were strong. Modern anonymous systems require different incentive analysis."
     :signals
     ["Surprise when people act 'selfishly'"
      "Assuming people will act against their interests"
      "Moral appeals without incentive alignment"
      "Expecting voluntary compliance without enforcement"
      "Blaming individuals rather than systems"
      "No analysis of 'What's in it for them?'"
      "Policies that assume altruism at scale"]
     :case-studies
     [{:name "Wells Fargo Fake Accounts Scandal"
       :year 2011-2016
       :entity "Wells Fargo Bank"
       :description "Aggressive cross-selling quotas incentivized employees to create millions of fake accounts. Management ignored incentive effects, blamed 'bad apples.' 5,300 employees fired, $3B in fines."
       :quantitative-impact "3.5M fake accounts created. $3B in fines and settlements. 5,300 employees fired. CEO resigned. Stock dropped 15%."
       :root-cause "Ignored incentive effects: unrealistic quotas + commission + job security = fraud. Assumed employees would 'do right thing.'"
       :should-have-done "Analyze incentives: 'What behavior do these quotas incentivize?' Would have predicted fraud. Align incentives with customer value."
       :sources ["OCC Consent Order (2016)", "CFPB enforcement action", "Congressional hearings (2016)", "Independent Directors Report (2017)"]}
      {:name "Soviet Nail Factory"
       :year 1950s-1980s
       :entity "Soviet Central Planning"
       :description "Factory incentivized by weight of nails produced. Made huge, useless nails. Changed to count of nails. Made tiny, useless nails. Ignored incentive effects."
       :quantitative-impact "Massive waste of resources. Unusable products. Contributed to economic inefficiency and eventual Soviet collapse."
       :root-cause "Ignored Goodhart's Law: when measure becomes target, it ceases to be good measure. Didn't think about incentive effects."
       :should-have-done "Think through incentives: 'If we measure by weight, what will they produce?' Design multi-dimensional incentives."
       :sources ["Nove, A. The Soviet Economic System (1977)", "Kornai, J. Economics of Shortage (1980)", "Soviet planning archives"]}
      {:name "Sears Auto Center Fraud"
       :year 1992
       :entity "Sears Auto Centers"
       :description "Mechanics had quotas for repairs and parts sales. Incentivized unnecessary repairs. Charged for work not done. $60M in fines, massive reputation damage."
       :quantitative-impact "$60M in fines and refunds. 20% drop in auto center revenue. Permanent reputation damage. Closed many centers."
       :root-cause "Ignored incentive effects: repair quotas + commission = unnecessary repairs. Assumed professional ethics would prevail."
       :should-have-done "Analyze incentives: 'What do repair quotas incentivize?' Would have predicted unnecessary repairs. Incentivize customer satisfaction, not volume."
       :sources ["California Department of Consumer Affairs investigation (1992)", "Sears settlement agreements", "Business ethics case studies"]}
      {:name "Cobra Effect - British India"
       :year 1900
       :entity "British Colonial Government"
       :description "Bounty for dead cobras to reduce population. People bred cobras for bounty. Program ended, breeders released cobras. Population increased."
       :quantitative-impact "Cobra population increased above baseline. Wasted government funds. Classic example of perverse incentives."
       :root-cause "Ignored incentive effects: bounty creates incentive to breed cobras, not just kill wild ones."
       :should-have-done "Think through incentives: 'What does this bounty incentivize?' Would have predicted cobra breeding."
       :sources ["Siebert, H. Der Kobra-Effekt (2001)", "Economic policy case studies", "Colonial administration records"]}
      {:name "Enron Accounting Fraud"
       :year 1985-2001
       :entity "Enron Corporation"
       :description "Executives incentivized by stock price and quarterly earnings. Used mark-to-market accounting and SPVs to inflate earnings. $74B in shareholder losses."
       :quantitative-impact "$74B shareholder losses. 20,000 jobs lost. $2B+ in pension losses. Arthur Andersen destroyed. Sarbanes-Oxley Act passed."
       :root-cause "Ignored incentive effects: stock options + quarterly pressure + complex accounting = fraud. Assumed executives would act ethically."
       :should-have-done "Analyze incentives: 'What do stock options + quarterly targets incentivize?' Long-term incentives, independent oversight."
       :sources ["Powers Report (2002)", "Bankruptcy examiner report", "SEC enforcement actions", "Congressional hearings"]}]
     :quantitative-thresholds
     [{:metric "Incentive Analysis Completion Rate"
       :warning-threshold "< 60% of major decisions include incentive analysis"
       :critical-threshold "< 40%"
       :measurement "Percentage of major decisions with documented stakeholder incentive analysis"
       :frequency "Per major decision"
       :example "Launched 10 initiatives, only 3 included incentive analysis"}
      {:metric "Incentive-Behavior Alignment Score"
       :warning-threshold "< 70% alignment"
       :critical-threshold "< 50% alignment"
       :measurement "Percentage of stakeholder groups whose incentives align with desired behavior"
       :frequency "Per system or policy design"
       :example "5 of 10 stakeholder groups have misaligned incentives"}
      {:metric "Perverse Incentive Detection Rate"
       :warning-threshold "< 50% of perverse incentives identified before launch"
       :critical-threshold "< 25%"
       :measurement "Percentage of perverse incentives caught in design vs discovered after launch"
       :frequency "Post-launch review"
       :example "Found 8 perverse incentives post-launch, only caught 2 in design"}]
     :environmental-triggers
     ["Complex systems with many stakeholders"
      "Moral or ideological thinking overriding incentive analysis"
      "Assumption that people are primarily altruistic"
      "Lack of game theory or economic thinking"
      "Naive trust in professional ethics"]
     :structural-safeguards
     ["Mandatory incentive analysis for all major decisions"
      "Stakeholder incentive mapping"
      "Game theory analysis of strategic interactions"
      "Pilot programs to test incentive effects"
      "Regular incentive audits"
      "Whistleblower protections and rewards"
      "Independent oversight of incentive systems"]
     :cognitive-safeguards
     ["Always ask: 'What's in it for them?'"
      "Map all stakeholder incentives explicitly"
      "Predict behavior based on incentives, not intentions"
      "Look for misalignments between incentives and goals"
      "Consider unintended consequences of incentives"
      "Study historical examples of perverse incentives"]
     :social-safeguards
     ["Diverse teams including economists and game theorists"
      "External review of incentive systems"
      "Transparency in incentive structures"
      "Stakeholder feedback on incentive effects"
      "Culture of incentive-aware thinking"]
     :recovery-protocols
     [{:step 1
       :action "Immediately map actual incentive effects causing problems"
       :responsible "Strategy team + economists"
       :timeline "Within 1 week"
       :success-criteria "Complete incentive map showing perverse effects"
       :escalation "If effects are severe, immediately suspend incentive system"}
      {:step 2
       :action "Redesign incentives to align with desired outcomes"
       :responsible "Incentive design team"
       :timeline "Within 1 month"
       :success-criteria "New incentive system with alignment analysis"
       :escalation "If design is complex, bring in external game theory experts"}
      {:step 3
       :action "Pilot new incentive system with monitoring"
       :responsible "Operations + analytics"
       :timeline "3-6 month pilot"
       :success-criteria "Incentives driving desired behavior without perverse effects"
       :escalation "If pilot shows problems, iterate design"}
      {:step 4
       :action "Full rollout with ongoing monitoring"
       :responsible "Operations team"
       :timeline "After successful pilot"
       :success-criteria "Sustained alignment between incentives and outcomes"
       :escalation "If problems emerge, rapid iteration"}]
     :lollapalooza-risk 0.95
     :amplifying-biases ["fundamental-attribution-error" "moral-reasoning-bias" "optimism-bias" "trust-bias"]
     :mitigating-models ["game-theory" "second-order-thinking" "systems-thinking" "inversion" "principal-agent-problem"]}
    
    {:name "misaligned-incentives"
     :severity "high"
     :description "Incentives that reward behavior contrary to stated goals, leading to perverse outcomes and goal subversion."
     :mechanism "Measurement and reward systems focus on easily measured proxies rather than true goals, creating misalignment."
     :psychological-root "Goodhart's Law: when measure becomes target, ceases to be good measure. Optimization pressure on proxies."
     :evolutionary-origin "Simple cause-effect worked in ancestral environments. Complex proxy measures are modern phenomenon."
     :signals
     ["Metrics improving but outcomes worsening"
      "Gaming of measurement systems"
      "Focus on hitting targets rather than achieving goals"
      "Unintended behaviors emerging"
      "Stakeholders optimizing for rewards, not results"
      "Disconnect between incentives and mission"]
     :case-studies
     [{:name "Teaching to the Test - No Child Left Behind"
       :year 2002-2015
       :entity "US Education System"
       :description "Schools incentivized by standardized test scores. Teachers taught to tests, narrowed curriculum, excluded low-performing students from testing. Scores up, learning down."
       :quantitative-impact "Test scores increased but college readiness declined. Achievement gaps widened. $1B+ in testing costs. Program abandoned."
       :root-cause "Misaligned incentives: test scores ≠ actual learning. Schools optimized for scores, not education."
       :should-have-done "Align incentives with actual learning outcomes, not test scores. Multiple measures, harder to game."
       :sources ["Koretz, D. The Testing Charade (2017)", "GAO reports", "National Research Council studies"]}
      {:name "Volkswagen Emissions Scandal"
       :year 2006-2015
       :entity "Volkswagen AG"
       :description "Engineers incentivized to meet emissions standards and performance targets simultaneously (impossible). Created defeat device to cheat tests. $30B+ in fines."
       :quantitative-impact "$30B+ in fines and settlements. Criminal charges. CEO resigned. 11M vehicles affected. Stock dropped 50%."
       :root-cause "Misaligned incentives: meet impossible standards or face consequences. Incentivized cheating."
       :should-have-done "Align incentives with reality. Don't incentivize impossible targets. Whistleblower protections."
       :sources ["DOJ settlements", "German parliamentary inquiry", "EPA enforcement actions"]}
      {:name "Luckin Coffee Fraud"
       :year 2019
       :entity "Luckin Coffee (China)"
       :description "Management incentivized by growth and valuation. Fabricated $300M in sales to meet growth targets. Stock crashed 80% in one day when exposed."
       :quantitative-impact "$300M fabricated sales. $180M fine. Delisted from NASDAQ. $5B+ in shareholder losses."
       :root-cause "Misaligned incentives: growth at any cost. Incentivized fraud to meet targets."
       :should-have-done "Align incentives with sustainable, verified growth. Independent audits. Realistic targets."
       :sources ["SEC complaint (2020)", "Muddy Waters report", "NASDAQ delisting notice"]}]
     :quantitative-thresholds
     [{:metric "Goal-Incentive Alignment Score"
       :warning-threshold "< 70% alignment"
       :critical-threshold "< 50% alignment"
       :measurement "Percentage of incentive metrics that directly measure true goals (not proxies)"
       :frequency "Annual incentive system audit"
       :example "8 of 10 KPIs are easily-gamed proxies, only 2 measure real goals"}
      {:metric "Gaming Detection Rate"
       :warning-threshold "> 20% of participants gaming metrics"
       :critical-threshold "> 40%"
       :measurement "Percentage of participants found to be optimizing metrics without improving outcomes"
       :frequency "Quarterly audit"
       :example "Found 35% of sales team hitting quotas through channel stuffing"}]
     :environmental-triggers
     ["Pressure for simple, measurable metrics"
      "Short-term focus (quarterly earnings)"
      "Competitive pressure to hit targets"
      "Difficulty measuring true goals"
      "Lack of holistic outcome measures"]
     :structural-safeguards
     ["Multi-dimensional incentives (harder to game)"
      "Outcome-based, not activity-based incentives"
      "Long-term incentives (3-5 years)"
      "Clawback provisions for gaming"
      "Regular incentive system audits"
      "Anonymous reporting of gaming"
      "Penalties for gaming, not just missing targets"]
     :cognitive-safeguards
     ["Ask: 'How could this incentive be gamed?'"
      "Invert: 'What perverse behavior does this reward?'"
      "Test: 'If everyone optimized this metric, what would happen?'"
      "Align: 'Does this metric truly measure our goal?'"
      "Study: Historical examples of similar incentive failures"]
     :social-safeguards
     ["Culture emphasizing outcomes over metrics"
      "Transparency in incentive design rationale"
      "Stakeholder input on incentive effects"
      "Whistleblower protections"
      "Leadership modeling focus on true goals"]
     :recovery-protocols
     [{:step 1
       :action "Identify how incentives are being gamed"
       :responsible "Internal audit + analytics"
       :timeline "Within 2 weeks"
       :success-criteria "Documented gaming patterns and perverse behaviors"
       :escalation "If gaming is widespread, immediately suspend incentive payouts"}
      {:step 2
       :action "Redesign incentives to align with true goals"
       :responsible "Compensation committee + strategy"
       :timeline "Within 2 months"
       :success-criteria "New incentive system resistant to gaming"
       :escalation "If design is difficult, bring in external compensation consultants"}
      {:step 3
       :action "Communicate new system and rationale"
       :responsible "Leadership + HR"
       :timeline "Before new system launch"
       :success-criteria "All stakeholders understand new incentives and why"
       :escalation "If communication fails, delay launch until clear"}
      {:step 4
       :action "Monitor new system for gaming"
       :responsible "Analytics + internal audit"
       :timeline "Ongoing, monthly reviews"
       :success-criteria "No gaming detected, outcomes improving"
       :escalation "If gaming emerges, rapid iteration"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["goodharts-law" "optimization-pressure" "short-term-thinking" "measurement-bias"]
     :mitigating-models ["systems-thinking" "second-order-thinking" "inversion" "long-term-thinking"]}
    
    {:name "hidden-incentives"
     :severity "high"
     :description "Failing to recognize implicit, social, or psychological incentives that drive behavior alongside or contrary to explicit incentives."
     :mechanism "Focus on formal incentives (money, promotions) while missing social incentives (status, belonging, identity)."
     :psychological-root "Economic thinking overemphasizes monetary incentives. Social psychology shows humans are deeply social beings."
     :evolutionary-origin "In ancestral tribes, social incentives (status, belonging, reputation) were often more important than material incentives."
     :signals
     ["Formal incentives not driving expected behavior"
      "Informal norms overriding formal rules"
      "Status games and politics dominating"
      "Identity and tribal dynamics affecting decisions"
      "Social proof and conformity effects"
      "Fear of embarrassment or exclusion driving choices"]
     :safeguards ["Map social and psychological incentives" "Consider status, belonging, identity" "Understand informal power structures" "Cultural and social analysis"]}
    
    {:name "delayed-incentives"
     :severity "medium"
     :description "Incentives with long time delays between action and reward, leading to hyperbolic discounting and short-term thinking."
     :mechanism "Human psychology heavily discounts delayed rewards. Immediate incentives dominate long-term incentives."
     :psychological-root "Hyperbolic discounting: present rewards valued much more than future rewards. Impatience."
     :evolutionary-origin "In uncertain ancestral environments, immediate rewards were more valuable than uncertain future rewards."
     :signals
     ["Short-term thinking despite long-term incentives"
      "Procrastination on important long-term tasks"
      "Preference for immediate gratification"
      "Long-term goals abandoned for short-term gains"
      "Difficulty maintaining discipline"]
     :safeguards ["Immediate feedback on long-term goals" "Commitment devices" "Interim milestones and rewards" "Make future consequences vivid"]}
    
    {:name "complex-incentives"
     :severity "medium"
     :description "Incentive systems so complex that people can't understand or predict them, leading to confusion and suboptimal behavior."
     :mechanism "Cognitive overload from complex incentive formulas. People simplify or ignore incentives they don't understand."
     :psychological-root "Limited working memory and cognitive capacity. Preference for simple, understandable systems."
     :evolutionary-origin "Simple cause-effect relationships were sufficient in ancestral environments. Complex formulas are modern."
     :signals
     ["Confusion about how incentives work"
      "People ignoring incentive systems"
      "Unexpected behaviors due to misunderstanding"
      "Frequent questions about how incentives are calculated"
      "Gaming through exploiting complexity"]
     :safeguards ["Simplify incentive systems" "Clear communication and examples" "Transparency in calculations" "Test understanding before launch"]}]})



;; Enhanced Model: Margin of Safety
;; From Benjamin Graham, emphasized by Buffett and Munger
(def margin-of-safety-enhanced
  {:name "margin-of-safety"
   :category "decision_making"
   :description "Build in a buffer between what you expect and what you need. Assume things will go wrong. The margin of safety is the difference between the intrinsic value and the price paid."
   :key-insight "The three most important words in investing are 'margin of safety.' But this applies to all decisions, not just investing."
   :application "In any decision: (1) Estimate what you need, (2) Add a buffer for uncertainty, (3) Only proceed if you have adequate margin, (4) Larger margin for higher uncertainty."
   :munger-quote "The margin of safety is the difference between what something is worth and what you pay for it. But it applies to everything in life."
   :failure-modes
   [{:name "no-margin"
     :severity "high"
     :description "Operating with zero buffer, assuming best-case scenarios, leaving no room for error or unexpected events."
     :mechanism "Optimism bias and planning fallacy lead to underestimating risks and overestimating capabilities."
     :psychological-root "Overconfidence, optimism bias, illusion of control. Humans systematically underestimate uncertainty."
     :evolutionary-origin "Optimism was adaptive for taking necessary risks. Excessive caution led to missed opportunities."
     :signals
     ["Plans assume everything goes perfectly"
      "No contingency budget or time"
      "Single point of failure"
      "Maximal leverage or utilization"
      "No backup plans"
      "Tight deadlines with no buffer"
      "Financial plans with no emergency fund"
      "Surprise when things go wrong"]
     :case-studies
     [{:name "Long-Term Capital Management Collapse"
       :year 1998
       :entity "LTCM Hedge Fund"
       :description "Brilliant models, Nobel Prize winners, but 25:1 leverage (no margin). Small market moves caused $4.6B loss in 4 months. Nearly caused systemic crisis."
       :quantitative-impact "$4.6B lost (90% of capital). $3.6B bailout. 14 banks at risk. Leverage: 25:1 (later 100:1). No margin for error."
       :root-cause "No margin of safety. Assumed models were perfect, markets would behave normally. Zero buffer for tail events."
       :should-have-done "Maintain 3:1 or 5:1 leverage maximum. Stress test for extreme scenarios. Build cash reserves."
       :sources ["Lowenstein, R. When Genius Failed (2000)", "Federal Reserve reports", "GAO-00-3 report"]}
      {:name "Lehman Brothers Bankruptcy"
       :year 2008
       :entity "Lehman Brothers"
       :description "Operated with 30:1 leverage, minimal capital buffer. Small decline in asset values caused insolvency. $619B bankruptcy, largest in US history."
       :quantitative-impact "$619B bankruptcy. 26,000 jobs lost. Triggered global financial crisis. No margin of safety."
       :root-cause "No margin: 30:1 leverage meant 3.3% asset decline = insolvency. Assumed housing prices wouldn't fall."
       :should-have-done "Maintain adequate capital ratios (10:1 max). Stress test for housing decline. Build liquidity reserves."
       :sources ["Lehman Bankruptcy Examiner Report (2010)", "Financial Crisis Inquiry Report", "SEC investigations"]}
      {:name "Space Shuttle Challenger"
       :year 1986
       :entity "NASA"
       :description "O-rings had no margin of safety below 53°F. Launched at 36°F. O-rings failed, killing 7 astronauts."
       :quantitative-impact "7 deaths. $4.5B shuttle destroyed. 32-month program suspension. No margin of safety in design or decision."
       :root-cause "No margin: O-rings barely worked at 53°F, completely failed at 36°F. Launched anyway due to schedule pressure."
       :should-have-done "Design with wide margin (work at 0°F, not just 53°F). Don't launch outside safety margins."
       :sources ["Rogers Commission Report (1986)", "Vaughan, D. The Challenger Launch Decision (1996)"]}
      {:name "Evergrande Collapse"
       :year 2021
       :entity "China Evergrande Group"
       :description "Real estate developer with $300B debt, minimal liquidity. Small revenue decline caused default. Systemic risk to Chinese economy."
       :quantitative-impact "$300B in liabilities. $50B+ in losses. 200,000 employees. 1.5M unfinished homes. No margin of safety."
       :root-cause "No margin: assumed perpetual growth, maximal leverage, minimal cash reserves. No buffer for downturn."
       :should-have-done "Maintain debt-to-equity ratio < 2:1. Build cash reserves for 12+ months operations. Stress test for revenue decline."
       :sources ["Company financial statements", "Chinese regulatory filings", "Financial press analysis"]}
      {:name "FTX Collapse"
       :year 2022
       :entity "FTX Cryptocurrency Exchange"
       :description "Used customer deposits as collateral for risky bets. No separation of funds, no reserves. Bank run caused instant collapse. $8B+ missing."
       :quantitative-impact "$8B+ customer funds missing. $32B peak valuation → $0. Criminal charges. 1M+ creditors."
       :root-cause "No margin: 100% of customer funds at risk. No reserves, no separation. One bad bet = insolvency."
       :should-have-done "Maintain 100% reserves for customer deposits. Separate customer funds. Build capital buffer."
       :sources ["Bankruptcy filings", "DOJ indictment", "FTX examiner reports"]}]
     :quantitative-thresholds
     [{:metric "Financial Margin Ratio"
       :warning-threshold "< 20% cash reserves relative to monthly burn"
       :critical-threshold "< 10% reserves"
       :measurement "Cash reserves / monthly operating expenses"
       :frequency "Monthly"
       :example "$100K monthly burn, only $80K in reserves (0.8 months, critical)"}
      {:metric "Schedule Buffer"
       :warning-threshold "< 20% time buffer on critical path"
       :critical-threshold "< 10% buffer"
       :measurement "(Planned time - minimum time) / planned time"
       :frequency "Per project"
       :example "Project planned for 100 days, critical path is 95 days (5% buffer, critical)"}
      {:metric "Capacity Utilization"
       :warning-threshold "> 90% of capacity"
       :critical-threshold "> 95% of capacity"
       :measurement "Current load / maximum capacity"
       :frequency "Continuous monitoring"
       :example "Manufacturing at 97% capacity, no margin for demand spike or equipment failure"}
      {:metric "Leverage Ratio"
       :warning-threshold "> 5:1 debt to equity"
       :critical-threshold "> 10:1"
       :measurement "Total liabilities / equity"
       :frequency "Quarterly"
       :example "$100M equity, $800M debt (8:1 leverage, warning level)"}]
     :environmental-triggers
     ["Competitive pressure to maximize efficiency"
      "Optimistic forecasts and planning"
      "Success bias from recent wins"
      "Pressure to meet aggressive targets"
      "Underestimation of uncertainty"
      "Overconfidence in models and predictions"]
     :structural-safeguards
     ["Mandatory buffer requirements (time, money, capacity)"
      "Stress testing for adverse scenarios"
      "Maximum leverage limits"
      "Minimum reserve requirements"
      "Redundancy in critical systems"
      "Conservative assumptions in planning"
      "Regular margin audits"]
     :cognitive-safeguards
     ["Ask: 'What if things go wrong?'"
      "Invert: 'What would cause catastrophic failure?'"
      "Assume Murphy's Law: if it can go wrong, it will"
      "Reference class forecasting: how long did similar projects really take?"
      "Add 50% buffer to estimates"
      "Never operate at maximum capacity"]
     :social-safeguards
     ["Culture valuing prudence over aggression"
      "Rewards for maintaining margins, not just maximizing efficiency"
      "Devil's advocate challenging optimistic assumptions"
      "External review of risk and margin adequacy"
      "Board oversight of leverage and reserves"]
     :recovery-protocols
     [{:step 1
       :action "Immediately assess actual margin situation"
       :responsible "CFO + risk management"
       :timeline "Within 24 hours"
       :success-criteria "Complete picture of buffers (cash, time, capacity)"
       :escalation "If margin is critically low, emergency board meeting"}
      {:step 2
       :action "Implement emergency margin restoration"
       :responsible "Executive team"
       :timeline "Within 1 week"
       :success-criteria "Margin restored to minimum safe levels"
       :escalation "If restoration not possible, consider asset sales, fundraising, or shutdown"}
      {:step 3
       :action "Root cause analysis of margin erosion"
       :responsible "Strategy team"
       :timeline "Within 2 weeks"
       :success-criteria "Understanding of how margin was lost"
       :escalation "If systematic issues found, overhaul planning and risk processes"}
      {:step 4
       :action "Implement structural margin requirements"
       :responsible "CFO + operations"
       :timeline "Within 1 month"
       :success-criteria "Policies ensuring adequate margins maintained"
       :escalation "If policies not followed, tie to compensation and performance reviews"}]
     :lollapalooza-risk 0.95
     :amplifying-biases ["optimism-bias" "planning-fallacy" "overconfidence" "illusion-of-control"]
     :mitigating-models ["inversion" "second-order-thinking" "stress-testing" "scenario-analysis" "murphy's-law"]}
    
    {:name "insufficient-margin"
     :severity "high"
     :description "Having some margin but not enough for the level of uncertainty and risk involved. Margin adequate for normal times but not for stress."
     :mechanism "Underestimating the range of possible outcomes. Margin sized for expected scenarios, not worst-case."
     :psychological-root "Anchoring on normal conditions. Difficulty imagining extreme scenarios. Normalcy bias."
     :evolutionary-origin "Extreme events were rare in ancestral environments. Planning for normal conditions was usually sufficient."
     :signals
     ["Margin adequate for 'normal' scenarios only"
      "No stress testing for extreme scenarios"
      "Margin based on historical averages"
      "Surprise when 'black swan' events occur"
      "Margin consumed quickly in stress"]
     :case-studies
     [{:name "Bear Stearns Collapse"
       :year 2008
       :entity "Bear Stearns"
       :description "Had capital buffers adequate for normal times. Stress scenario (housing crash + liquidity crisis) exceeded margin. Collapsed in days."
       :quantitative-impact "$18B market cap → $1.2B fire sale to JPMorgan. 14,000 jobs. Margin was insufficient for stress."
       :root-cause "Margin sized for normal market conditions, not for housing crash + credit freeze + bank run."
       :should-have-done "Stress test for extreme scenarios. Size margin for 3-sigma events, not 1-sigma."
       :sources ["SEC investigation", "Financial Crisis Inquiry Report", "Cohan, W. House of Cards (2009)"]}
      {:name "Texas Power Grid Failure"
       :year 2021
       :entity "ERCOT (Texas Grid Operator)"
       :description "Grid had margin for normal Texas winters. Extreme cold exceeded margin. Rolling blackouts, 246 deaths, $130B in damages."
       :quantitative-impact "246 deaths. $130B in damages. 4.5M without power. Grid margin insufficient for extreme cold."
       :root-cause "Margin sized for typical Texas winter, not for once-in-100-year cold snap."
       :should-have-done "Size margin for extreme scenarios. Winterize infrastructure. Maintain larger reserves."
       :sources ["FERC/NERC investigation", "Texas state reports", "University of Texas analysis"]}]
     :quantitative-thresholds
     [{:metric "Stress Test Survival"
       :warning-threshold "Fails 1-in-10-year stress scenario"
       :critical-threshold "Fails 1-in-5-year scenario"
       :measurement "Can system survive X-year stress event?"
       :frequency "Annual stress testing"
       :example "Company would be insolvent in 1-in-7-year revenue decline"}
      {:metric "Margin Adequacy Ratio"
       :warning-threshold "< 2x expected worst-case"
       :critical-threshold "< 1.5x expected worst-case"
       :measurement "Actual margin / worst-case scenario requirement"
       :frequency "Quarterly review"
       :example "Have $500K reserves, worst-case scenario needs $400K (1.25x, critical)"}]
     :environmental-triggers
     ["Normalcy bias and recency bias"
      "Lack of historical perspective"
      "Pressure to minimize 'unproductive' buffers"
      "Overconfidence in risk models"
      "Underestimation of tail risk"]
     :structural-safeguards
     ["Stress testing for extreme scenarios (3-sigma, 5-sigma)"
      "Margin requirements based on worst-case, not average"
      "Historical analysis of extreme events"
      "Regular scenario planning"
      "Conservative assumptions"]
     :cognitive-safeguards
     ["Ask: 'What's the worst that could happen?'"
      "Study historical extremes, not just averages"
      "Plan for 100-year events, not just 10-year"
      "Assume correlations increase in crisis"
      "Reference class: how bad did it get for others?"]
     :social-safeguards
     ["External risk review"
      "Board-level stress testing oversight"
      "Culture of prudence and conservatism"
      "Rewards for maintaining adequate margins"]
     :recovery-protocols
     [{:step 1
       :action "Assess margin adequacy for extreme scenarios"
       :responsible "Risk management + external consultants"
       :timeline "Within 2 weeks"
       :success-criteria "Stress test results for 1-in-100-year scenarios"
       :escalation "If margin is inadequate, emergency capital raise or risk reduction"}
      {:step 2
       :action "Increase margin to adequate levels"
       :responsible "CFO + operations"
       :timeline "Within 3 months"
       :success-criteria "Margin sufficient for extreme scenarios"
       :escalation "If not achievable, reduce risk exposure or scale"}
      {:step 3
       :action "Implement ongoing stress testing"
       :responsible "Risk management"
       :timeline "Quarterly ongoing"
       :success-criteria "Regular stress tests, margin maintained"
       :escalation "If stress tests show problems, immediate action"}]
     :lollapalooza-risk 0.85
     :amplifying-biases ["normalcy-bias" "recency-bias" "underestimation-of-extremes"]
     :mitigating-models ["stress-testing" "scenario-analysis" "fat-tails" "black-swan-thinking"]}
    
    {:name "margin-erosion"
     :severity "medium"
     :description "Starting with adequate margin but allowing it to erode over time through complacency, pressure, or gradual risk accumulation."
     :mechanism "Boiling frog effect. Small incremental changes don't trigger alarm. Margin gradually consumed."
     :psychological-root "Adaptation to risk. Normalization of deviance. Complacency after success."
     :evolutionary-origin "Gradual environmental changes were rare. Humans adapted to stable environments."
     :signals
     ["Margin was adequate but is now thin"
      "Gradual increase in leverage or utilization"
      "Incremental risk-taking"
      "Normalization of tight margins"
      "Complacency about buffers"
      "Pressure to 'use' excess margin"]
     :safeguards ["Regular margin audits" "Alerts for margin decline" "Policies preventing margin erosion" "Culture of maintaining buffers"]}
    
    {:name "false-margin"
     :severity "high"
     :description "Believing you have margin when you don't, due to correlated risks, hidden dependencies, or illiquid assets."
     :mechanism "Diversification illusion. Assets appear uncorrelated in normal times but correlate in crisis."
     :psychological-root "Overconfidence in risk models. Failure to consider correlation in stress."
     :evolutionary-origin "Independent risks were more common in ancestral environments. Systemic correlation is modern."
     :signals
     ["Diversification assumed to provide margin"
      "Liquidity assumed in stress"
      "Correlations assumed stable"
      "Surprise when 'diversified' portfolio crashes together"
      "Hidden dependencies not recognized"]
     :safeguards ["Stress test for correlation increases" "Liquidity analysis" "Identify hidden dependencies" "Conservative correlation assumptions"]}
    
    {:name "excessive-margin"
     :severity "low"
     :description "Maintaining such large margins that opportunities are missed and resources are wasted. Over-optimization for safety."
     :mechanism "Extreme risk aversion. Trauma from past failures. Inability to calibrate appropriate margin."
     :psychological-root "Loss aversion. Fear and anxiety. Overreaction to past failures."
     :evolutionary-origin "Excessive caution prevented death but also led to starvation. Balance was needed."
     :signals
     ["Huge cash reserves earning nothing"
      "Massive overcapacity"
      "Missed opportunities due to excessive caution"
      "Competitors taking market share"
      "Resources sitting idle"]
     :safeguards ["Opportunity cost analysis" "Calibrate margin to risk level" "Portfolio approach (some risk acceptable)" "Regular margin optimization"]}]})



;; Enhanced Model: Circle of Competence
;; Munger: "Know the edge of your competence"
(def circle-of-competence-enhanced
  {:name "circle-of-competence"
   :category "decision_making"
   :description "Know what you know and what you don't know. Stay within your circle of competence. The size of the circle is less important than knowing its boundaries."
   :key-insight "You don't have to be an expert on every company, or even many. You only have to be able to evaluate companies within your circle of competence. The size of that circle is not very important; knowing its boundaries, however, is vital."
   :application "Before making decisions: (1) Assess if this is within your competence, (2) If yes, proceed with confidence, (3) If no, either learn or defer to experts, (4) Never pretend competence you don't have."
   :munger-quote "You have to figure out what your own aptitudes are. If you play games where other people have the aptitudes and you don't, you're going to lose. And that's as close to certain as any prediction that you can make. You have to figure out where you've got an edge. And you've got to play within your own circle of competence."
   :failure-modes
   [{:name "overestimating-competence"
     :severity "high"
     :description "Believing you understand something when you don't, leading to decisions in areas where you lack expertise."
     :mechanism "Dunning-Kruger effect: low competence leads to overconfidence. Illusion of knowledge from superficial understanding."
     :psychological-root "Overconfidence bias, illusion of knowledge, inability to recognize own ignorance."
     :evolutionary-origin "Confidence was adaptive for leadership and mating. Admitting ignorance was costly socially."
     :signals
     ["Making decisions in unfamiliar domains"
      "Dismissing expert advice"
      "Overconfidence in predictions"
      "Surprise when things go wrong"
      "Inability to explain reasoning deeply"
      "Relying on surface-level knowledge"
      "Not seeking expert input"]
     :case-studies
     [{:name "GE's Disastrous Expansion"
       :year 2000-2017
       :entity "General Electric"
       :description "GE, expert in industrial equipment, expanded into financial services (GE Capital). Lacked competence in finance. 2008 crisis nearly destroyed company. Lost $460B in market cap."
       :quantitative-impact "$460B market cap loss (peak $600B → $140B). GE Capital required $140B+ bailout. Near bankruptcy."
       :root-cause "Overestimated competence in financial services. Industrial company thought it understood complex finance."
       :should-have-done "Recognize finance was outside circle of competence. Stick to industrial strengths or hire true experts."
       :sources ["GE annual reports", "Financial Crisis Inquiry Report", "Gryta & Mann, Lights Out (2020)"]}
      {:name "Microsoft's Mobile Phone Failure"
       :year 2013-2015
       :entity "Microsoft"
       :description "Microsoft, expert in PC software, bought Nokia for $7.2B to enter mobile. Lacked competence in mobile hardware/ecosystem. Wrote off $7.6B."
       :quantitative-impact "$7.6B write-off. $7.2B acquisition wasted. Mobile division shut down. 7,800 jobs cut."
       :root-cause "Overestimated competence in mobile. PC software expertise didn't transfer to mobile ecosystem."
       :should-have-done "Recognize mobile was different competence. Partner instead of acquire, or stay in PC strengths."
       :sources ["Microsoft financial statements", "Nadella, S. Hit Refresh (2017)", "SEC filings"]}
      {:name "Quibi Streaming Failure"
       :year 2020
       :entity "Quibi"
       :description "Hollywood executives (Katzenberg, Whitman) thought entertainment expertise would work in mobile streaming. Raised $1.75B, shut down in 6 months."
       :quantitative-impact "$1.75B raised, nearly all lost. Shut down after 6 months. 200 employees laid off."
       :root-cause "Overestimated competence. Hollywood expertise didn't transfer to mobile-first streaming."
       :should-have-done "Recognize mobile streaming required different competence. Test assumptions before $1.75B commitment."
       :sources ["Company announcements", "Wall Street Journal coverage", "Post-mortem analyses"]}
      {:name "Daimler-Chrysler Merger Disaster"
       :year 1998-2007
       :entity "Daimler-Benz"
       :description "Daimler (luxury German cars) thought expertise would work with Chrysler (American mass market). Cultural and operational incompetence. Lost $36B."
       :quantitative-impact "$36B merger → sold for $7.4B (9 years later). $28B+ loss. Called 'worst merger in history.'"
       :root-cause "Overestimated competence in American mass-market cars. Luxury German expertise didn't transfer."
       :should-have-done "Recognize different markets require different competence. Stick to luxury segment."
       :sources ["Vlasic & Stertz, Taken for a Ride (2000)", "Company financial statements", "Business case studies"]}
      {:name "Yahoo's Failed Acquisitions"
       :year 2005-2016
       :entity "Yahoo"
       :description "Yahoo (portal/search) bought dozens of companies in areas outside competence (social, video, etc.). Failed to integrate or grow them. Wasted billions."
       :quantitative-impact "Spent $6B+ on acquisitions (Tumblr $1.1B, Broadcast.com $5.7B, etc.). Most written off. Yahoo sold for $4.8B."
       :root-cause "Overestimated competence in social media, video, mobile. Portal expertise didn't transfer."
       :should-have-done "Recognize limits of competence. Focus on core strengths or hire true experts to run acquisitions."
       :sources ["Yahoo financial statements", "Carlson, N. Marissa Mayer and the Fight to Save Yahoo (2014)"]}]
     :quantitative-thresholds
     [{:metric "Competence Assessment Score"
       :warning-threshold "< 7/10 on competence rubric"
       :critical-threshold "< 5/10"
       :measurement "Self-assessed competence on: deep knowledge, experience, track record, expert validation"
       :frequency "Per major decision outside core area"
       :example "Scored 4/10 on competence in new market (critical - should not proceed)"}
      {:metric "Expert Consultation Rate"
       :warning-threshold "< 50% of outside-competence decisions involve experts"
       :critical-threshold "< 25%"
       :measurement "Percentage of decisions outside core competence where experts were consulted"
       :frequency "Quarterly review"
       :example "Made 10 decisions outside core, only consulted experts on 2 (20%, critical)"}
      {:metric "Success Rate Outside Competence"
       :warning-threshold "< 40% success rate"
       :critical-threshold "< 25% success rate"
       :measurement "Percentage of outside-competence initiatives that succeeded"
       :frequency "Annual review"
       :example "8 initiatives outside core, only 1 succeeded (12.5%, critical)"}]
     :environmental-triggers
     ["Success in one domain creating overconfidence"
      "Pressure to grow beyond core business"
      "Attractive opportunities in unfamiliar areas"
      "Dismissal of domain-specific expertise"
      "Belief that general intelligence transfers"]
     :structural-safeguards
     ["Competence assessment required before major decisions"
      "Expert review for outside-competence decisions"
      "Track record analysis in new domains"
      "Small pilots before large commitments"
      "Hiring true experts, not generalists"
      "Board oversight of competence boundaries"]
     :cognitive-safeguards
     ["Ask: 'Do I really understand this deeply?'"
      "Test: 'Can I explain this to an expert?'"
      "Invert: 'What don't I know about this?'"
      "Reference: 'What's my track record in this domain?'"
      "Humility: 'Who knows this better than me?'"]
     :social-safeguards
     ["Culture valuing intellectual humility"
      "Rewards for staying within competence"
      "No penalties for saying 'I don't know'"
      "Expert advisors with veto power"
      "Diverse teams with complementary expertise"]
     :recovery-protocols
     [{:step 1
       :action "Immediately assess actual competence level"
       :responsible "Leadership + external experts"
       :timeline "Within 1 week"
       :success-criteria "Honest assessment of competence gaps"
       :escalation "If competence is critically low, halt initiative"}
      {:step 2
       :action "Hire true experts or exit domain"
       :responsible "CEO + board"
       :timeline "Within 1 month"
       :success-criteria "Experts hired with full authority, or exit decision made"
       :escalation "If can't hire experts and can't exit, forced liquidation"}
      {:step 3
       :action "Implement competence assessment process"
       :responsible "Strategy team"
       :timeline "Within 2 months"
       :success-criteria "Mandatory competence assessment for all major decisions"
       :escalation "If process not followed, tie to performance reviews"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["dunning-kruger" "overconfidence" "illusion-of-knowledge" "confirmation-bias"]
     :mitigating-models ["intellectual-humility" "first-principles" "expert-consultation" "pilot-testing"]}
    
    {:name "underestimating-competence"
     :severity "medium"
     :description "Believing something is outside your competence when you actually have the skills, leading to missed opportunities."
     :mechanism "Impostor syndrome, excessive humility, underconfidence from past failures."
     :psychological-root "Impostor syndrome, anxiety, underconfidence, fear of failure."
     :evolutionary-origin "Underconfidence prevented costly failures. Overconfidence was more dangerous than underconfidence."
     :signals
     ["Excessive deference to others"
      "Missed opportunities in areas of actual strength"
      "Overreliance on consultants"
      "Inability to recognize own expertise"
      "Constant self-doubt"]
     :safeguards ["Objective competence assessment" "Track record review" "Peer validation" "Confidence calibration"]}
    
    {:name "static-circle"
     :severity "medium"
     :description "Failing to expand circle of competence over time, missing opportunities to learn and grow."
     :mechanism "Comfort zone bias. Fear of learning new things. Overvaluing existing expertise."
     :psychological-root "Status quo bias, fear of incompetence, comfort with familiar."
     :evolutionary-origin "Specialization was efficient. Generalists were less successful than specialists."
     :signals
     ["No new skills or domains learned"
      "Declining relevance as world changes"
      "Missed opportunities in adjacent areas"
      "Competitors learning and adapting faster"
      "Obsolescence of existing competence"]
     :safeguards ["Continuous learning culture" "Adjacent domain exploration" "Calculated expansion" "Mentorship and training"]}
    
    {:name "fuzzy-boundaries"
     :severity "high"
     :description "Not clearly defining the boundaries of competence, leading to drift into incompetence without realizing it."
     :mechanism "Gradual drift from core competence. Lack of clear boundaries. Incremental expansion without validation."
     :psychological-root "Overconfidence from success. Failure to recognize when crossing boundaries."
     :evolutionary-origin "Gradual environmental changes were rare. Clear boundaries were obvious."
     :signals
     ["Unclear where competence ends"
      "Gradual drift into unfamiliar territory"
      "Declining success rates"
      "Surprise at failures"
      "No explicit competence definition"]
     :safeguards ["Explicit competence definition" "Regular boundary review" "Success rate monitoring" "Expert validation of boundaries"]}
    
    {:name "false-competence"
     :severity "high"
     :description "Believing you have competence based on credentials, titles, or past success rather than actual deep knowledge and track record."
     :mechanism "Confusing credentials with competence. Assuming past success guarantees future success."
     :psychological-root "Halo effect from credentials. Overconfidence from past success."
     :evolutionary-origin "In small tribes, past success was good predictor. Modern complex domains are different."
     :signals
     ["Relying on credentials rather than knowledge"
      "Assuming past success transfers"
      "Inability to demonstrate deep understanding"
      "Surprise when expertise doesn't transfer"
      "Defensive when competence questioned"]
     :safeguards ["Competence validation through demonstration" "Track record in specific domain" "Expert peer review" "Continuous learning requirements"]}]})

;; Enhanced Model: Opportunity Cost
;; Every choice has a cost - what you give up
(def opportunity-cost-enhanced
  {:name "opportunity-cost"
   :category "decision_making"
   :description "The cost of any decision is what you give up by not choosing the next best alternative. Every choice has an opportunity cost."
   :key-insight "There's no such thing as a free lunch. Every decision means saying no to something else. The real cost is what you sacrifice."
   :application "For every decision: (1) Identify the next best alternative, (2) Calculate what you give up, (3) Compare the value of what you get vs what you sacrifice, (4) Choose only if the gain exceeds the opportunity cost."
   :munger-quote "The first rule of fishing: fish where the fish are. The first rule of investing: don't do things that are outside your circle of competence. The opportunity cost of doing stupid things is enormous."
   :failure-modes
   [{:name "ignoring-opportunity-cost"
     :severity "high"
     :description "Evaluating decisions in isolation without considering what else could be done with the same resources."
     :mechanism "Mental accounting treats each decision separately. Failure to consider alternatives."
     :psychological-root "Narrow framing, mental accounting, failure to consider alternatives."
     :evolutionary-origin "In simple environments, alternatives were obvious. Modern complex choices require explicit comparison."
     :signals
     ["Decisions made without considering alternatives"
      "Focus on absolute returns, not relative"
      "No comparison to next best option"
      "Surprise at poor relative performance"
      "Resources allocated without prioritization"
      "Everything seems like a good idea in isolation"]
     :case-studies
     [{:name "Sears' Failed Digital Transformation"
       :year 2005-2018
       :entity "Sears Holdings"
       :description "Sears invested billions in physical stores while Amazon invested in e-commerce. Opportunity cost: could have built digital empire. Instead went bankrupt."
       :quantitative-impact "Spent $6B+ on store maintenance. Opportunity cost: could have built Amazon competitor. Sears bankrupt (2018), Amazon worth $1.7T."
       :root-cause "Ignored opportunity cost of investing in declining stores vs building digital capabilities."
       :should-have-done "Compare opportunity cost: $1B in stores vs $1B in digital. Digital had far higher return."
       :sources ["Sears bankruptcy filings", "Lampert testimony", "Retail industry analysis"]}
      {:name "Blockbuster Rejecting Netflix"
       :year 2000
       :entity "Blockbuster Video"
       :description "Blockbuster could have bought Netflix for $50M in 2000. Rejected it. Opportunity cost: Netflix now worth $280B. Blockbuster bankrupt."
       :quantitative-impact "Passed on $50M investment. Opportunity cost: $280B+ (5,600x return). Blockbuster bankrupt (2010)."
       :root-cause "Ignored opportunity cost of $50M investment vs continuing to invest in physical stores."
       :should-have-done "Calculate opportunity cost: $50M in Netflix vs $50M in stores. Netflix had unlimited upside."
       :sources ["Keyes, J. Netflix vs Blockbuster case study", "Antioco, J. Harvard Business Review (2011)"]}
      {:name "Kodak's Digital Camera Delay"
       :year 1975-2000
       :entity "Kodak"
       :description "Kodak invented digital camera (1975) but didn't invest, protecting film business. Opportunity cost: could have owned digital photography. Went bankrupt."
       :quantitative-impact "Opportunity cost of not investing in digital: lost entire $31B market cap. Bankrupt (2012)."
       :root-cause "Ignored opportunity cost: invest in digital (uncertain) vs protect film (certain decline)."
       :should-have-done "Calculate opportunity cost: film business was dying anyway. Digital was only option."
       :sources ["Lucas & Goh, Harvard Business Review (2009)", "Bankruptcy filings", "Munir & Phillips, Organization Science (2005)"]}
      {:name "Yahoo Passing on Google and Facebook"
       :year 1998-2006
       :entity "Yahoo"
       :description "Yahoo could have bought Google ($1M, 1998) and Facebook ($1B, 2006). Passed on both. Opportunity cost: $2.5T+ in value."
       :quantitative-impact "Opportunity cost: Google ($1.7T) + Facebook ($800B) = $2.5T. Yahoo sold for $4.8B."
       :root-cause "Ignored opportunity cost of small investments vs continuing existing strategy."
       :should-have-done "Calculate opportunity cost: $1M or $1B vs potential to own next-generation platforms."
       :sources ["Carlson, N. Business Insider (2011)", "Vise, D. The Google Story (2005)"]}
      {:name "Nokia Rejecting Android"
       :year 2007-2010
       :entity "Nokia"
       :description "Nokia dominated mobile phones (40% market share). Rejected Android, stuck with Symbian. Opportunity cost: could have been Samsung. Lost everything."
       :quantitative-impact "40% market share → 3% → sold to Microsoft for $7.2B (peak value $300B). Opportunity cost: $293B."
       :root-cause "Ignored opportunity cost of investing in Android vs protecting Symbian."
       :should-have-done "Calculate opportunity cost: Symbian was dying. Android was the future."
       :sources ["Doz & Wilson, Ringtone (2017)", "Nokia financial statements", "Microsoft acquisition documents"]}]
     :quantitative-thresholds
     [{:metric "Opportunity Cost Analysis Rate"
       :warning-threshold "< 60% of major decisions include opportunity cost analysis"
       :critical-threshold "< 40%"
       :measurement "Percentage of major decisions with documented opportunity cost analysis"
       :frequency "Per major decision"
       :example "Made 10 major decisions, only 3 included opportunity cost analysis (30%, critical)"}
      {:metric "Relative Return vs Best Alternative"
       :warning-threshold "< 80% of best alternative return"
       :critical-threshold "< 60%"
       :measurement "Actual return / return of best alternative"
       :frequency "Annual portfolio review"
       :example "Invested in A (10% return), best alternative B was 20% return (50% relative, critical)"}
      {:metric "Resource Allocation Efficiency"
       :warning-threshold "< 70% of resources in top opportunities"
       :critical-threshold "< 50%"
       :measurement "Percentage of resources allocated to highest-return opportunities"
       :frequency "Quarterly review"
       :example "Only 40% of budget in top opportunities, 60% in mediocre projects (critical)"}]
     :environmental-triggers
     ["Narrow framing of decisions"
      "Sunk cost fallacy (past investments)"
      "Status quo bias"
      "Lack of explicit prioritization"
      "No systematic opportunity comparison"]
     :structural-safeguards
     ["Mandatory opportunity cost analysis"
      "Explicit comparison to next best alternative"
      "Portfolio approach to resource allocation"
      "Regular reallocation based on opportunity cost"
      "Kill underperforming projects to free resources"
      "Zero-based budgeting"]
     :cognitive-safeguards
     ["Ask: 'What else could we do with these resources?'"
      "Compare: 'What's the next best alternative?'"
      "Calculate: 'What do we give up by choosing this?'"
      "Prioritize: 'Is this the highest-value use of resources?'"
      "Reallocate: 'Should we move resources to better opportunities?'"]
     :social-safeguards
     ["Culture of opportunity cost thinking"
      "Rewards for reallocating to better opportunities"
      "No penalties for killing projects"
      "Transparent opportunity comparison"
      "External benchmarking"]
     :recovery-protocols
     [{:step 1
       :action "Immediately assess opportunity cost of current resource allocation"
       :responsible "Strategy team + finance"
       :timeline "Within 2 weeks"
       :success-criteria "Complete opportunity cost analysis of all major initiatives"
       :escalation "If major misallocations found, emergency reallocation"}
      {:step 2
       :action "Reallocate resources to highest-value opportunities"
       :responsible "Executive team"
       :timeline "Within 1 month"
       :success-criteria "Resources shifted to top opportunities"
       :escalation "If reallocation blocked, escalate to board"}
      {:step 3
       :action "Implement ongoing opportunity cost process"
       :responsible "Strategy + finance"
       :timeline "Within 2 months"
       :success-criteria "Quarterly opportunity cost reviews, systematic reallocation"
       :escalation "If process not followed, tie to performance reviews"}]
     :lollapalooza-risk 0.85
     :amplifying-biases ["narrow-framing" "sunk-cost-fallacy" "status-quo-bias" "mental-accounting"]
     :mitigating-models ["portfolio-thinking" "prioritization" "zero-based-budgeting" "reallocation"]}
    
    {:name "sunk-cost-fallacy"
     :severity "high"
     :description "Continuing to invest in something because of past investments, ignoring that those costs are sunk and the opportunity cost is what matters."
     :mechanism "Emotional attachment to past investments. Desire to avoid admitting mistakes. Escalation of commitment."
     :psychological-root "Loss aversion, commitment bias, desire to avoid regret."
     :evolutionary-origin "Persistence was often adaptive. Giving up too easily led to failure."
     :signals
     ["Continuing failing projects because 'we've already invested so much'"
      "Throwing good money after bad"
      "Escalation of commitment"
      "Inability to cut losses"
      "Justifying continued investment with past costs"]
     :safeguards ["Ignore sunk costs in decisions" "Focus on forward-looking opportunity cost" "Blameless post-mortems" "Rewards for cutting losses early"]}
    
    {:name "implicit-opportunity-cost"
     :severity "medium"
     :description "Failing to recognize implicit opportunity costs like time, attention, and energy, not just money."
     :mechanism "Focus on explicit monetary costs while ignoring implicit costs of time and attention."
     :psychological-root "Money is salient and measurable. Time and attention are less tangible."
     :evolutionary-origin "Time was abundant in ancestral environments. Modern scarcity of attention is new."
     :signals
     ["Saying yes to everything"
      "Overcommitment"
      "Lack of focus"
      "Spreading resources too thin"
      "Not valuing time and attention"]
     :safeguards ["Explicit time and attention budgets" "Saying no to good opportunities for great ones" "Focus and prioritization" "Opportunity cost of time"]}
    
    {:name "local-optimization"
     :severity "medium"
     :description "Optimizing individual decisions without considering portfolio-level opportunity costs."
     :mechanism "Each decision looks good in isolation but portfolio is suboptimal."
     :psychological-root "Narrow framing, inability to see system-level effects."
     :evolutionary-origin "Local optimization was sufficient in simple environments."
     :signals
     ["Every project looks good individually"
      "Portfolio underperforms"
      "Resources spread too thin"
      "No prioritization"
      "Doing too many things poorly"]
     :safeguards ["Portfolio-level optimization" "Explicit prioritization" "Concentration in best opportunities" "Regular reallocation"]}
    
    {:name "status-quo-opportunity-cost"
     :severity "high"
     :description "Failing to recognize that maintaining the status quo has opportunity cost - what you could gain by changing."
     :mechanism "Status quo bias. Inaction feels safe but has hidden opportunity cost."
     :psychological-root "Loss aversion, status quo bias, fear of change."
     :evolutionary-origin "Status quo was often safe. Change was risky."
     :signals
     ["Inaction despite better alternatives"
      "Maintaining failing strategies"
      "Fear of change"
      "Competitors gaining ground"
      "Missed opportunities due to inaction"]
     :safeguards ["Invert: 'What's the cost of not changing?'" "Opportunity cost of status quo" "Regular strategy reviews" "Bias toward action when opportunity cost is high"]}]})

;; Enhanced Model: Systems Thinking
;; Understanding interconnections, feedback loops, and emergent properties
(def systems-thinking-enhanced
  {:name "systems-thinking"
   :category "decision_making"
   :description "Understanding how parts of a system interact and influence each other over time, including feedback loops, delays, and emergent properties."
   :key-insight "You can't understand a system by only looking at its parts. The interactions between parts create behaviors that don't exist in any single component."
   :application "Map the system: identify components, relationships, feedback loops, delays, and leverage points. Look for unintended consequences and emergent properties."
   :munger-quote "I think it is undeniably true that the human brain must work in models. The trick is to have your brain work better than the other person's brain because it understands the most fundamental models."
   :failure-modes
   [{:name "reductionist-thinking"
     :severity "high"
     :description "Breaking complex systems into parts and optimizing each part independently, destroying emergent properties and system-level performance."
     :mechanism "Analytical reduction works for simple systems but fails for complex adaptive systems where interactions matter more than components."
     :psychological-root "Reductionism is cognitively easier. Understanding whole systems requires holding multiple relationships in mind simultaneously."
     :evolutionary-origin "Ancestral problems were mostly simple cause-effect. Complex systems thinking is recent evolutionary demand."
     :signals
     ["Optimizing departments independently"
      "Ignoring cross-functional dependencies"
      "Local optimization hurting global performance"
      "Surprise at system-level failures"
      "Inability to explain emergent behaviors"
      "Treating symptoms instead of root causes"
      "Reorganizations that don't improve performance"
      "Metrics improving while outcomes worsen"]
     :case-studies
     [{:name "Soviet Nail Factory"
       :year 1960
       :entity "Soviet Central Planning"
       :description "Factory given quota by weight produced only heavy nails. Changed to quota by quantity, produced only tiny useless nails. Optimized metric, destroyed system purpose."
       :quantitative-impact "Massive waste of resources. Chronic shortage of useful nails throughout Soviet economy."
       :root-cause "Reductionist optimization of single metric without understanding system purpose and interactions."
       :should-have-done "Define system purpose (useful nails for construction), measure outcomes not outputs, allow market feedback."
       :sources ["Kornai, J. The Socialist System (1992)", "Soviet economic planning literature"]}
      {:name "US Healthcare System"
       :year "2000-present"
       :entity "US Healthcare Industry"
       :description "Each component optimized independently: hospitals for bed utilization, doctors for patient volume, insurers for claim denial, pharma for drug prices. System-level outcome: highest cost, mediocre results."
       :quantitative-impact "$4.3T annual spending (18% of GDP), 50% higher per capita than other developed nations. Outcomes: lower life expectancy, higher infant mortality."
       :root-cause "Reductionist optimization of each component without system-level design. Perverse incentives at every interface."
       :should-have-done "Design for system-level outcomes (health, not healthcare). Align incentives across components. Measure what matters."
       :sources ["Commonwealth Fund reports", "WHO health statistics", "OECD health data"]}
      {:name "Boeing 737 MAX"
       :year 2019
       :entity "Boeing Corporation"
       :description "Optimized for: low pilot training cost (reuse 737 certification), fuel efficiency (larger engines), production speed (minimal changes). System interaction: larger engines changed aerodynamics, MCAS software added, pilots not trained, crashes killed 346."
       :quantitative-impact "346 deaths, $20B+ in costs, 20-month grounding, criminal charges, reputation destroyed."
       :root-cause "Optimized individual components and constraints without understanding system-level safety implications and interactions."
       :should-have-done "System-level safety analysis, comprehensive pilot training, transparent MCAS documentation, proper certification."
       :sources ["Congressional investigation report", "NTSB reports", "DOJ criminal investigation"]}
      {:name "Wells Fargo Account Fraud"
       :year "2011-2016"
       :entity "Wells Fargo Bank"
       :description "Optimized sales metrics: accounts per customer, cross-selling ratios. Ignored system interactions: employee pressure, customer trust, regulatory risk. Employees opened 3.5M fake accounts."
       :quantitative-impact "$3B in fines, CEO resigned, 5,300 employees fired, reputation damage, customer exodus."
       :root-cause "Reductionist focus on sales metrics without understanding system-level effects on culture, ethics, and customer relationships."
       :should-have-done "System-level thinking: how do incentives affect behavior? What are unintended consequences? Monitor for gaming."
       :sources ["CFPB enforcement action", "Congressional testimony", "Independent investigation reports"]}
      {:name "Enron Energy Trading"
       :year 2001
       :entity "Enron Corporation"
       :description "Optimized mark-to-market accounting, SPV structures, and reported earnings independently. System interaction: accounting fraud enabled by complex structures, auditor conflicts, board failures."
       :quantitative-impact "$74B in shareholder losses, 20,000 jobs lost, pension funds destroyed, criminal convictions."
       :root-cause "Reductionist optimization of financial metrics without understanding system-level integrity and sustainability."
       :should-have-done "System-level analysis: Are earnings real? Can this be sustained? What are the risks? Independent oversight."
       :sources ["Powers Report (2002)", "Enron bankruptcy examiner report", "Criminal trial records"]}]
     :quantitative-thresholds
     [{:metric "Cross-Functional Dependencies Mapped"
       :warning-threshold "< 50% of major dependencies identified"
       :critical-threshold "< 25% identified"
       :measurement "Percentage of system interactions documented"
       :frequency "Per major initiative"
       :example "Identified 3 of 12 cross-functional dependencies in new product launch"}
      {:metric "System-Level Metrics vs Component Metrics"
       :warning-threshold "< 30% system-level metrics"
       :critical-threshold "< 10% system-level metrics"
       :measurement "Ratio of outcome metrics to output metrics"
       :frequency "Quarterly metric review"
       :example "Measuring department outputs but not customer outcomes"}]
     :environmental-triggers
     ["Siloed organizational structure"
      "Component-level incentives"
      "Short-term pressure"
      "Lack of systems expertise"
      "Complexity and time pressure"
      "Narrow functional expertise"]
     :structural-safeguards
     ["System dynamics modeling for major decisions"
      "Cross-functional teams with system authority"
      "System-level metrics and incentives"
      "Regular system audits"
      "Causal loop diagrams required"
      "End-to-end process ownership"
      "System architects in decision-making"]
     :cognitive-safeguards
     ["Map the full system before optimizing parts"
      "Identify feedback loops and delays"
      "Look for unintended consequences"
      "Ask: 'What happens when everyone does this?'"
      "Consider second and third-order effects"
      "Study system failures in similar domains"]
     :social-safeguards
     ["Diverse perspectives in system design"
      "External system reviews"
      "Stakeholder feedback loops"
      "Transparency and open discussion"
      "Challenge reductionist thinking"]
     :recovery-protocols
     [{:step 1
       :action "Map the actual system including all interactions"
       :responsible "Systems analysis team"
       :timeline "Within 1 week"
       :success-criteria "Complete system map with feedback loops"
       :escalation "If system too complex, bring in systems thinking expert"}
      {:step 2
       :action "Identify where reductionist optimization is causing system-level harm"
       :responsible "Cross-functional team"
       :timeline "Within 2 weeks"
       :success-criteria "Root causes identified with quantitative impact"
       :escalation "If causes unclear, conduct system simulation"}
      {:step 3
       :action "Redesign for system-level optimization"
       :responsible "Strategy and operations teams"
       :timeline "Within 1 month"
       :success-criteria "New design with aligned incentives and metrics"
       :escalation "If redesign insufficient, consider fundamental restructuring"}
      {:step 4
       :action "Implement with system-level monitoring"
       :responsible "All teams"
       :timeline "Within 2 months"
       :success-criteria "System-level metrics improving"
       :escalation "If not improving, return to step 2"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["confirmation-bias" "availability-heuristic" "dunning-kruger"]
     :mitigating-models ["second-order-thinking" "inversion" "feedback-loops" "unintended-consequences"]}
    
    {:name "ignoring-feedback-loops"
     :severity "critical"
     :description "Failing to recognize and account for reinforcing and balancing feedback loops that can cause exponential growth, collapse, or oscillation."
     :mechanism "Linear thinking assumes one-way causation. Feedback loops create circular causation where effects become causes."
     :psychological-root "Human cognition evolved for simple cause-effect. Circular causation is cognitively demanding and counter-intuitive."
     :evolutionary-origin "Most ancestral problems involved linear causation. Complex system dynamics with feedback loops are modern phenomena."
     :signals
     ["Surprise at exponential growth or collapse"
      "Failure to anticipate tipping points"
      "Boom-bust cycles"
      "Runaway processes"
      "Inability to explain why problems keep recurring"
      "Short-term fixes that worsen long-term problems"
      "Missing reinforcing loops (virtuous/vicious cycles)"
      "Missing balancing loops (goal-seeking, resistance)"]
     :case-studies
     [{:name "2008 Financial Crisis"
       :year "2007-2008"
       :entity "Global Financial System"
       :description "Reinforcing loop: Easy credit → rising home prices → more lending (collateral rising!) → more speculation → higher prices → more lending. Ignored balancing loop: affordability declining, defaults rising. Loop reversed: defaults → falling prices → more defaults → credit freeze → crash."
       :quantitative-impact "$7.4T in stock wealth destroyed, 8.7M jobs lost, 10M foreclosures, $498B in TARP bailouts, Great Recession."
       :root-cause "Ignored positive feedback loop amplifying risk. Assumed prices would keep rising. Missed tipping point."
       :should-have-done "Recognize bubble dynamics, model feedback loops, identify tipping points, tighten lending before crisis."
       :sources ["Financial Crisis Inquiry Report (2011)", "Federal Reserve studies", "IMF reports"]}
      {:name "Antibiotic Resistance"
       :year "1950-present"
       :entity "Global Healthcare"
       :description "Reinforcing loop: Antibiotic use → bacterial resistance → infections harder to treat → more antibiotic use → more resistance. Now 700,000 deaths/year, projected 10M/year by 2050."
       :quantitative-impact "700,000 annual deaths from resistant infections. $100B+ annual healthcare costs. Existential threat to modern medicine."
       :root-cause "Ignored feedback loop between antibiotic use and resistance. Treated antibiotics as unlimited resource."
       :should-have-done "Antibiotic stewardship, reserve drugs for severe cases, invest in alternatives, monitor resistance, model feedback dynamics."
       :sources ["O'Neill Report (2016)", "WHO Global Action Plan on AMR", "CDC reports"]}
      {:name "Social Media Polarization"
       :year "2010-present"
       :entity "Facebook, Twitter, YouTube"
       :description "Reinforcing loop: Engagement algorithm → shows extreme content (gets engagement) → users see more extreme content → become more extreme → engage more with extreme content → algorithm learns. Balancing loop ignored: societal polarization, mental health, democracy."
       :quantitative-impact "Measurable increase in political polarization, mental health crisis in teens, election interference, genocide incitement (Myanmar)."
       :root-cause "Optimized engagement metric without understanding feedback loop to extremism and societal harm."
       :should-have-done "Model feedback loops, optimize for well-being not engagement, add friction for extreme content, diversify recommendations."
       :sources ["Haugen testimony (2021)", "Academic studies on polarization", "Amnesty International reports"]}]
     :quantitative-thresholds
     [{:metric "Feedback Loops Identified"
       :warning-threshold "< 3 major loops identified"
       :critical-threshold "< 1 loop identified"
       :measurement "Number of reinforcing and balancing loops documented"
       :frequency "Per system change"
       :example "Identified 1 of 6 feedback loops in new incentive system"}]
     :environmental-triggers
     ["Complex systems with delays"
      "Multiple interacting agents"
      "Non-linear relationships"
      "Optimization for single metric"
      "Lack of system modeling"]
     :structural-safeguards
     ["Causal loop diagrams required"
      "System dynamics modeling"
      "Feedback loop monitoring"
      "Early warning systems"
      "Kill switches for runaway processes"
      "Gradual rollouts with monitoring"]
     :cognitive-safeguards
     ["Map reinforcing and balancing loops"
      "Identify time delays"
      "Look for tipping points"
      "Ask: 'What happens at scale?'"
      "Consider exponential dynamics"
      "Study historical feedback loop failures"]
     :social-safeguards
     ["Systems thinking expertise in decisions"
      "External review of system dynamics"
      "Stakeholder feedback mechanisms"
      "Regulatory oversight for systemic risks"]
     :recovery-protocols
     [{:step 1
       :action "Identify the feedback loop causing runaway behavior"
       :responsible "Systems analysis team"
       :timeline "Immediately"
       :success-criteria "Documented causal loop diagram"
       :escalation "If loop unclear, emergency expert consultation"}
      {:step 2
       :action "Determine intervention points to break or reverse loop"
       :responsible "Strategy team"
       :timeline "Within days"
       :success-criteria "Multiple intervention options with predicted effects"
       :escalation "If no interventions found, consider system shutdown"}
      {:step 3
       :action "Implement intervention with intensive monitoring"
       :responsible "Operations team"
       :timeline "Within 1 week"
       :success-criteria "Feedback loop weakening or reversing"
       :escalation "If ineffective, implement backup intervention"}
      {:step 4
       :action "Build permanent monitoring and safeguards"
       :responsible "Risk management"
       :timeline "Within 1 month"
       :success-criteria "Early warning system preventing recurrence"
       :escalation "If insufficient, redesign system"}]
     :lollapalooza-risk 0.95
     :amplifying-biases ["exponential-growth-bias" "optimism-bias" "confirmation-bias"]
     :mitigating-models ["second-order-thinking" "inversion" "margin-of-safety" "scenario-analysis"]}
    
    {:name "missing-leverage-points"
     :severity "high"
     :description "Failing to identify high-leverage intervention points in systems, leading to ineffective interventions or wasted effort."
     :mechanism "Not all intervention points are equal. Small changes at leverage points can cause large system changes. Most interventions target low-leverage points."
     :psychological-root "Leverage points are often counter-intuitive and non-obvious. Obvious interventions feel more actionable."
     :evolutionary-origin "Direct action on visible problems was effective in simple environments. Leverage points are modern concept."
     :signals
     ["Lots of effort, little system change"
      "Treating symptoms not root causes"
      "Interventions that don't stick"
      "Problems keep recurring"
      "Resources wasted on low-impact changes"
      "Missing structural vs symptomatic interventions"]
     :safeguards ["Leverage point analysis (Meadows)", "Root cause analysis", "System modeling", "Focus on structure not events", "Test interventions before scaling"]}
    
    {:name "ignoring-delays"
     :severity "medium"
     :description "Failing to account for time delays between actions and consequences, leading to overcorrection, oscillation, or premature abandonment."
     :mechanism "Delays between cause and effect obscure causation. Interventions take time to work. Impatience leads to overcorrection."
     :psychological-root "Humans expect immediate feedback. Delayed consequences are cognitively difficult to connect to causes."
     :evolutionary-origin "Ancestral cause-effect was mostly immediate. Long delays are modern phenomenon."
     :signals
     ["Frequent strategy changes"
      "Overcorrection and oscillation"
      "Impatience with initiatives"
      "Boom-bust cycles"
      "Abandoning strategies prematurely"
      "Not waiting for feedback"]
     :safeguards ["Patience protocols", "Time-series analysis", "Leading indicators", "Commitment devices", "Model expected delays", "Resist overcorrection"]}
    
    {:name "linear-thinking-in-non-linear-systems"
     :severity "high"
     :description "Assuming linear relationships when reality involves thresholds, tipping points, phase transitions, and exponential dynamics."
     :mechanism "Linear extrapolation is cognitively easy but wrong for non-linear systems. Thresholds and tipping points cause sudden regime changes."
     :psychological-root "Linear thinking is default. Non-linear dynamics are counter-intuitive and require mathematical modeling."
     :evolutionary-origin "Many ancestral relationships were approximately linear in normal ranges. Extreme non-linearity is modern."
     :signals
     ["Surprise at sudden changes"
      "Failure to anticipate tipping points"
      "Linear extrapolation errors"
      "Missing phase transitions"
      "Underestimating exponential growth"
      "Not recognizing thresholds"]
     :safeguards ["Non-linear modeling", "Threshold identification", "Sensitivity analysis", "Phase transition awareness", "Exponential thinking", "Scenario analysis"]}]})

;; Enhanced Model: First Principles Thinking
;; Breaking down complex problems to fundamental truths and reasoning up from there
(def first-principles-enhanced
  {:name "first-principles"
   :category "decision_making"
   :description "Break down complex problems into their most basic, foundational truths and reason up from there, rather than reasoning by analogy or convention."
   :key-insight "Don't accept conventional wisdom or analogies as truth. Question assumptions down to fundamental physics, economics, or human nature."
   :application "When facing a problem: (1) Identify and challenge all assumptions, (2) Break down to fundamental truths that cannot be deduced further, (3) Reason up from first principles to create new solutions."
   :munger-quote "I think it is undeniably true that the human brain must work in models. The trick is to have your brain work better than the other person's brain because it understands the most fundamental models."
   :failure-modes
   [{:name "reasoning-by-analogy"
     :severity "high"
     :description "Accepting 'this is how it's always been done' or 'this is like X' instead of questioning down to fundamental truths."
     :mechanism "Analogies and conventions are cognitively easy. First principles thinking requires hard mental work to question everything."
     :psychological-root "Social proof, authority bias, and cognitive ease. Brains conserve energy by accepting existing patterns."
     :evolutionary-origin "Following the tribe's conventions was survival-optimal. Questioning everything was dangerous and energy-intensive."
     :signals
     ["Frequent use of analogies in reasoning"
      "'That's how it's always done' justifications"
      "Accepting industry conventions without question"
      "Incremental thinking within existing paradigms"
      "Inability to explain why something must be true"
      "Following best practices without understanding"
      "Copying competitors without understanding fundamentals"
      "Resistance to questioning assumptions"]
     :case-studies
     [{:name "SpaceX Rocket Reusability"
       :year 2015
       :entity "SpaceX"
       :description "Rocket industry: 'Rockets are expendable, that's how it's always been.' Musk: 'What are rockets made of? Aerospace-grade aluminum alloys, titanium, carbon fiber. What's the material cost? ~$200K. Why does a launch cost $60M? Because we throw it away.' Built reusable rockets."
       :quantitative-impact "Launch cost reduced from $60M to $28M (53% reduction). Falcon 9 boosters reused 15+ times. Revolutionized space industry."
       :root-cause "Industry reasoned by analogy ('rockets have always been expendable') instead of first principles (material costs vs launch costs)."
       :should-have-done "Question: Why are rockets expendable? What's the fundamental physics? What's the material cost? Can we reuse?"
       :sources ["Musk interviews", "SpaceX technical papers", "Industry cost analyses"]}
      {:name "Tesla Electric Vehicles"
       :year 2008
       :entity "Tesla Motors"
       :description "Auto industry: 'EVs are golf carts, limited range, expensive batteries.' Musk: 'What are batteries made of? Lithium, cobalt, nickel, carbon. What's the material cost per kWh? Can we reduce it?' Built battery gigafactory, reduced cost 85%."
       :quantitative-impact "Battery cost: $1,000/kWh (2008) → $137/kWh (2020). Tesla market cap $800B+, forced entire auto industry to EVs."
       :root-cause "Industry reasoned by analogy ('EVs have always been expensive') instead of first principles (material costs, manufacturing scale)."
       :should-have-done "Question: Why are batteries expensive? What's the material cost? What's the manufacturing cost? Can we scale?"
       :sources ["Tesla battery day presentations", "BloombergNEF battery price survey", "Industry analyses"]}
      {:name "Airbnb Hotel Alternative"
       :year 2008
       :entity "Airbnb"
       :description "Hospitality industry: 'Lodging requires hotels, professional management, licenses.' Airbnb: 'What do travelers need? A place to sleep. What do homeowners have? Extra rooms. Can we connect them?'"
       :quantitative-impact "$100B+ valuation, 7M listings, disrupted $600B hotel industry, created new market."
       :root-cause "Industry reasoned by analogy ('lodging = hotels') instead of first principles (travelers need beds, homeowners have beds)."
       :should-have-done "Question: Why do we need hotels? What's the fundamental need? What resources exist? Can we match supply and demand?"
       :sources ["Airbnb founding story", "Market analyses", "Valuation reports"]}
      {:name "Netflix Streaming vs DVD"
       :year 2007
       :entity "Netflix"
       :description "Video rental industry: 'People rent physical media from stores or by mail.' Netflix: 'What do people want? Content. How can content be delivered? Internet bandwidth is growing. Can we stream?'"
       :quantitative-impact "$280B market cap, 230M subscribers, destroyed Blockbuster ($5B), revolutionized entertainment."
       :root-cause "Blockbuster reasoned by analogy ('video rental = physical stores') instead of first principles (content delivery via internet)."
       :should-have-done "Question: What's the fundamental need? Content access. What's the best delivery method given technology trends?"
       :sources ["Netflix history", "Blockbuster bankruptcy filing", "Streaming industry reports"]}
      {:name "Amazon AWS Cloud Computing"
       :year 2006
       :entity "Amazon Web Services"
       :description "Tech industry: 'Companies need their own data centers and servers.' Amazon: 'What do companies need? Compute and storage. We have excess capacity. Can we rent it?'"
       :quantitative-impact "$80B+ annual revenue, 32% of cloud market, $1T+ market created, changed how all software is built."
       :root-cause "Industry reasoned by analogy ('IT = own your servers') instead of first principles (compute as utility, economies of scale)."
       :should-have-done "Question: Why own servers? What's the fundamental need? Can compute be a utility like electricity?"
       :sources ["AWS history", "Cloud market reports", "Bezos shareholder letters"]}]
     :quantitative-thresholds
     [{:metric "Assumptions Challenged"
       :warning-threshold "< 30% of major assumptions questioned"
       :critical-threshold "< 10% questioned"
       :measurement "Percentage of key assumptions broken down to first principles"
       :frequency "Per major decision or strategy"
       :example "Accepted 9 of 10 industry assumptions without questioning fundamentals"}
      {:metric "Reasoning by Analogy vs First Principles"
       :warning-threshold "> 70% analogies"
       :critical-threshold "> 90% analogies"
       :measurement "Ratio of analogical reasoning to first principles reasoning in decision documents"
       :frequency "Per strategy document"
       :example "Strategy justified by 'like Uber but for X' without fundamental analysis"}]
     :environmental-triggers
     ["Strong industry conventions"
      "Time pressure"
      "Lack of technical depth"
      "Social proof and conformity pressure"
      "Authority figures defending status quo"
      "Complexity and cognitive load"]
     :structural-safeguards
     ["Mandatory assumption documentation"
      "First principles analysis required for major decisions"
      "Challenge sessions with technical experts"
      "Assumption testing and validation"
      "Diverse perspectives (outsiders, different industries)"
      "Innovation time for questioning fundamentals"]
     :cognitive-safeguards
     ["Ask: 'What are we assuming that might not be true?'"
      "Ask: 'What are the fundamental constraints? (physics, economics, human nature)'"
      "Ask: 'Why is this the way it is? Could it be different?'"
      "Break down to material/energy/information fundamentals"
      "Separate what's possible from what's conventional"
      "Study breakthrough innovations in other domains"]
     :social-safeguards
     ["Encourage questioning of sacred cows"
      "Reward innovative thinking"
      "Bring in outsiders without industry baggage"
      "Create psychological safety for challenging assumptions"
      "Study first principles thinkers (Musk, Bezos, Feynman)"]
     :recovery-protocols
     [{:step 1
       :action "Identify what assumptions were accepted without questioning"
       :responsible "Strategy team"
       :timeline "Within 1 week"
       :success-criteria "Complete list of unquestioned assumptions"
       :escalation "If assumptions unclear, bring in external perspective"}
      {:step 2
       :action "Break down each assumption to first principles"
       :responsible "Technical and strategy teams"
       :timeline "Within 2 weeks"
       :success-criteria "First principles analysis of each assumption"
       :escalation "If analysis insufficient, bring in domain experts"}
      {:step 3
       :action "Develop alternative approaches based on first principles"
       :responsible "Innovation team"
       :timeline "Within 1 month"
       :success-criteria "3+ alternative approaches that challenge conventions"
       :escalation "If no alternatives found, expand search to other industries"}
      {:step 4
       :action "Test and validate new approaches"
       :responsible "Product and operations teams"
       :timeline "Within 2 months"
       :success-criteria "Validated approach that outperforms conventional wisdom"
       :escalation "If validation fails, iterate on alternatives"}]
     :lollapalooza-risk 0.80
     :amplifying-biases ["authority-bias" "social-proof" "status-quo-bias" "confirmation-bias"]
     :mitigating-models ["inversion" "second-order-thinking" "circle-of-competence" "intellectual-humility"]}
    
    {:name "stopping-too-early"
     :severity "high"
     :description "Stopping the questioning process before reaching true first principles, accepting intermediate assumptions as fundamental."
     :mechanism "Questioning is hard work. People stop when they reach something that 'sounds fundamental' even if it can be broken down further."
     :psychological-root "Cognitive ease, satisficing, and social pressure to accept certain assumptions as given."
     :evolutionary-origin "Infinite questioning was impractical. 'Good enough' answers enabled action."
     :signals
     ["Accepting 'that's just how it is' explanations"
      "Stopping at disciplinary boundaries"
      "Not questioning foundational assumptions"
      "Accepting intermediate conclusions as fundamental"
      "Inability to explain at deeper levels"
      "Circular reasoning"]
     :safeguards ["Ask 'Why?' five times (Toyota 5 Whys)", "Question across disciplines", "Seek explanations at physics/economics/human nature level", "Test if assumption can be broken down further"]}
    
    {:name "ignoring-constraints"
     :severity "medium"
     :description "Reasoning from first principles without considering real-world constraints (physics, economics, human nature, regulations)."
     :mechanism "Pure first principles thinking can lead to solutions that violate fundamental constraints or are economically infeasible."
     :psychological-root "Optimism bias and lack of practical experience. Theory without reality testing."
     :evolutionary-origin "Theoretical thinking without practical constraints is modern luxury."
     :signals
     ["Solutions that violate physics"
      "Economically infeasible proposals"
      "Ignoring human nature"
      "Regulatory impossibilities"
      "'Sounds good in theory' solutions"
      "Lack of practical validation"]
     :safeguards ["Validate against fundamental constraints", "Economic feasibility analysis", "Physics/engineering review", "Human nature reality check", "Regulatory assessment"]}
    
    {:name "reinventing-the-wheel"
     :severity "medium"
     :description "Questioning everything including things that are actually well-optimized, wasting time on problems already solved."
     :mechanism "Not all conventions are wrong. Some represent accumulated wisdom and optimization. Questioning everything is inefficient."
     :psychological-root "Overconfidence in ability to improve on existing solutions. Not-invented-here syndrome."
     :evolutionary-origin "Respecting accumulated wisdom was survival-optimal. Pure individualism was risky."
     :signals
     ["Questioning well-optimized solutions"
      "Ignoring accumulated wisdom"
      "Reinventing inferior solutions"
      "Time wasted on solved problems"
      "Not-invented-here syndrome"
      "Arrogance about existing solutions"]
     :safeguards ["Chesterton's Fence: understand why before changing", "Study history of existing solutions", "Identify what's actually suboptimal vs well-optimized", "Balance innovation with respect for wisdom"]}
    
    {:name "analysis-paralysis"
     :severity "medium"
     :description "Questioning everything indefinitely without ever reaching conclusions or taking action."
     :mechanism "First principles thinking is intellectually satisfying but can prevent action. Perfect understanding is impossible."
     :psychological-root "Perfectionism, fear of being wrong, preference for thinking over doing."
     :evolutionary-origin "Overthinking was safer than premature action in some contexts."
     :signals
     ["Endless analysis without decisions"
      "Perfectionism in understanding"
      "Inability to act with uncertainty"
      "Missing opportunities due to overthinking"
      "Competitors moving faster"
      "Diminishing returns on analysis"]
     :safeguards ["Set decision deadlines", "Good enough is good enough", "Test and learn vs perfect understanding", "Reversible decisions need less analysis", "Bias toward action"]}]})

;; Export all enhanced models
(def enhanced-models
  [second-order-thinking-enhanced
   inversion-enhanced
   incentives-enhanced
   margin-of-safety-enhanced
   circle-of-competence-enhanced
   opportunity-cost-enhanced
   systems-thinking-enhanced
   first-principles-enhanced])

;; Enhanced Model: Asymmetric Risk/Reward
;; Seeking opportunities where upside vastly exceeds downside
(def asymmetric-risk-enhanced
  {:name "asymmetric-risk"
   :category "decision_making"
   :description "Seek situations where potential upside is much larger than potential downside. Avoid situations where downside is catastrophic regardless of upside."
   :key-insight "You don't need to be right often if your wins are much bigger than your losses. Conversely, avoid risks where one bad outcome can destroy you."
   :application "For each decision: (1) What's the worst case? (2) What's the best case? (3) What's the probability distribution? (4) Is the risk/reward asymmetric? (5) Can I survive the worst case?"
   :munger-quote "It is remarkable how much long-term advantage people like us have gotten by trying to be consistently not stupid, instead of trying to be very intelligent."
   :failure-modes
   [{:name "symmetric-thinking"
     :severity "high"
     :description "Treating all risks and rewards as symmetric, missing opportunities with massive upside and limited downside, or taking risks with catastrophic downside."
     :mechanism "Default mental model assumes normal distributions and symmetric outcomes. Reality often has fat tails and extreme asymmetry."
     :psychological-root "Linear thinking, normal distribution bias, and inability to grasp exponential outcomes."
     :evolutionary-origin "Ancestral risks and rewards were mostly symmetric. Extreme asymmetry is modern phenomenon."
     :signals
     ["Equal weight to upside and downside"
      "Missing convex opportunities (limited downside, unlimited upside)"
      "Taking concave risks (limited upside, unlimited downside)"
      "Not distinguishing between Type 1 and Type 2 risks"
      "Ignoring tail risks"
      "Not seeking optionality"
      "Missing venture capital / startup logic"
      "Treating all decisions as equally reversible"]
     :case-studies
     [{:name "Venture Capital Model"
       :year "1970-present"
       :entity "VC Industry"
       :description "Asymmetric model: Invest in 100 startups. 90 fail (lose 1x), 9 return 2-3x, 1 returns 100x+. Portfolio returns 5-10x despite 90% failure rate. Works because upside is unlimited, downside is capped at investment."
       :quantitative-impact "Top VC funds return 5-10x over 10 years. Sequoia: $1M in Google → $4.3B (4,300x). Benchmark: $6.7M in Uber → $7B (1,045x)."
       :root-cause "Understanding asymmetric risk: capped downside (lose investment), unlimited upside (company can grow 1000x)."
       :should-have-done "Apply to personal decisions: take asymmetric bets where downside is limited and upside is exponential."
       :sources ["VC fund returns", "Sequoia/Benchmark case studies", "Power law distributions in VC"]}
      {:name "Nassim Taleb's Barbell Strategy"
       :year 2007
       :entity "Nassim Taleb"
       :description "Before 2008 crisis: 90% in ultra-safe Treasury bonds, 10% in highly speculative options betting on market crash. Asymmetric: limited downside (can only lose 10%), unlimited upside (options pay 10-100x in crash). Made fortune in 2008."
       :quantitative-impact "Universa (Taleb's fund) returned 65% in 2008 while S&P fell 37%. Similar gains in 2020 COVID crash."
       :root-cause "Understanding asymmetry: avoid middle (symmetric risk), combine safe + highly asymmetric bets."
       :should-have-done "Avoid symmetric risks (can lose 50%, gain 50%). Seek asymmetric (lose 10%, gain 1000%)."
       :sources ["Taleb, N. The Black Swan (2007)", "Universa fund returns", "Barbell strategy papers"]}
      {:name "Amazon Bet on AWS"
       :year 2006
       :entity "Amazon"
       :description "Bezos: 'What if we rent our excess compute capacity?' Downside: $1-2B investment, might fail. Upside: could create new market. Asymmetric bet. AWS now $80B+ revenue, more profitable than retail."
       :quantitative-impact "AWS: $80B+ annual revenue, 32% of cloud market, 70%+ of Amazon's operating profit. 40x+ return on initial investment."
       :root-cause "Recognized asymmetry: limited downside (affordable investment), massive upside (new market creation)."
       :should-have-done "Look for opportunities where downside is affordable and upside is transformational."
       :sources ["Bezos shareholder letters", "AWS financial reports", "Amazon history"]}
      {:name "Long-Term Capital Management (LTCM)"
       :year 1998
       :entity "LTCM Hedge Fund"
       :description "Symmetric thinking failure: Strategies with small consistent gains (0.5-1% monthly) but catastrophic tail risk. Made money 47 months, lost everything in month 48. Downside was unlimited (bankruptcy), upside was limited (1% monthly)."
       :quantitative-impact "$4.6B loss, near collapse of financial system, $3.6B bailout, fund liquidated."
       :root-cause "Ignored asymmetry: limited upside (capped gains), unlimited downside (bankruptcy). Concave risk profile."
       :should-have-done "Avoid strategies with limited upside and catastrophic downside. Seek opposite."
       :sources ["Lowenstein, R. When Genius Failed (2000)", "Federal Reserve reports", "LTCM case studies"]}
      {:name "Nuclear Power Plant Design"
       :year "1970-present"
       :entity "Nuclear Industry"
       :description "Asymmetric risk: Upside is limited (electricity generation, predictable revenue). Downside is catastrophic (Chernobyl, Fukushima). Concave risk profile. Compare to solar: limited downside (panel fails), limited upside (predictable generation). But downside is affordable."
       :quantitative-impact "Chernobyl: $235B cost, 4,000-60,000 deaths. Fukushima: $500B+ cost, 154,000 evacuated. Nuclear growth stalled."
       :root-cause "Concave risk profile: limited upside, catastrophic downside. Asymmetry works against you."
       :should-have-done "Avoid technologies/strategies with catastrophic downside regardless of upside. Seek convex profiles."
       :sources ["WHO reports", "IAEA studies", "Nuclear accident cost analyses"]}]
     :quantitative-thresholds
     [{:metric "Upside/Downside Ratio"
       :warning-threshold "< 3:1"
       :critical-threshold "< 1:1 (symmetric or concave)"
       :measurement "Ratio of potential upside to potential downside"
       :frequency "Per major decision"
       :example "Investment with 50% upside, 40% downside = 1.25:1 (poor asymmetry)"}
      {:metric "Ruin Probability"
       :warning-threshold "> 1%"
       :critical-threshold "> 5%"
       :measurement "Probability of catastrophic loss (bankruptcy, death, irreversible harm)"
       :frequency "Per major risk"
       :example "Strategy with 10% chance of bankruptcy regardless of upside"}]
     :environmental-triggers
     ["Normal distribution assumptions"
      "Ignoring tail risks"
      "Pressure for consistent returns"
      "Lack of optionality thinking"
      "Short-term thinking"
      "Not understanding convexity"]
     :structural-safeguards
     ["Asymmetry analysis required for major decisions"
      "Ruin probability calculation"
      "Barbell strategies (safe + asymmetric)"
      "Portfolio approach to risks"
      "Never bet the company on symmetric risks"
      "Seek optionality and convexity"]
     :cognitive-safeguards
     ["Ask: 'What's the worst case? Can I survive it?'"
      "Ask: 'What's the best case? Is it transformational?'"
      "Ask: 'Is this convex (limited down, unlimited up) or concave (limited up, unlimited down)?'"
      "Seek situations where you can't lose much but can gain a lot"
      "Avoid situations where you can't gain much but can lose a lot"
      "Think in terms of optionality"]
     :social-safeguards
     ["Risk review by independent experts"
      "Tail risk assessment"
      "Study asymmetric successes (VC, Taleb)"
      "Challenge symmetric thinking"]
     :recovery-protocols
     [{:step 1
       :action "Identify if you're in a concave risk situation (limited upside, large downside)"
       :responsible "Risk management team"
       :timeline "Immediately"
       :success-criteria "Clear assessment of risk asymmetry"
       :escalation "If in catastrophic risk situation, emergency exit"}
      {:step 2
       :action "Calculate ruin probability and maximum loss"
       :responsible "Finance and risk teams"
       :timeline "Within 48 hours"
       :success-criteria "Quantified worst-case scenarios"
       :escalation "If ruin probability > 5%, immediate de-risking"}
      {:step 3
       :action "Restructure for convex risk profile"
       :responsible "Strategy team"
       :timeline "Within 1 week"
       :success-criteria "Limited downside, preserved upside optionality"
       :escalation "If restructuring insufficient, exit position"}
      {:step 4
       :action "Implement asymmetry analysis for all future decisions"
       :responsible "All decision-makers"
       :timeline "Ongoing"
       :success-criteria "Systematic asymmetry evaluation"
       :escalation "If not adopted, mandate training"}]
     :lollapalooza-risk 0.95
     :amplifying-biases ["normal-distribution-bias" "optimism-bias" "underestimating-tail-risks"]
     :mitigating-models ["margin-of-safety" "inversion" "second-order-thinking" "optionality"]}
    
    {:name "ignoring-tail-risks"
     :severity "critical"
     :description "Focusing on average outcomes while ignoring low-probability, high-impact tail events that can cause ruin."
     :mechanism "Normal distribution thinking. Reality has fat tails. Rare events are more common and more extreme than normal models predict."
     :psychological-root "Cognitive ease of normal distributions. Difficulty imagining extreme events."
     :evolutionary-origin "Extreme events were rare in ancestral environments. Modern interconnected systems create fat tails."
     :signals
     ["Using normal distribution assumptions"
      "Ignoring 'black swan' events"
      "No stress testing for extremes"
      "Overconfidence in models"
      "Dismissing low-probability events"
      "No ruin analysis"]
     :safeguards ["Stress test for 3-sigma, 5-sigma events", "Assume fat tails", "Calculate ruin probability", "Margin of safety for tail events", "Study historical extremes"]}
    
    {:name "not-seeking-optionality"
     :severity "high"
     :description "Failing to seek and create options that provide asymmetric upside with limited downside."
     :mechanism "Options have convex payoff: limited downside (option premium), unlimited upside. Most people don't think in terms of optionality."
     :psychological-root "Linear thinking. Options and convexity are counter-intuitive."
     :evolutionary-origin "Optionality is modern concept. Ancestral decisions were mostly binary."
     :signals
     ["Making irreversible commitments"
      "Not preserving flexibility"
      "Missing opportunities to create options"
      "Not valuing optionality"
      "Burning bridges"
      "All-or-nothing thinking"]
     :safeguards ["Seek optionality", "Preserve flexibility", "Make reversible decisions", "Create options before committing", "Value flexibility", "Bezos Type 2 decisions"]}
    
    {:name "taking-concave-risks"
     :severity "critical"
     :description "Taking risks with limited upside and unlimited downside (selling options, picking up pennies in front of steamroller)."
     :mechanism "Concave strategies feel good (consistent small wins) until catastrophic loss. Like selling insurance or lottery tickets."
     :psychological-root "Humans prefer consistent small gains over lumpy uncertain gains. Underestimate tail risks."
     :evolutionary-origin "Consistent resources were survival-optimal. Boom-bust was risky."
     :signals
     ["Consistent small gains"
      "Selling volatility or insurance"
      "Strategies that 'work until they don't'"
      "Picking up pennies in front of steamroller"
      "Ignoring catastrophic scenarios"
      "Overconfidence in risk management"]
     :safeguards ["Identify concave risk profiles", "Avoid selling options", "Avoid strategies with catastrophic downside", "Seek convex not concave"]}
    
    {:name "betting-the-company"
     :severity "critical"
     :description "Taking risks where one bad outcome can cause ruin, regardless of upside."
     :mechanism "Ruin is permanent. No amount of upside justifies ruin risk. But people take ruin risks for insufficient upside."
     :psychological-root "Optimism bias, overconfidence, greed, and pressure."
     :evolutionary-origin "All-or-nothing bets were sometimes necessary for survival."
     :signals
     ["Single point of failure"
      "Concentrated risks"
      "High leverage"
      "No margin of safety"
      "One bad outcome = bankruptcy"
      "Ignoring ruin probability"]
     :safeguards ["Never bet the company", "Diversify", "Margin of safety", "Calculate ruin probability", "Avoid leverage on ruin risks", "Portfolio approach"]}]})

;; Enhanced Model: Reversible vs Irreversible Decisions (Bezos Type 1 vs Type 2)
;; Different decision processes for different types of decisions
(def reversible-decisions-enhanced
  {:name "reversible-decisions"
   :category "decision_making"
   :description "Distinguish between irreversible decisions (Type 1: one-way doors) that require careful analysis, and reversible decisions (Type 2: two-way doors) that should be made quickly by individuals or small teams."
   :key-insight "Most decisions are reversible (Type 2) and should be made fast with 70% of information. Irreversible decisions (Type 1) require more analysis. Treating all decisions as Type 1 causes organizational paralysis."
   :application "For each decision: (1) Is this reversible? (2) If reversible, decide fast with 70% info. (3) If irreversible, slow down and analyze deeply. (4) Most decisions are Type 2."
   :munger-quote "It is remarkable how much long-term advantage people like us have gotten by trying to be consistently not stupid, instead of trying to be very intelligent."
   :failure-modes
   [{:name "treating-type-2-as-type-1"
     :severity "high"
     :description "Treating reversible decisions as if they're irreversible, leading to analysis paralysis, slow decision-making, and loss of competitive advantage."
     :mechanism "Risk aversion and perfectionism cause over-analysis of reversible decisions. Organizations default to slow careful process for all decisions."
     :psychological-root "Loss aversion, fear of being wrong, desire for certainty, and CYA culture."
     :evolutionary-origin "Caution was survival-optimal. Hasty decisions were dangerous."
     :signals
     ["Analysis paralysis on reversible decisions"
      "Slow decision-making on minor issues"
      "Requiring extensive data for reversible choices"
      "Multiple approval layers for Type 2 decisions"
      "Competitors moving faster"
      "Missing opportunities due to slow decisions"
      "Perfectionism on reversible choices"
      "Inability to distinguish decision types"]
     :case-studies
     [{:name "Amazon's Bias for Action"
       :year "1997-present"
       :entity "Amazon"
       :description "Bezos: 'Most decisions are Type 2 (reversible). Make them fast with 70% of info. Type 1 (irreversible) need more analysis. Most organizations treat all decisions as Type 1 and become slow.' Amazon's speed is competitive advantage."
       :quantitative-impact "$1.7T market cap, fastest innovation cycle in retail/tech, outpaced all competitors. Speed as core advantage."
       :root-cause "Correctly distinguishing Type 1 vs Type 2. Fast decisions on Type 2, careful on Type 1."
       :should-have-done "Adopt this framework. Identify what's truly irreversible. Move fast on everything else."
       :sources ["Bezos shareholder letters", "Amazon leadership principles", "Working Backwards book"]}
      {:name "Google's 20% Time"
       :year "2004-2013"
       :entity "Google"
       :description "Engineers could spend 20% time on side projects. Type 2 decisions: try projects, kill if not working. Created Gmail, AdSense, Google News. Later killed program, slowed innovation."
       :quantitative-impact "Gmail: 1.8B users. AdSense: $20B+ annual revenue. Google News: 1B+ users. All from Type 2 experiments."
       :root-cause "Recognized side projects are Type 2 (reversible). Enabled fast experimentation."
       :should-have-done "Maintain Type 2 experimentation culture. Don't let bureaucracy slow reversible decisions."
       :sources ["Google history", "Product revenue data", "Employee accounts"]}
      {:name "Netflix Culture of Freedom and Responsibility"
       :year "2009-present"
       :entity "Netflix"
       :description "Empower employees to make reversible decisions without approval. 'Don't seek to please your boss, seek to do what's best for Netflix.' Type 2 decisions made by individuals. Type 1 decisions have process."
       :quantitative-impact "$280B market cap, 230M subscribers, outpaced all entertainment competitors in streaming transition."
       :root-cause "Correctly identified that most operational decisions are Type 2. Empowered fast decision-making."
       :should-have-done "Adopt similar culture. Trust employees on Type 2 decisions."
       :sources ["Netflix culture deck", "No Rules Rules book", "Hastings interviews"]}
      {:name "Yahoo's Slow Decision-Making"
       :year "2000-2010"
       :entity "Yahoo"
       :description "Treated all decisions as Type 1. Multiple approval layers, extensive analysis, slow execution. Missed Google acquisition ($1M ask), Facebook acquisition ($1B ask), search innovation, mobile. Declined from $125B to $4.5B."
       :quantitative-impact "$125B peak valuation → $4.5B sale to Verizon. Lost $120B+ in value. Passed on Google and Facebook."
       :root-cause "Treated Type 2 decisions (acquisitions, product experiments) as Type 1. Analysis paralysis."
       :should-have-done "Distinguish Type 1 vs Type 2. Move fast on acquisitions and experiments (Type 2). Slow down on irreversible bets (Type 1)."
       :sources ["Yahoo history", "Acquisition case studies", "Marissa Mayer accounts"]}
      {:name "Kodak's Digital Photography Delay"
       :year "1975-2000"
       :entity "Kodak"
       :description "Invented digital camera in 1975. Treated digital transition as Type 1 (irreversible, threatening film business). Delayed for 25 years. Actually was Type 2 (could experiment, iterate, learn). Bankruptcy 2012."
       :quantitative-impact "$31B peak value → bankruptcy. Lost entire digital photography market to Canon, Nikon, Sony, smartphones."
       :root-cause "Treated digital experiments as Type 1 (irreversible commitment). Actually Type 2 (reversible experiments)."
       :should-have-done "Recognize digital experiments are Type 2. Run parallel experiments. Learn fast. Iterate."
       :sources ["Kodak bankruptcy filing", "Digital camera history", "Innovation case studies"]}]
     :quantitative-thresholds
     [{:metric "Decision Velocity"
       :warning-threshold "< 70% of decisions made within 1 week"
       :critical-threshold "< 50% within 1 week"
       :measurement "Percentage of Type 2 decisions made within 1 week"
       :frequency "Monthly decision audit"
       :example "Only 40% of reversible decisions made within 1 week"}
      {:metric "Type 1 vs Type 2 Identification"
       :warning-threshold "< 80% correctly identified"
       :critical-threshold "< 60% correctly identified"
       :measurement "Percentage of decisions correctly classified as Type 1 or Type 2"
       :frequency "Quarterly decision review"
       :example "Treating 50% of Type 2 decisions as Type 1"}]
     :environmental-triggers
     ["Risk-averse culture"
      "Perfectionism"
      "Lack of trust"
      "Bureaucratic processes"
      "Fear of failure"
      "CYA culture"]
     :structural-safeguards
     ["Explicit Type 1 vs Type 2 classification"
      "Fast-track process for Type 2 decisions"
      "Empower individuals/small teams for Type 2"
      "Careful process only for Type 1"
      "Decision velocity metrics"
      "Bias for action on Type 2"]
     :cognitive-safeguards
     ["Ask: 'Is this reversible?'"
      "Ask: 'What's the cost of reversing this decision?'"
      "Ask: 'What's the cost of delay?'"
      "Default to Type 2 unless proven Type 1"
      "70% information rule for Type 2"
      "Study Bezos framework"]
     :social-safeguards
     ["Culture of experimentation"
      "Celebrate fast decisions on Type 2"
      "Punish slow decisions on Type 2, not failed experiments"
      "Trust and empowerment"
      "Reduce approval layers"]
     :recovery-protocols
     [{:step 1
       :action "Audit recent decisions: Type 1 or Type 2?"
       :responsible "Leadership team"
       :timeline "Within 1 week"
       :success-criteria "Complete decision classification"
       :escalation "If most Type 2 treated as Type 1, culture problem"}
      {:step 2
       :action "Identify bottlenecks in Type 2 decision-making"
       :responsible "Operations team"
       :timeline "Within 2 weeks"
       :success-criteria "Root causes of slow Type 2 decisions"
       :escalation "If systemic (approval layers, culture), escalate to CEO"}
      {:step 3
       :action "Implement fast-track process for Type 2"
       :responsible "All teams"
       :timeline "Within 1 month"
       :success-criteria "Type 2 decisions made in < 1 week"
       :escalation "If not improving, mandate decision velocity metrics"}
      {:step 4
       :action "Build culture of bias for action"
       :responsible "Leadership"
       :timeline "Ongoing"
       :success-criteria "Decision velocity increasing, innovation accelerating"
       :escalation "If culture doesn't shift, leadership changes needed"}]
     :lollapalooza-risk 0.85
     :amplifying-biases ["loss-aversion" "status-quo-bias" "perfectionism" "fear-of-failure"]
     :mitigating-models ["inversion" "opportunity-cost" "first-principles" "asymmetric-risk"]}
    
    {:name "treating-type-1-as-type-2"
     :severity "critical"
     :description "Treating irreversible decisions as if they're reversible, leading to hasty decisions with catastrophic consequences."
     :mechanism "Overconfidence and optimism lead to treating irreversible decisions casually. 'We can always fix it later' when you can't."
     :psychological-root "Optimism bias, overconfidence, impatience, and underestimating irreversibility."
     :evolutionary-origin "Optimism and action bias were survival-optimal in many contexts."
     :signals
     ["Hasty decisions on irreversible matters"
      "Insufficient analysis of Type 1 decisions"
      "Overconfidence in ability to reverse"
      "Ignoring irreversible consequences"
      "Impatience with necessary analysis"
      "Dismissing concerns as 'overthinking'"]
     :safeguards ["Identify truly irreversible decisions", "Slow down on Type 1", "Pre-mortem for Type 1", "Margin of safety", "Inversion: what makes this irreversible?"]}
    
    {:name "not-identifying-decision-type"
     :severity "high"
     :description "Failing to classify decisions as Type 1 or Type 2, leading to random decision processes."
     :mechanism "Without explicit classification, organizations default to treating all decisions the same (usually Type 1 process)."
     :psychological-root "Lack of framework. Not taught to distinguish decision types."
     :evolutionary-origin "Decision type classification is modern concept."
     :signals
     ["No explicit decision classification"
      "Same process for all decisions"
      "Inconsistent decision speed"
      "Confusion about when to slow down vs speed up"
      "No shared framework"]
     :safeguards ["Explicit Type 1 vs Type 2 classification", "Teach Bezos framework", "Decision classification checklist", "Shared language"]}
    
    {:name "reversibility-illusion"
     :severity "high"
     :description "Believing decisions are reversible when they're actually irreversible or very costly to reverse."
     :mechanism "Optimism bias and underestimating switching costs, reputation effects, and path dependencies."
     :psychological-root "Optimism bias, present bias, and difficulty imagining future constraints."
     :evolutionary-origin "Overconfidence in ability to change course was sometimes adaptive."
     :signals
     ["Underestimating switching costs"
      "Ignoring reputation effects"
      "Missing path dependencies"
      "Overconfidence in reversibility"
      "Discovering decisions are harder to reverse than expected"]
     :safeguards ["Realistically assess reversibility", "Calculate switching costs", "Consider reputation and relationship effects", "Identify path dependencies"]}
    
    {:name "speed-at-all-costs"
     :severity "medium"
     :description "Overvaluing speed and treating all decisions as Type 2, even when careful analysis is warranted."
     :mechanism "Bias for action becomes dogma. Speed is valued over quality even for irreversible decisions."
     :psychological-root "Action bias, impatience, and overconfidence."
     :evolutionary-origin "Action was often better than inaction in ancestral environments."
     :signals
     ["Rushing irreversible decisions"
      "Dismissing analysis as 'overthinking'"
      "Valuing speed over quality on Type 1"
      "Regretting hasty irreversible decisions"
      "Culture of 'move fast and break things' on Type 1"]
     :safeguards ["Distinguish Type 1 vs Type 2", "Slow down on Type 1", "Fast on Type 2, careful on Type 1", "Balance speed and quality"]}]})

;; Enhanced Model: Compounding
;; Understanding exponential growth and decay over time
(def compounding-enhanced
  {:name "compounding"
   :category "decision_making"
   :description "Small changes compound exponentially over time. 1% improvement daily = 37x improvement in a year. Works for good (learning, investing) and bad (debt, bad habits)."
   :key-insight "Compounding is the most powerful force in the universe (Einstein allegedly). Most people underestimate exponential growth and focus on linear improvements."
   :application "For any decision: (1) What compounds? (2) What's the rate? (3) What's the time horizon? (4) Am I compounding positively or negatively? (5) Small differences in rate create massive differences over time."
   :munger-quote "Understanding both the power of compound return and the difficulty of getting it is the heart and soul of understanding a lot of things."
   :failure-modes
   [{:name "linear-thinking"
     :severity "high"
     :description "Thinking linearly when reality compounds exponentially, leading to massive underestimation of long-term effects."
     :mechanism "Human brains evolved for linear thinking. Exponential growth is counter-intuitive and cognitively difficult."
     :psychological-root "Linear thinking is default. Exponential requires mathematical thinking."
     :evolutionary-origin "Most ancestral phenomena were linear. Exponential growth is modern."
     :signals
     ["Linear extrapolation of exponential trends"
      "Underestimating long-term effects"
      "Surprise at exponential outcomes"
      "Not understanding 'hockey stick' curves"
      "Dismissing small improvements as insignificant"
      "Not recognizing compounding in action"
      "Focusing on absolute numbers not rates"
      "Missing the power of small rate differences"]
     :case-studies
     [{:name "Warren Buffett's Wealth"
       :year "1950-2024"
       :entity "Warren Buffett"
       :description "Started investing at age 11. 99% of his $120B wealth was created after age 50. Not because he got better, but because compounding had more time. 22% annual return for 70 years = $120B from modest start."
       :quantitative-impact "$120B net worth. 99% created after age 50. 22% CAGR over 70 years. $1,000 invested in Berkshire in 1965 = $42M in 2024."
       :root-cause "Understanding compounding. Time + rate = exponential wealth. Most people focus on rate, ignore time."
       :should-have-done "Start early. Focus on sustainable rate. Let time do the work. Don't interrupt compounding."
       :sources ["Berkshire Hathaway annual reports", "Buffett biography", "Investment performance data"]}
      {:name "COVID-19 Exponential Spread"
       :year 2020
       :entity "Global pandemic"
       :description "Early 2020: 'Just 100 cases, no big deal.' Exponential growth: doubles every 3 days. 100 → 200 → 400 → 800 → 1,600 → 3,200 → 6,400 → 12,800 → 25,600 → 51,200 → 102,400 → 204,800 in one month. Linear thinkers missed it."
       :quantitative-impact "7M deaths globally, $16T economic cost, complete societal disruption. Could have been prevented with exponential thinking."
       :root-cause "Linear thinking about exponential spread. 'Just 100 cases' → 100,000 cases in 30 days."
       :should-have-done "Understand exponential growth. Act early when numbers are small. Exponential problems require exponential response."
       :sources ["WHO data", "Economic impact studies", "Epidemiological models"]}
      {:name "Moore's Law and Computing"
       :year "1965-2024"
       :entity "Semiconductor industry"
       :description "Transistors per chip double every 2 years. 1971: 2,300 transistors. 2024: 100 billion transistors. 43 million times improvement. Enabled smartphones, AI, internet. Linear thinkers missed it."
       :quantitative-impact "Computing power: 43M times improvement. Created $5T+ tech industry. Transformed civilization."
       :root-cause "Understanding exponential technology improvement. Compound at 41% annually for 50 years = 43M times."
       :should-have-done "Recognize exponential technology trends. Invest early. Build on exponential foundations."
       :sources ["Moore's Law data", "Semiconductor industry reports", "Technology history"]}
      {:name "Credit Card Debt Trap"
       :year "1980-present"
       :entity "US Consumer Debt"
       :description "18-29% APR on credit cards. $5,000 balance at 24% APR, paying minimum (2% = $100/month): Takes 30 years to pay off, total paid = $19,000. Negative compounding destroys wealth."
       :quantitative-impact "$1T+ in US credit card debt. Average household: $6,000 balance. Millions trapped in debt spiral."
       :root-cause "Not understanding negative compounding. 'Just $5,000' becomes $19,000 over time."
       :should-have-done "Understand negative compounding. Avoid high-interest debt. Pay off fast. Don't let debt compound."
       :sources ["Federal Reserve consumer credit data", "Credit card industry reports", "Debt studies"]}
      {:name "Learning and Skill Compounding"
       :year "Lifetime"
       :entity "Individual development"
       :description "1% improvement daily = 37x improvement in a year (1.01^365 = 37.8). 1% decline daily = 97% decline in a year (0.99^365 = 0.03). Small daily habits compound to life transformation."
       :quantitative-impact "10 years of 1% daily improvement = 37^10 = 4.8 quadrillion times improvement (theoretical). Practically: expert level in any skill."
       :root-cause "Understanding positive compounding of learning. Small daily improvements compound exponentially."
       :should-have-done "Focus on daily improvement rate. Consistency matters more than intensity. Let compounding work."
       :sources ["Atomic Habits (Clear)", "Peak (Ericsson)", "Deliberate practice research"]}]
     :quantitative-thresholds
     [{:metric "Compounding Rate Awareness"
       :warning-threshold "< 50% of decisions consider compounding effects"
       :critical-threshold "< 25% consider compounding"
       :measurement "Percentage of decisions that explicitly analyze compounding"
       :frequency "Quarterly decision audit"
       :example "Made 20 decisions, only 3 considered long-term compounding effects"}
      {:metric "Time Horizon"
       :warning-threshold "< 3 years"
       :critical-threshold "< 1 year"
       :measurement "Average time horizon in decision analysis"
       :frequency "Per major decision"
       :example "Optimizing for quarterly results, ignoring 10-year compounding"}]
     :environmental-triggers
     ["Short-term thinking"
      "Quarterly pressure"
      "Lack of mathematical literacy"
      "Impatience"
      "Focus on absolute numbers not rates"
      "Interrupting compounding"]
     :structural-safeguards
     ["Long-term incentives aligned with compounding"
      "Compound interest calculators required"
      "Exponential thinking training"
      "Long time horizons in planning"
      "Protect compounding processes"
      "Measure rates not just absolutes"]
     :cognitive-safeguards
     ["Ask: 'What compounds here?'"
      "Ask: 'What's the rate and time horizon?'"
      "Ask: 'Am I thinking linearly about something exponential?'"
      "Calculate exponential outcomes"
      "Focus on sustainable rates"
      "Don't interrupt compounding"
      "Study exponential examples"]
     :social-safeguards
     ["Share exponential thinking frameworks"
      "Celebrate long-term compounding"
      "Resist short-term pressure"
      "Study compounding success stories"]
     :recovery-protocols
     [{:step 1
       :action "Identify where linear thinking caused exponential problem"
       :responsible "Analysis team"
       :timeline "Within 1 week"
       :success-criteria "Clear understanding of exponential dynamics"
       :escalation "If exponential crisis, emergency response"}
      {:step 2
       :action "Model exponential scenarios"
       :responsible "Strategy team"
       :timeline "Within 2 weeks"
       :success-criteria "Exponential projections for key variables"
       :escalation "If projections show crisis, escalate to leadership"}
      {:step 3
       :action "Implement exponential response"
       :responsible "Operations team"
       :timeline "Immediately"
       :success-criteria "Response matches exponential dynamics"
       :escalation "If linear response to exponential problem, override"}
      {:step 4
       :action "Build exponential thinking into decision processes"
       :responsible "All teams"
       :timeline "Ongoing"
       :success-criteria "Exponential analysis standard practice"
       :escalation "If not adopted, mandate training"}]
     :lollapalooza-risk 0.90
     :amplifying-biases ["linear-thinking-bias" "present-bias" "impatience"]
     :mitigating-models ["second-order-thinking" "long-term-thinking" "patience" "systems-thinking"]}
    
    {:name "short-time-horizons"
     :severity "high"
     :description "Focusing on short-term results and interrupting compounding processes before they can work."
     :mechanism "Compounding requires time. Interrupting early destroys exponential gains. Quarterly thinking kills compounding."
     :psychological-root "Present bias, impatience, and short-term incentives."
     :evolutionary-origin "Immediate rewards were more certain than distant rewards."
     :signals
     ["Quarterly thinking"
      "Frequent strategy changes"
      "Interrupting long-term processes"
      "Impatience with compounding"
      "Chasing short-term gains"
      "Not giving compounding time to work"]
     :safeguards ["Long time horizons", "Patience", "Don't interrupt compounding", "Long-term incentives", "Ignore short-term noise"]}
    
    {:name "negative-compounding"
     :severity "critical"
     :description "Allowing negative habits, debt, or problems to compound, leading to exponential decline."
     :mechanism "Negative compounding is as powerful as positive. Bad habits, debt, and problems compound exponentially."
     :psychological-root "Present bias, denial, and difficulty changing habits."
     :evolutionary-origin "Immediate gratification was survival-optimal."
     :signals
     ["Accumulating debt"
      "Bad habits persisting"
      "Problems growing exponentially"
      "Denial of negative trends"
      "Not addressing compounding problems early"
      "Exponential decline"]
     :safeguards ["Identify negative compounding early", "Address immediately", "Break negative compound loops", "Avoid high-interest debt", "Fix bad habits fast"]}
    
    {:name "not-starting-early"
     :severity "high"
     :description "Delaying the start of positive compounding processes, losing years of exponential growth."
     :mechanism "Compounding is time-dependent. Starting late loses years of exponential growth. Starting at 25 vs 35 = 2x wealth at retirement."
     :psychological-root "Present bias, procrastination, and not understanding time value."
     :evolutionary-origin "Delayed gratification was cognitively demanding."
     :signals
     ["Procrastinating on investing"
      "Delaying learning"
      "Not starting good habits"
      "Waiting for 'perfect time'"
      "Missing early compounding years"
      "Starting late"]
     :safeguards ["Start now", "Don't wait for perfect conditions", "Understand time value", "Early start > perfect start", "Begin compounding immediately"]}
    
    {:name "focusing-on-rate-ignoring-time"
     :severity "medium"
     :description "Obsessing over rate of return while ignoring time horizon, missing that time is often more important than rate."
     :mechanism "10% for 40 years > 15% for 20 years. But people focus on rate and ignore time."
     :psychological-root "Rate is salient and controllable. Time is abstract and uncontrollable."
     :evolutionary-origin "Immediate high returns were more valuable than distant returns."
     :signals
     ["Chasing high returns"
      "Ignoring time horizon"
      "Taking excessive risk for rate"
      "Not understanding time value"
      "Interrupting compounding for higher rate"
      "Missing that time > rate"]
     :safeguards ["Understand time value", "Sustainable rate > maximum rate", "Don't interrupt compounding", "Time is the key variable", "Patience"]}]})

;; Update export to include all enhanced models
(def enhanced-models
  [second-order-thinking-enhanced
   inversion-enhanced
   incentives-enhanced
   margin-of-safety-enhanced
   circle-of-competence-enhanced
   opportunity-cost-enhanced
   systems-thinking-enhanced
   first-principles-enhanced
   asymmetric-risk-enhanced
   reversible-decisions-enhanced
   compounding-enhanced])
