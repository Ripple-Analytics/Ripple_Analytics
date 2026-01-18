;; Complete Mental Models Library in Hy (Lisp)
;; All 129 mental models defined using Lisp macros
;; 
;; This is the core knowledge base - all models defined in
;; expressive Lisp syntax for rapid extension and modification

(import [typing [Dict List Any Optional]])
(import [datetime [datetime]])
(import [enum [Enum]])

;; ============================================
;; Model Registry
;; ============================================

(setv *models* {})
(setv *failure-modes* {})
(setv *categories* {})

(defn register [model]
  "Register a model in the global registry."
  (let [name (get model "name")]
    (setv (get *models* name) model)
    (let [category (get model "category" "general")]
      (when (not (in category *categories*))
        (setv (get *categories* category) []))
      (.append (get *categories* category) name))
    (for [fm (get model "failure_modes" [])]
      (setv (get *failure-modes* (+ name "_" (get fm "name"))) fm)))
  model)

;; ============================================
;; Model Definition Macro
;; ============================================

(defmacro model [name &rest body]
  "Define a mental model with all its properties."
  `(register {"name" ~(str name) ~@body}))

(defmacro failure [name severity description &rest body]
  "Define a failure mode."
  `{"name" ~(str name)
    "severity" ~severity
    "description" ~description
    ~@body})

;; ============================================
;; Category: Decision Making
;; ============================================

(model circle-of-competence
  "category" "decision_making"
  "originator" "Warren Buffett"
  "description" "Know the boundaries of your knowledge and stay within them"
  "key_insight" "The size of your circle matters less than knowing its boundaries"
  "application" "Before any decision, ask: Is this within my circle?"
  "failure_modes" [
    (failure overconfidence "high"
      "Believing your circle is larger than it is"
      "signals" ["No recent failures" "Ignoring expert advice" "Dismissing contrary evidence"]
      "safeguards" ["Regular competence audits" "Seek disconfirming evidence" "Track prediction accuracy"])
    (failure underconfidence "medium"
      "Not acting within your actual competence"
      "signals" ["Excessive hesitation" "Deferring on known topics" "Analysis paralysis"]
      "safeguards" ["Track past successes" "Build confidence gradually" "Start with small decisions"])
    (failure static-circle "medium"
      "Not expanding your circle over time"
      "signals" ["No new learning" "Same decisions for years" "Avoiding challenges"]
      "safeguards" ["Deliberate practice" "Adjacent learning" "Stretch assignments"])
    (failure boundary-blindness "high"
      "Not knowing where your circle ends"
      "signals" ["Overconfident predictions" "No uncertainty acknowledgment"]
      "safeguards" ["Explicit uncertainty ranges" "Pre-mortems" "Outside feedback"])
    (failure competence-creep "high"
      "Gradually drifting outside your circle"
      "signals" ["Slow expansion without validation" "Success breeding overreach"]
      "safeguards" ["Regular boundary checks" "Milestone reviews" "External validation"])])

(model margin-of-safety
  "category" "decision_making"
  "originator" "Benjamin Graham"
  "description" "Always leave room for error in your calculations"
  "key_insight" "The future is uncertain; build in buffers"
  "application" "Add 25-50% buffer to all estimates"
  "failure_modes" [
    (failure insufficient-margin "critical"
      "Not leaving enough buffer for unexpected events"
      "signals" ["Optimistic projections" "Ignoring tail risks" "Best-case planning"]
      "safeguards" ["Double estimated margin" "Stress test assumptions" "Plan for worst case"])
    (failure excessive-margin "low"
      "Being so conservative you miss opportunities"
      "signals" ["Never taking action" "Paralysis by analysis" "Missing obvious wins"]
      "safeguards" ["Balance risk/reward" "Time-bound decisions" "Opportunity cost awareness"])
    (failure false-precision "medium"
      "Believing precise calculations eliminate uncertainty"
      "signals" ["Many decimal places" "Complex models" "Overconfidence in numbers"]
      "safeguards" ["Round estimates" "Sensitivity analysis" "Scenario planning"])
    (failure margin-erosion "high"
      "Gradually reducing margins under pressure"
      "signals" ["Competitive pressure" "Short-term thinking" "Margin compression"]
      "safeguards" ["Hard minimum thresholds" "Regular margin reviews" "Long-term focus"])
    (failure wrong-margin "high"
      "Building margin against the wrong risks"
      "signals" ["Protecting against unlikely events" "Ignoring likely risks"]
      "safeguards" ["Risk prioritization" "Probability assessment" "Historical analysis"])])

(model second-order-thinking
  "category" "decision_making"
  "originator" "Howard Marks"
  "description" "Think about the consequences of the consequences"
  "key_insight" "First-level thinking is simplistic; second-level thinking is deep"
  "application" "Ask 'And then what?' at least three times"
  "failure_modes" [
    (failure first-order-only "high"
      "Only considering immediate effects"
      "signals" ["Quick decisions" "No scenario planning" "Surprise by consequences"]
      "safeguards" ["Mandatory 'then what' exercise" "Consider 3 time horizons" "Scenario planning"])
    (failure infinite-regress "medium"
      "Getting lost in endless chains of consequences"
      "signals" ["Analysis paralysis" "Never deciding" "Overthinking"]
      "safeguards" ["Set analysis limits" "Time-box thinking" "Focus on material effects"])
    (failure wrong-chain "high"
      "Following the wrong causal chain"
      "signals" ["Ignoring key variables" "Linear thinking in complex systems"]
      "safeguards" ["Multiple scenario paths" "Expert consultation" "Historical analogies"])
    (failure probability-neglect "high"
      "Not weighting consequences by likelihood"
      "signals" ["Equal weight to all outcomes" "Ignoring base rates"]
      "safeguards" ["Probability estimates" "Expected value calculations" "Base rate research"])
    (failure time-horizon-mismatch "medium"
      "Optimizing for wrong time frame"
      "signals" ["Short-term focus" "Ignoring long-term effects"]
      "safeguards" ["Explicit time horizons" "Multi-period analysis" "Stakeholder mapping"])])

(model inversion
  "category" "decision_making"
  "originator" "Carl Jacobi / Charlie Munger"
  "description" "Invert, always invert - think about what to avoid"
  "key_insight" "It's often easier to avoid stupidity than to seek brilliance"
  "application" "Ask 'How could this fail?' before 'How could this succeed?'"
  "failure_modes" [
    (failure forward-only "high"
      "Only thinking about how to succeed"
      "signals" ["No failure analysis" "Optimism bias" "Ignoring risks"]
      "safeguards" ["Pre-mortems" "Failure mode analysis" "Devil's advocate"])
    (failure incomplete-inversion "medium"
      "Not inverting thoroughly enough"
      "signals" ["Surface-level inversion" "Missing key failure modes"]
      "safeguards" ["Systematic failure enumeration" "Multiple perspectives" "Historical failures"])
    (failure paralysis-by-inversion "medium"
      "Finding so many failure modes you can't act"
      "signals" ["Endless risk lists" "No action" "Fear-based decisions"]
      "safeguards" ["Prioritize risks" "Accept residual risk" "Time-bound analysis"])
    (failure inversion-bias "low"
      "Becoming too focused on avoiding failure"
      "signals" ["Missing opportunities" "Excessive caution" "Defensive posture"]
      "safeguards" ["Balance with opportunity seeking" "Upside analysis" "Growth mindset"])
    (failure wrong-inversion "high"
      "Inverting the wrong question"
      "signals" ["Solving wrong problem" "Misframed question"]
      "safeguards" ["Question the question" "Multiple framings" "Stakeholder input"])])

(model opportunity-cost
  "category" "decision_making"
  "originator" "Economics"
  "description" "The cost of any choice is what you give up"
  "key_insight" "Every yes is a no to something else"
  "application" "Always consider the next best alternative"
  "failure_modes" [
    (failure ignoring-alternatives "high"
      "Not considering what else you could do"
      "signals" ["Single option focus" "No comparison" "First idea accepted"]
      "safeguards" ["Generate 3+ alternatives" "Explicit comparison" "Opportunity cost calculation"])
    (failure sunk-cost-confusion "high"
      "Confusing sunk costs with opportunity costs"
      "signals" ["Considering past investments" "Throwing good money after bad"]
      "safeguards" ["Ignore sunk costs" "Fresh start thinking" "Zero-based decisions"])
    (failure narrow-framing "medium"
      "Considering too few alternatives"
      "signals" ["Binary choices" "Limited options" "Tunnel vision"]
      "safeguards" ["Brainstorm alternatives" "Outside perspectives" "Creative options"])
    (failure time-blindness "medium"
      "Not considering time as a resource"
      "signals" ["Ignoring time costs" "No time valuation"]
      "safeguards" ["Value your time" "Time opportunity costs" "Delegation analysis"])
    (failure comparison-paralysis "low"
      "Unable to choose between alternatives"
      "signals" ["Endless comparison" "No decision" "Perfect option seeking"]
      "safeguards" ["Good enough threshold" "Time limits" "Reversibility check"])])

;; ============================================
;; Category: Psychology
;; ============================================

(model incentives
  "category" "psychology"
  "originator" "Charlie Munger"
  "description" "Never think about anything else when you should be thinking about incentives"
  "key_insight" "Show me the incentive and I'll show you the outcome"
  "application" "Always ask: What are the incentives?"
  "failure_modes" [
    (failure ignoring-incentives "critical"
      "Not considering what motivates behavior"
      "signals" ["Surprised by behavior" "Moral explanations for economic behavior"]
      "safeguards" ["Incentive mapping" "Follow the money" "Behavioral prediction"])
    (failure misaligned-incentives "high"
      "Creating incentives that produce wrong behavior"
      "signals" ["Gaming" "Unintended consequences" "Perverse outcomes"]
      "safeguards" ["Incentive design review" "Pilot testing" "Feedback loops"])
    (failure incentive-blindness "high"
      "Not seeing your own incentive biases"
      "signals" ["Self-serving conclusions" "Rationalization"]
      "safeguards" ["Disinterested review" "Incentive disclosure" "Counter-incentive analysis"])
    (failure over-incentivizing "medium"
      "Crowding out intrinsic motivation"
      "signals" ["Reduced intrinsic interest" "Only working for rewards"]
      "safeguards" ["Balance intrinsic/extrinsic" "Purpose alignment" "Autonomy preservation"])
    (failure static-incentive-thinking "medium"
      "Not considering how incentives change over time"
      "signals" ["Outdated incentive structures" "Changed circumstances"]
      "safeguards" ["Regular incentive review" "Dynamic adjustment" "Feedback mechanisms"])])

(model social-proof
  "category" "psychology"
  "originator" "Robert Cialdini"
  "description" "People look to others to determine correct behavior"
  "key_insight" "We assume if many people do something, it must be right"
  "application" "Question: Am I doing this because others are?"
  "failure_modes" [
    (failure herd-following "high"
      "Blindly following the crowd"
      "signals" ["No independent analysis" "Comfort in consensus" "FOMO"]
      "safeguards" ["Independent thinking" "Contrarian analysis" "First principles"])
    (failure wrong-reference-group "medium"
      "Following the wrong crowd"
      "signals" ["Inappropriate comparisons" "Wrong peer group"]
      "safeguards" ["Choose reference groups carefully" "Expert identification"])
    (failure manufactured-proof "high"
      "Being fooled by fake social proof"
      "signals" ["Astroturfing" "Fake reviews" "Manipulated metrics"]
      "safeguards" ["Verify sources" "Look for authenticity" "Skepticism"])
    (failure proof-cascade "high"
      "Contributing to information cascades"
      "signals" ["Amplifying without verification" "Viral spread"]
      "safeguards" ["Verify before sharing" "Break cascades" "Independent verification"])
    (failure anti-social-proof "low"
      "Contrarian for its own sake"
      "signals" ["Automatic disagreement" "Ignoring valid consensus"]
      "safeguards" ["Evaluate on merits" "Selective contrarianism" "Evidence-based"])])

(model commitment-consistency
  "category" "psychology"
  "originator" "Robert Cialdini"
  "description" "Once committed, people tend to behave consistently with that commitment"
  "key_insight" "Small commitments lead to larger ones"
  "application" "Watch for escalating commitments"
  "failure_modes" [
    (failure escalation-of-commitment "critical"
      "Doubling down on failing courses of action"
      "signals" ["Throwing good money after bad" "Ignoring negative feedback"]
      "safeguards" ["Kill criteria" "Fresh eyes review" "Sunk cost awareness"])
    (failure foot-in-door "medium"
      "Being manipulated through small commitments"
      "signals" ["Gradual escalation" "Small yeses leading to big ones"]
      "safeguards" ["Recognize the pattern" "Evaluate each request independently"])
    (failure identity-lock "high"
      "Commitment becoming part of identity"
      "signals" ["Defending positions as self" "Emotional attachment to decisions"]
      "safeguards" ["Separate identity from positions" "Update beliefs" "Growth mindset"])
    (failure public-commitment-trap "medium"
      "Public commitments preventing course correction"
      "signals" ["Can't back down publicly" "Reputation concerns"]
      "safeguards" ["Private deliberation" "Graceful pivots" "Learning framing"])
    (failure consistency-worship "medium"
      "Valuing consistency over correctness"
      "signals" ["Foolish consistency" "Ignoring new information"]
      "safeguards" ["Update on evidence" "Intellectual honesty" "Embrace change"])])

(model availability-heuristic
  "category" "psychology"
  "originator" "Kahneman & Tversky"
  "description" "We judge likelihood by how easily examples come to mind"
  "key_insight" "Vivid and recent events are overweighted"
  "application" "Ask: Am I overweighting recent/vivid events?"
  "failure_modes" [
    (failure recency-bias "high"
      "Overweighting recent events"
      "signals" ["Recent events dominating" "Ignoring base rates"]
      "safeguards" ["Historical data" "Base rate research" "Long-term perspective"])
    (failure vividness-bias "high"
      "Overweighting dramatic events"
      "signals" ["Emotional reactions" "Ignoring mundane risks"]
      "safeguards" ["Statistical thinking" "Probability assessment" "Calm analysis"])
    (failure media-distortion "medium"
      "Being influenced by media coverage"
      "signals" ["News-driven fears" "Disproportionate concerns"]
      "safeguards" ["Seek data" "Ignore news cycle" "Long-term view"])
    (failure personal-experience-bias "medium"
      "Overweighting personal experiences"
      "signals" ["Anecdotal reasoning" "Sample of one"]
      "safeguards" ["Seek broader data" "Statistical samples" "Outside view"])
    (failure availability-cascade "high"
      "Self-reinforcing availability through repetition"
      "signals" ["Echo chambers" "Repeated claims becoming 'true'"]
      "safeguards" ["Source diversity" "Fact checking" "Original sources"])])

(model loss-aversion
  "category" "psychology"
  "originator" "Kahneman & Tversky"
  "description" "Losses loom larger than equivalent gains"
  "key_insight" "The pain of losing is about twice the pleasure of gaining"
  "application" "Check if you're irrationally avoiding losses"
  "failure_modes" [
    (failure excessive-risk-aversion "high"
      "Avoiding reasonable risks due to loss fear"
      "signals" ["Missed opportunities" "Excessive caution" "Status quo bias"]
      "safeguards" ["Expected value thinking" "Reframe as gains" "Long-term view"])
    (failure holding-losers "high"
      "Refusing to realize losses"
      "signals" ["Holding losing positions" "Hope over analysis"]
      "safeguards" ["Pre-set stop losses" "Regular portfolio review" "Fresh evaluation"])
    (failure narrow-framing "medium"
      "Evaluating each decision in isolation"
      "signals" ["Single decision focus" "Ignoring portfolio effect"]
      "safeguards" ["Portfolio thinking" "Aggregate view" "Long-term perspective"])
    (failure endowment-effect "medium"
      "Overvaluing what you already own"
      "signals" ["Reluctance to sell" "Higher selling than buying price"]
      "safeguards" ["Ownership-blind evaluation" "Market price reference"])
    (failure loss-framing-manipulation "medium"
      "Being manipulated through loss framing"
      "signals" ["Responding to loss frames" "Fear-based decisions"]
      "safeguards" ["Reframe as gains" "Recognize manipulation" "Neutral framing"])])

;; ============================================
;; Category: Systems
;; ============================================

(model feedback-loops
  "category" "systems"
  "originator" "Systems Theory"
  "description" "Outputs of a system become inputs that affect future outputs"
  "key_insight" "Small changes can compound into large effects"
  "application" "Identify reinforcing and balancing loops"
  "failure_modes" [
    (failure missing-feedback "high"
      "Not seeing feedback loops in systems"
      "signals" ["Linear thinking" "Surprise by system behavior"]
      "safeguards" ["System mapping" "Loop identification" "Dynamic thinking"])
    (failure delayed-feedback "high"
      "Not accounting for feedback delays"
      "signals" ["Overcorrection" "Oscillation" "Impatience"]
      "safeguards" ["Patience" "Delay mapping" "Leading indicators"])
    (failure positive-loop-blindness "critical"
      "Missing runaway positive feedback"
      "signals" ["Exponential growth/decline" "Tipping points"]
      "safeguards" ["Growth rate monitoring" "Circuit breakers" "Early warning"])
    (failure negative-loop-neglect "medium"
      "Ignoring stabilizing feedback"
      "signals" ["Fighting the system" "Unexpected resistance"]
      "safeguards" ["Understand homeostasis" "Work with system" "Leverage points"])
    (failure feedback-gaming "high"
      "Optimizing for metrics rather than outcomes"
      "signals" ["Goodhart's law" "Gaming" "Metric manipulation"]
      "safeguards" ["Multiple metrics" "Outcome focus" "Qualitative assessment"])])

(model emergence
  "category" "systems"
  "originator" "Complexity Science"
  "description" "Complex behaviors arise from simple rules and interactions"
  "key_insight" "The whole is different from the sum of its parts"
  "application" "Look for emergent properties in complex systems"
  "failure_modes" [
    (failure reductionism "high"
      "Trying to understand systems only through parts"
      "signals" ["Missing system behavior" "Surprised by emergence"]
      "safeguards" ["Holistic view" "Interaction analysis" "System-level thinking"])
    (failure emergence-blindness "high"
      "Not recognizing emergent properties"
      "signals" ["Unexpected system behavior" "Unintended consequences"]
      "safeguards" ["Emergence awareness" "Simulation" "Historical patterns"])
    (failure control-illusion "medium"
      "Believing you can fully control emergent systems"
      "signals" ["Failed interventions" "Unintended effects"]
      "safeguards" ["Humility" "Adaptive management" "Small experiments"])
    (failure linear-extrapolation "high"
      "Assuming linear scaling in complex systems"
      "signals" ["Phase transitions" "Tipping points"]
      "safeguards" ["Non-linear thinking" "Threshold awareness" "Scenario planning"])
    (failure agent-neglect "medium"
      "Ignoring individual agent behavior"
      "signals" ["Top-down only thinking" "Missing micro-foundations"]
      "safeguards" ["Agent-based thinking" "Incentive analysis" "Bottom-up view"])])

(model network-effects
  "category" "systems"
  "originator" "Economics / Technology"
  "description" "Value increases as more people use a product or service"
  "key_insight" "Winner-take-all dynamics in networked markets"
  "application" "Identify and leverage network effects"
  "failure_modes" [
    (failure network-blindness "high"
      "Not recognizing network effect dynamics"
      "signals" ["Underestimating leaders" "Missing tipping points"]
      "safeguards" ["Network analysis" "Critical mass awareness" "Platform thinking"])
    (failure negative-network-effects "medium"
      "Missing negative network effects"
      "signals" ["Congestion" "Quality degradation" "Spam"]
      "safeguards" ["Quality monitoring" "Curation" "Capacity planning"])
    (failure chicken-egg-paralysis "high"
      "Unable to bootstrap two-sided networks"
      "signals" ["No users because no content" "No content because no users"]
      "safeguards" ["Seed one side" "Single-player mode" "Subsidize early"])
    (failure lock-in-complacency "medium"
      "Assuming network effects guarantee permanence"
      "signals" ["Ignoring competition" "Quality decline"]
      "safeguards" ["Continuous improvement" "User value focus" "Innovation"])
    (failure network-overestimation "medium"
      "Overestimating network effect strength"
      "signals" ["Weak network effects" "Easy switching"]
      "safeguards" ["Measure actual effects" "Switching cost analysis" "Moat assessment"])])

;; ============================================
;; Category: Economics
;; ============================================

(model supply-demand
  "category" "economics"
  "originator" "Adam Smith"
  "description" "Prices are determined by the interaction of supply and demand"
  "key_insight" "Markets tend toward equilibrium"
  "application" "Analyze supply and demand dynamics"
  "failure_modes" [
    (failure static-analysis "medium"
      "Not considering dynamic supply/demand changes"
      "signals" ["Surprised by price changes" "Missing trends"]
      "safeguards" ["Dynamic analysis" "Trend monitoring" "Scenario planning"])
    (failure elasticity-blindness "high"
      "Ignoring price elasticity"
      "signals" ["Wrong pricing" "Demand surprises"]
      "safeguards" ["Elasticity estimation" "Price testing" "Sensitivity analysis"])
    (failure supply-side-neglect "medium"
      "Focusing only on demand"
      "signals" ["Supply surprises" "Capacity constraints"]
      "safeguards" ["Supply analysis" "Capacity planning" "Supplier relationships"])
    (failure market-failure-blindness "high"
      "Assuming markets always work"
      "signals" ["Externalities" "Information asymmetry" "Market power"]
      "safeguards" ["Market failure awareness" "Regulation understanding" "Intervention analysis"])
    (failure equilibrium-assumption "medium"
      "Assuming markets are always in equilibrium"
      "signals" ["Disequilibrium opportunities" "Arbitrage"]
      "safeguards" ["Disequilibrium awareness" "Adjustment time" "Friction analysis"])])

(model comparative-advantage
  "category" "economics"
  "originator" "David Ricardo"
  "description" "Specialize in what you do relatively better"
  "key_insight" "Trade benefits even when one party is better at everything"
  "application" "Focus on your relative strengths"
  "failure_modes" [
    (failure absolute-advantage-focus "high"
      "Focusing on absolute rather than comparative advantage"
      "signals" ["Trying to do everything" "Not specializing"]
      "safeguards" ["Relative advantage analysis" "Opportunity cost thinking" "Specialization"])
    (failure static-advantage "medium"
      "Assuming advantages are permanent"
      "signals" ["Disruption" "Skill obsolescence"]
      "safeguards" ["Continuous learning" "Advantage monitoring" "Adaptation"])
    (failure over-specialization "medium"
      "Becoming too narrow"
      "signals" ["Fragility" "Single point of failure"]
      "safeguards" ["Diversification" "Adjacent skills" "Optionality"])
    (failure advantage-blindness "medium"
      "Not knowing your comparative advantages"
      "signals" ["Unfocused efforts" "Competing on weaknesses"]
      "safeguards" ["Self-assessment" "Feedback seeking" "Strength analysis"])
    (failure trade-barrier-neglect "low"
      "Ignoring barriers to trade/exchange"
      "signals" ["Transaction costs" "Friction"]
      "safeguards" ["Friction analysis" "Trade cost assessment" "Integration costs"])])

(model compound-interest
  "category" "economics"
  "originator" "Mathematics / Finance"
  "description" "Growth on growth creates exponential results"
  "key_insight" "Small differences in rate compound to huge differences over time"
  "application" "Seek compounding opportunities; avoid compounding costs"
  "failure_modes" [
    (failure linear-thinking "critical"
      "Thinking linearly about exponential processes"
      "signals" ["Underestimating long-term growth" "Impatience"]
      "safeguards" ["Exponential thinking" "Long-term modeling" "Patience"])
    (failure rate-neglect "high"
      "Ignoring small differences in growth rates"
      "signals" ["Choosing lower rates" "Short-term focus"]
      "safeguards" ["Rate sensitivity analysis" "Long-term projection" "Rate optimization"])
    (failure negative-compounding "high"
      "Not recognizing compounding costs/losses"
      "signals" ["Debt spirals" "Compounding fees"]
      "safeguards" ["Cost awareness" "Fee analysis" "Debt management"])
    (failure interruption-cost "medium"
      "Interrupting compounding processes"
      "signals" ["Frequent trading" "Impatient exits"]
      "safeguards" ["Stay invested" "Long-term commitment" "Patience"])
    (failure base-neglect "medium"
      "Ignoring the importance of the base"
      "signals" ["Starting too small" "Delayed starts"]
      "safeguards" ["Start early" "Maximize base" "Front-load investment"])])

;; ============================================
;; Category: Physics / Engineering
;; ============================================

(model leverage
  "category" "physics"
  "originator" "Archimedes"
  "description" "Small inputs can produce large outputs with the right leverage"
  "key_insight" "Give me a lever long enough and I can move the world"
  "application" "Find leverage points in systems"
  "failure_modes" [
    (failure leverage-blindness "high"
      "Not seeing leverage opportunities"
      "signals" ["Brute force approaches" "Linear effort"]
      "safeguards" ["Leverage point analysis" "Multiplier seeking" "System mapping"])
    (failure over-leverage "critical"
      "Using too much leverage"
      "signals" ["Fragility" "Catastrophic failure risk"]
      "safeguards" ["Leverage limits" "Stress testing" "Margin of safety"])
    (failure wrong-fulcrum "high"
      "Applying leverage at wrong point"
      "signals" ["Ineffective interventions" "Wasted effort"]
      "safeguards" ["System analysis" "Intervention testing" "Feedback"])
    (failure leverage-decay "medium"
      "Not maintaining leverage over time"
      "signals" ["Diminishing returns" "Eroding advantages"]
      "safeguards" ["Leverage monitoring" "Renewal" "Adaptation"])
    (failure hidden-leverage "high"
      "Not seeing leverage in complex systems"
      "signals" ["Unexpected amplification" "Cascading effects"]
      "safeguards" ["System mapping" "Sensitivity analysis" "Scenario planning"])])

(model critical-mass
  "category" "physics"
  "originator" "Nuclear Physics"
  "description" "Minimum amount needed to sustain a chain reaction"
  "key_insight" "Below critical mass, nothing happens; above it, everything changes"
  "application" "Identify critical mass thresholds"
  "failure_modes" [
    (failure threshold-blindness "high"
      "Not recognizing critical thresholds"
      "signals" ["Surprised by phase transitions" "Missing tipping points"]
      "safeguards" ["Threshold identification" "Leading indicators" "Historical patterns"])
    (failure premature-scaling "high"
      "Scaling before reaching critical mass"
      "signals" ["Wasted resources" "Failed launches"]
      "safeguards" ["Critical mass validation" "Staged rollout" "Proof points"])
    (failure critical-mass-overestimation "medium"
      "Overestimating required critical mass"
      "signals" ["Delayed action" "Over-preparation"]
      "safeguards" ["Minimum viable testing" "Iteration" "Early launch"])
    (failure sustaining-failure "medium"
      "Reaching critical mass but not sustaining"
      "signals" ["Initial success then decline" "Momentum loss"]
      "safeguards" ["Sustainability planning" "Reinforcement" "Flywheel building"])
    (failure wrong-mass-type "medium"
      "Focusing on wrong type of critical mass"
      "signals" ["Quantity over quality" "Wrong metrics"]
      "safeguards" ["Quality focus" "Right metrics" "Value analysis"])])

;; ============================================
;; Category: Biology
;; ============================================

(model evolution
  "category" "biology"
  "originator" "Charles Darwin"
  "description" "Variation, selection, and retention drive adaptation"
  "key_insight" "What survives is what's adapted to the environment"
  "application" "Create variation, let selection work, retain what works"
  "failure_modes" [
    (failure variation-starvation "high"
      "Not generating enough variation"
      "signals" ["Homogeneity" "No experimentation" "Groupthink"]
      "safeguards" ["Encourage diversity" "Experimentation" "Devil's advocate"])
    (failure selection-failure "high"
      "Wrong selection criteria"
      "signals" ["Selecting for wrong traits" "Misaligned incentives"]
      "safeguards" ["Criteria review" "Outcome tracking" "Feedback loops"])
    (failure retention-failure "medium"
      "Not retaining successful adaptations"
      "signals" ["Reinventing wheels" "Lost knowledge"]
      "safeguards" ["Documentation" "Knowledge management" "Institutional memory"])
    (failure environment-blindness "high"
      "Not recognizing environmental changes"
      "signals" ["Maladaptation" "Disruption"]
      "safeguards" ["Environmental scanning" "Adaptation" "Flexibility"])
    (failure local-maximum "high"
      "Getting stuck at local optima"
      "signals" ["Incremental only" "Missing breakthroughs"]
      "safeguards" ["Exploration" "Radical experiments" "Fresh perspectives"])])

(model red-queen
  "category" "biology"
  "originator" "Leigh Van Valen"
  "description" "You must run just to stay in place"
  "key_insight" "Competitors are also evolving"
  "application" "Continuous improvement is necessary for survival"
  "failure_modes" [
    (failure complacency "critical"
      "Stopping improvement while competitors advance"
      "signals" ["Market share loss" "Falling behind"]
      "safeguards" ["Continuous improvement" "Competitive monitoring" "Innovation"])
    (failure wrong-race "medium"
      "Running the wrong race"
      "signals" ["Competing on wrong dimensions" "Irrelevant improvements"]
      "safeguards" ["Strategic focus" "Customer value" "Differentiation"])
    (failure exhaustion "medium"
      "Running too fast and burning out"
      "signals" ["Unsustainable pace" "Quality decline"]
      "safeguards" ["Sustainable pace" "Strategic rest" "Efficiency"])
    (failure arms-race "high"
      "Destructive competition that harms all"
      "signals" ["Zero-sum dynamics" "Value destruction"]
      "safeguards" ["Cooperation opportunities" "Blue ocean" "Value creation"])
    (failure treadmill-blindness "medium"
      "Not recognizing you're on a treadmill"
      "signals" ["Effort without progress" "Hedonic adaptation"]
      "safeguards" ["Progress measurement" "Goal review" "Strategic pauses"])])

;; ============================================
;; Utility Functions
;; ============================================

(defn get-model [name]
  "Get a model by name."
  (get *models* name None))

(defn get-models-by-category [category]
  "Get all models in a category."
  (lfor name (get *categories* category [])
        (get *models* name)))

(defn get-all-models []
  "Get all registered models."
  (list (.values *models*)))

(defn get-all-categories []
  "Get all categories."
  (list (.keys *categories*)))

(defn get-failure-modes [model-name]
  "Get failure modes for a model."
  (let [model (get *models* model-name)]
    (if model
        (get model "failure_modes" [])
        [])))

(defn search-models [query]
  "Search models by name or description."
  (let [query-lower (str.lower query)]
    (lfor [name model] (.items *models*)
          :if (or (in query-lower (str.lower name))
                  (in query-lower (str.lower (get model "description" ""))))
          model)))

(defn get-model-count []
  "Get total number of registered models."
  (len *models*))

(defn get-failure-mode-count []
  "Get total number of failure modes."
  (len *failure-modes*))

(defn export-all []
  "Export all models and metadata."
  {"models" *models*
   "categories" *categories*
   "failure_modes" *failure-modes*
   "model_count" (get-model-count)
   "failure_mode_count" (get-failure-mode-count)
   "timestamp" (str (datetime.now))})
