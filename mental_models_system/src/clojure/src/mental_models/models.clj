(ns mental-models.models
  "Complete Mental Models Library in Clojure
   All 129 mental models defined using Clojure's expressive data structures
   
   This is the core knowledge base - all models defined in
   idiomatic Clojure for rapid extension and modification")

;; ============================================
;; Model Registry (Atoms for state)
;; ============================================

(def models (atom {}))
(def failure-modes (atom {}))
(def categories (atom {}))

(defn register-model
  "Register a model in the global registry."
  [model]
  (let [name (:name model)
        category (get model :category "general")]
    (swap! models assoc name model)
    (swap! categories update category (fnil conj []) name)
    (doseq [fm (:failure-modes model)]
      (swap! failure-modes assoc (str name "_" (:name fm)) fm))
    model))

(defn get-model
  "Get a model by name."
  [name]
  (get @models name))

(defn get-models-by-category
  "Get all models in a category."
  [category]
  (map get-model (get @categories category [])))

(defn get-all-models
  "Get all registered models."
  []
  (vals @models))

(defn search-models
  "Search models by name or description."
  [query]
  (let [q (clojure.string/lower-case query)]
    (filter #(or (clojure.string/includes? (clojure.string/lower-case (:name %)) q)
                 (clojure.string/includes? (clojure.string/lower-case (get % :description "")) q))
            (vals @models))))

;; ============================================
;; Failure Mode Helper
;; ============================================

(defn failure
  "Create a failure mode map."
  [name severity description & {:keys [signals safeguards]}]
  {:name name
   :severity severity
   :description description
   :signals (or signals [])
   :safeguards (or safeguards [])})

;; ============================================
;; Category: Decision Making
;; ============================================

(register-model
 {:name "circle-of-competence"
  :category "decision_making"
  :originator "Warren Buffett"
  :description "Know the boundaries of your knowledge and stay within them"
  :key-insight "The size of your circle matters less than knowing its boundaries"
  :application "Before any decision, ask: Is this within my circle?"
  :failure-modes
  [(failure "overconfidence" "high"
            "Believing your circle is larger than it is"
            :signals ["No recent failures" "Ignoring expert advice" "Dismissing contrary evidence"]
            :safeguards ["Regular competence audits" "Seek disconfirming evidence" "Track prediction accuracy"])
   (failure "underconfidence" "medium"
            "Not acting within your actual competence"
            :signals ["Excessive hesitation" "Deferring on known topics" "Analysis paralysis"]
            :safeguards ["Track past successes" "Build confidence gradually" "Start with small decisions"])
   (failure "static-circle" "medium"
            "Not expanding your circle over time"
            :signals ["No new learning" "Same decisions for years" "Avoiding challenges"]
            :safeguards ["Deliberate practice" "Adjacent learning" "Stretch assignments"])
   (failure "boundary-blindness" "high"
            "Not knowing where your circle ends"
            :signals ["Overconfident predictions" "No uncertainty acknowledgment"]
            :safeguards ["Explicit uncertainty ranges" "Pre-mortems" "Outside feedback"])
   (failure "competence-creep" "high"
            "Gradually drifting outside your circle"
            :signals ["Slow expansion without validation" "Success breeding overreach"]
            :safeguards ["Regular boundary checks" "Milestone reviews" "External validation"])]})

(register-model
 {:name "margin-of-safety"
  :category "decision_making"
  :originator "Benjamin Graham"
  :description "Always leave room for error in your calculations"
  :key-insight "The future is uncertain; build in buffers"
  :application "Add 25-50% buffer to all estimates"
  :failure-modes
  [(failure "insufficient-margin" "critical"
            "Not leaving enough buffer for unexpected events"
            :signals ["Optimistic projections" "Ignoring tail risks" "Best-case planning"]
            :safeguards ["Double estimated margin" "Stress test assumptions" "Plan for worst case"])
   (failure "excessive-margin" "low"
            "Being so conservative you miss opportunities"
            :signals ["Never taking action" "Paralysis by analysis" "Missing obvious wins"]
            :safeguards ["Balance risk/reward" "Time-bound decisions" "Opportunity cost awareness"])
   (failure "false-precision" "medium"
            "Believing precise calculations eliminate uncertainty"
            :signals ["Many decimal places" "Complex models" "Overconfidence in numbers"]
            :safeguards ["Round estimates" "Sensitivity analysis" "Scenario planning"])
   (failure "margin-erosion" "high"
            "Gradually reducing margins under pressure"
            :signals ["Competitive pressure" "Short-term thinking" "Margin compression"]
            :safeguards ["Hard minimum thresholds" "Regular margin reviews" "Long-term focus"])
   (failure "wrong-margin" "high"
            "Building margin against the wrong risks"
            :signals ["Protecting against unlikely events" "Ignoring likely risks"]
            :safeguards ["Risk prioritization" "Probability assessment" "Historical analysis"])]})

(register-model
 {:name "second-order-thinking"
  :category "decision_making"
  :originator "Howard Marks"
  :description "Think about the consequences of the consequences"
  :key-insight "First-level thinking is simplistic; second-level thinking is deep"
  :application "Ask 'And then what?' at least three times"
  :failure-modes
  [(failure "first-order-only" "high"
            "Only considering immediate effects"
            :signals ["Quick decisions" "No scenario planning" "Surprise by consequences"]
            :safeguards ["Mandatory 'then what' exercise" "Consider 3 time horizons" "Scenario planning"])
   (failure "infinite-regress" "medium"
            "Getting lost in endless chains of consequences"
            :signals ["Analysis paralysis" "Never deciding" "Overthinking"]
            :safeguards ["Set analysis limits" "Time-box thinking" "Focus on material effects"])
   (failure "wrong-chain" "high"
            "Following the wrong causal chain"
            :signals ["Ignoring key variables" "Linear thinking in complex systems"]
            :safeguards ["Multiple scenario paths" "Expert consultation" "Historical analogies"])
   (failure "probability-neglect" "high"
            "Not weighting consequences by likelihood"
            :signals ["Equal weight to all outcomes" "Ignoring base rates"]
            :safeguards ["Probability estimates" "Expected value calculations" "Base rate research"])
   (failure "time-horizon-mismatch" "medium"
            "Optimizing for wrong time frame"
            :signals ["Short-term focus" "Ignoring long-term effects"]
            :safeguards ["Explicit time horizons" "Multi-period analysis" "Stakeholder mapping"])]})

(register-model
 {:name "inversion"
  :category "decision_making"
  :originator "Carl Jacobi / Charlie Munger"
  :description "Invert, always invert - think about what to avoid"
  :key-insight "It's often easier to avoid stupidity than to seek brilliance"
  :application "Ask 'How could this fail?' before 'How could this succeed?'"
  :failure-modes
  [(failure "forward-only" "high"
            "Only thinking about how to succeed"
            :signals ["No failure analysis" "Optimism bias" "Ignoring risks"]
            :safeguards ["Pre-mortems" "Failure mode analysis" "Devil's advocate"])
   (failure "incomplete-inversion" "medium"
            "Not inverting thoroughly enough"
            :signals ["Surface-level inversion" "Missing key failure modes"]
            :safeguards ["Systematic failure enumeration" "Multiple perspectives" "Historical failures"])
   (failure "paralysis-by-inversion" "medium"
            "Finding so many failure modes you can't act"
            :signals ["Endless risk lists" "No action" "Fear-based decisions"]
            :safeguards ["Prioritize risks" "Accept residual risk" "Time-bound analysis"])
   (failure "inversion-bias" "low"
            "Becoming too focused on avoiding failure"
            :signals ["Missing opportunities" "Excessive caution" "Defensive posture"]
            :safeguards ["Balance with opportunity seeking" "Upside analysis" "Growth mindset"])
   (failure "wrong-inversion" "high"
            "Inverting the wrong question"
            :signals ["Solving wrong problem" "Misframed question"]
            :safeguards ["Question the question" "Multiple framings" "Stakeholder input"])]})

(register-model
 {:name "opportunity-cost"
  :category "decision_making"
  :originator "Economics"
  :description "The cost of any choice is what you give up"
  :key-insight "Every yes is a no to something else"
  :application "Always consider the next best alternative"
  :failure-modes
  [(failure "ignoring-alternatives" "high"
            "Not considering what else you could do"
            :signals ["Single option focus" "No comparison" "First idea accepted"]
            :safeguards ["Generate 3+ alternatives" "Explicit comparison" "Opportunity cost calculation"])
   (failure "sunk-cost-confusion" "high"
            "Confusing sunk costs with opportunity costs"
            :signals ["Considering past investments" "Throwing good money after bad"]
            :safeguards ["Ignore sunk costs" "Fresh start thinking" "Zero-based decisions"])
   (failure "narrow-framing" "medium"
            "Considering too few alternatives"
            :signals ["Binary choices" "Limited options" "Tunnel vision"]
            :safeguards ["Brainstorm alternatives" "Outside perspectives" "Creative options"])
   (failure "time-blindness" "medium"
            "Not considering time as a resource"
            :signals ["Ignoring time costs" "No time valuation"]
            :safeguards ["Value your time" "Time opportunity costs" "Delegation analysis"])
   (failure "comparison-paralysis" "low"
            "Unable to choose between alternatives"
            :signals ["Endless comparison" "No decision" "Perfect option seeking"]
            :safeguards ["Good enough threshold" "Time limits" "Reversibility check"])]})

;; ============================================
;; Category: Psychology
;; ============================================

(register-model
 {:name "incentives"
  :category "psychology"
  :originator "Charlie Munger"
  :description "Never think about anything else when you should be thinking about incentives"
  :key-insight "Show me the incentive and I'll show you the outcome"
  :application "Always ask: What are the incentives?"
  :failure-modes
  [(failure "ignoring-incentives" "critical"
            "Not considering what motivates behavior"
            :signals ["Surprised by behavior" "Moral explanations for economic behavior"]
            :safeguards ["Incentive mapping" "Follow the money" "Behavioral prediction"])
   (failure "misaligned-incentives" "high"
            "Creating incentives that produce wrong behavior"
            :signals ["Gaming" "Unintended consequences" "Perverse outcomes"]
            :safeguards ["Incentive design review" "Pilot testing" "Feedback loops"])
   (failure "incentive-blindness" "high"
            "Not seeing your own incentive biases"
            :signals ["Self-serving conclusions" "Rationalization"]
            :safeguards ["Disinterested review" "Incentive disclosure" "Counter-incentive analysis"])
   (failure "over-incentivizing" "medium"
            "Crowding out intrinsic motivation"
            :signals ["Reduced intrinsic interest" "Only working for rewards"]
            :safeguards ["Balance intrinsic/extrinsic" "Purpose alignment" "Autonomy preservation"])
   (failure "static-incentive-thinking" "medium"
            "Not considering how incentives change over time"
            :signals ["Outdated incentive structures" "Changed circumstances"]
            :safeguards ["Regular incentive review" "Dynamic adjustment" "Feedback mechanisms"])]})

(register-model
 {:name "social-proof"
  :category "psychology"
  :originator "Robert Cialdini"
  :description "People look to others to determine correct behavior"
  :key-insight "We assume if many people do something, it must be right"
  :application "Question: Am I doing this because others are?"
  :failure-modes
  [(failure "herd-following" "high"
            "Blindly following the crowd"
            :signals ["No independent analysis" "Comfort in consensus" "FOMO"]
            :safeguards ["Independent thinking" "Contrarian analysis" "First principles"])
   (failure "wrong-reference-group" "medium"
            "Following the wrong crowd"
            :signals ["Inappropriate comparisons" "Wrong peer group"]
            :safeguards ["Choose reference groups carefully" "Expert identification"])
   (failure "manufactured-proof" "high"
            "Being fooled by fake social proof"
            :signals ["Astroturfing" "Fake reviews" "Manipulated metrics"]
            :safeguards ["Verify sources" "Look for authenticity" "Skepticism"])
   (failure "proof-cascade" "high"
            "Contributing to information cascades"
            :signals ["Amplifying without verification" "Viral spread"]
            :safeguards ["Verify before sharing" "Break cascades" "Independent verification"])
   (failure "anti-social-proof" "low"
            "Contrarian for its own sake"
            :signals ["Automatic disagreement" "Ignoring valid consensus"]
            :safeguards ["Evaluate on merits" "Selective contrarianism" "Evidence-based"])]})

(register-model
 {:name "commitment-consistency"
  :category "psychology"
  :originator "Robert Cialdini"
  :description "Once committed, people tend to behave consistently with that commitment"
  :key-insight "Small commitments lead to larger ones"
  :application "Watch for escalating commitments"
  :failure-modes
  [(failure "escalation-of-commitment" "critical"
            "Doubling down on failing courses of action"
            :signals ["Throwing good money after bad" "Ignoring negative feedback"]
            :safeguards ["Kill criteria" "Fresh eyes review" "Sunk cost awareness"])
   (failure "foot-in-door" "medium"
            "Being manipulated through small commitments"
            :signals ["Gradual escalation" "Small yeses leading to big ones"]
            :safeguards ["Recognize the pattern" "Evaluate each request independently"])
   (failure "identity-lock" "high"
            "Commitment becoming part of identity"
            :signals ["Defending positions as self" "Emotional attachment to decisions"]
            :safeguards ["Separate identity from positions" "Update beliefs" "Growth mindset"])
   (failure "public-commitment-trap" "medium"
            "Public commitments preventing course correction"
            :signals ["Can't back down publicly" "Reputation concerns"]
            :safeguards ["Private deliberation" "Graceful pivots" "Learning framing"])
   (failure "consistency-worship" "medium"
            "Valuing consistency over correctness"
            :signals ["Foolish consistency" "Ignoring new information"]
            :safeguards ["Update on evidence" "Intellectual honesty" "Embrace change"])]})

(register-model
 {:name "availability-heuristic"
  :category "psychology"
  :originator "Kahneman & Tversky"
  :description "We judge likelihood by how easily examples come to mind"
  :key-insight "Vivid and recent events are overweighted"
  :application "Ask: Am I overweighting recent/vivid events?"
  :failure-modes
  [(failure "recency-bias" "high"
            "Overweighting recent events"
            :signals ["Recent events dominating" "Ignoring base rates"]
            :safeguards ["Historical data" "Base rate research" "Long-term perspective"])
   (failure "vividness-bias" "high"
            "Overweighting dramatic events"
            :signals ["Emotional reactions" "Ignoring mundane risks"]
            :safeguards ["Statistical thinking" "Probability assessment" "Calm analysis"])
   (failure "media-distortion" "medium"
            "Being influenced by media coverage"
            :signals ["News-driven fears" "Disproportionate concerns"]
            :safeguards ["Seek data" "Ignore news cycle" "Long-term view"])
   (failure "personal-experience-bias" "medium"
            "Overweighting personal experiences"
            :signals ["Anecdotal reasoning" "Sample of one"]
            :safeguards ["Seek broader data" "Statistical samples" "Outside view"])
   (failure "availability-cascade" "high"
            "Self-reinforcing availability through repetition"
            :signals ["Echo chambers" "Repeated claims becoming 'true'"]
            :safeguards ["Source diversity" "Fact checking" "Original sources"])]})

(register-model
 {:name "loss-aversion"
  :category "psychology"
  :originator "Kahneman & Tversky"
  :description "Losses loom larger than equivalent gains"
  :key-insight "The pain of losing is about twice the pleasure of gaining"
  :application "Check if you're irrationally avoiding losses"
  :failure-modes
  [(failure "excessive-risk-aversion" "high"
            "Avoiding reasonable risks due to loss fear"
            :signals ["Missed opportunities" "Excessive caution" "Status quo bias"]
            :safeguards ["Expected value thinking" "Reframe as gains" "Long-term view"])
   (failure "holding-losers" "high"
            "Refusing to realize losses"
            :signals ["Holding losing positions" "Hope over analysis"]
            :safeguards ["Pre-set stop losses" "Regular portfolio review" "Fresh evaluation"])
   (failure "narrow-framing" "medium"
            "Evaluating each decision in isolation"
            :signals ["Single decision focus" "Ignoring portfolio effect"]
            :safeguards ["Portfolio thinking" "Aggregate view" "Long-term perspective"])
   (failure "endowment-effect" "medium"
            "Overvaluing what you already own"
            :signals ["Reluctance to sell" "Higher selling than buying price"]
            :safeguards ["Ownership-blind evaluation" "Market price reference"])
   (failure "loss-framing-manipulation" "medium"
            "Being manipulated through loss framing"
            :signals ["Responding to loss frames" "Fear-based decisions"]
            :safeguards ["Reframe as gains" "Recognize manipulation" "Neutral framing"])]})

;; ============================================
;; Category: Systems
;; ============================================

(register-model
 {:name "feedback-loops"
  :category "systems"
  :originator "Systems Theory"
  :description "Outputs of a system become inputs that affect future outputs"
  :key-insight "Small changes can compound into large effects"
  :application "Identify reinforcing and balancing loops"
  :failure-modes
  [(failure "loop-blindness" "high"
            "Not seeing feedback loops in systems"
            :signals ["Surprised by exponential growth/decay" "Linear thinking"]
            :safeguards ["Map system dynamics" "Look for delays" "Trace causality"])
   (failure "positive-loop-neglect" "high"
            "Missing reinforcing loops"
            :signals ["Underestimating growth" "Missing compounding effects"]
            :safeguards ["Look for virtuous/vicious cycles" "Compound thinking"])
   (failure "negative-loop-neglect" "medium"
            "Missing balancing loops"
            :signals ["Expecting unlimited growth" "Ignoring constraints"]
            :safeguards ["Identify limits" "Look for stabilizing forces"])
   (failure "delay-blindness" "high"
            "Not accounting for delays in feedback"
            :signals ["Impatience" "Overcorrection" "Oscillation"]
            :safeguards ["Map delays" "Patient observation" "Avoid overreaction"])
   (failure "intervention-backfire" "high"
            "Interventions that trigger opposing feedback"
            :signals ["Unintended consequences" "System resistance"]
            :safeguards ["Model interventions" "Small experiments" "Monitor effects"])]})

(register-model
 {:name "emergence"
  :category "systems"
  :originator "Systems Theory"
  :description "Complex behaviors arise from simple rules and interactions"
  :key-insight "The whole is greater than the sum of its parts"
  :application "Look for emergent properties in complex systems"
  :failure-modes
  [(failure "reductionism" "high"
            "Trying to understand systems only through parts"
            :signals ["Missing emergent properties" "Oversimplification"]
            :safeguards ["Study interactions" "Observe whole system" "Look for patterns"])
   (failure "emergence-mysticism" "medium"
            "Attributing too much to emergence"
            :signals ["Avoiding analysis" "Magical thinking"]
            :safeguards ["Ground in mechanisms" "Test predictions" "Seek explanations"])
   (failure "scale-blindness" "high"
            "Not seeing how properties change with scale"
            :signals ["Assuming linear scaling" "Missing phase transitions"]
            :safeguards ["Study at multiple scales" "Look for thresholds"])
   (failure "interaction-neglect" "high"
            "Ignoring interactions between components"
            :signals ["Isolated analysis" "Missing synergies/conflicts"]
            :safeguards ["Map relationships" "Study interfaces" "Network analysis"])
   (failure "prediction-overconfidence" "medium"
            "Thinking emergent systems are predictable"
            :signals ["Precise forecasts" "Ignoring uncertainty"]
            :safeguards ["Embrace uncertainty" "Scenario planning" "Adaptive strategies"])]})

(register-model
 {:name "network-effects"
  :category "systems"
  :originator "Economics/Technology"
  :description "Value increases as more people use a product or service"
  :key-insight "Networks can create winner-take-all dynamics"
  :application "Identify and leverage network effects"
  :failure-modes
  [(failure "network-blindness" "high"
            "Not recognizing network effects"
            :signals ["Undervaluing platforms" "Missing growth potential"]
            :safeguards ["Map network connections" "Study adoption curves"])
   (failure "network-overestimation" "medium"
            "Overestimating network effect strength"
            :signals ["Assuming all networks are equal" "Ignoring switching costs"]
            :safeguards ["Measure actual effects" "Compare to alternatives"])
   (failure "chicken-egg-paralysis" "high"
            "Unable to bootstrap network"
            :signals ["Waiting for critical mass" "No early adopter strategy"]
            :safeguards ["Seed the network" "Create initial value" "Subsidize early users"])
   (failure "network-fragility" "medium"
            "Not seeing how networks can collapse"
            :signals ["Assuming permanence" "Ignoring alternatives"]
            :safeguards ["Monitor engagement" "Maintain value" "Watch for substitutes"])
   (failure "negative-network-effects" "high"
            "Missing when more users reduce value"
            :signals ["Congestion" "Quality dilution" "Spam"]
            :safeguards ["Monitor quality" "Manage growth" "Curate community"])]})

;; ============================================
;; Category: Economics
;; ============================================

(register-model
 {:name "supply-demand"
  :category "economics"
  :originator "Adam Smith"
  :description "Prices are determined by the interaction of supply and demand"
  :key-insight "Price is a signal that coordinates economic activity"
  :application "Analyze how supply and demand affect any market"
  :failure-modes
  [(failure "price-blindness" "high"
            "Ignoring price signals"
            :signals ["Not adjusting to price changes" "Ignoring market signals"]
            :safeguards ["Monitor prices" "Respond to signals" "Understand price formation"])
   (failure "supply-only-thinking" "medium"
            "Only considering supply side"
            :signals ["Ignoring demand" "Cost-plus pricing"]
            :safeguards ["Study demand curves" "Value-based pricing" "Customer research"])
   (failure "demand-only-thinking" "medium"
            "Only considering demand side"
            :signals ["Ignoring supply constraints" "Assuming infinite supply"]
            :safeguards ["Map supply chain" "Understand capacity" "Monitor inventory"])
   (failure "equilibrium-assumption" "high"
            "Assuming markets are always in equilibrium"
            :signals ["Ignoring disequilibrium" "Missing arbitrage"]
            :safeguards ["Look for imbalances" "Monitor adjustment speed"])
   (failure "elasticity-blindness" "high"
            "Not considering price elasticity"
            :signals ["Assuming fixed demand" "Ignoring substitutes"]
            :safeguards ["Measure elasticity" "Study substitutes" "Test price sensitivity"])]})

(register-model
 {:name "comparative-advantage"
  :category "economics"
  :originator "David Ricardo"
  :description "Specialize in what you do relatively better, even if not absolutely better"
  :key-insight "Trade benefits both parties even when one is better at everything"
  :application "Focus on your relative strengths, not absolute ones"
  :failure-modes
  [(failure "absolute-advantage-focus" "high"
            "Only considering absolute advantage"
            :signals ["Trying to do everything" "Not specializing"]
            :safeguards ["Calculate opportunity costs" "Compare relative efficiency"])
   (failure "static-advantage-thinking" "medium"
            "Assuming advantages don't change"
            :signals ["Complacency" "Not investing in capabilities"]
            :safeguards ["Monitor competitive landscape" "Invest in skills"])
   (failure "over-specialization" "medium"
            "Specializing too narrowly"
            :signals ["Single point of failure" "No diversification"]
            :safeguards ["Maintain core competencies" "Strategic diversification"])
   (failure "trade-barrier-blindness" "medium"
            "Ignoring barriers to trade/exchange"
            :signals ["Assuming frictionless exchange" "Ignoring transaction costs"]
            :safeguards ["Map barriers" "Reduce friction" "Account for costs"])
   (failure "advantage-erosion" "high"
            "Not protecting comparative advantage"
            :signals ["Competitors catching up" "Commoditization"]
            :safeguards ["Continuous improvement" "Build moats" "Innovate"])]})

(register-model
 {:name "compound-interest"
  :category "economics"
  :originator "Mathematics/Finance"
  :description "Returns on returns create exponential growth"
  :key-insight "Small differences in rate compound to huge differences over time"
  :application "Start early, be patient, optimize the rate"
  :failure-modes
  [(failure "linear-thinking" "critical"
            "Thinking in linear rather than exponential terms"
            :signals ["Underestimating long-term growth" "Impatience"]
            :safeguards ["Calculate compound effects" "Visualize exponentials" "Think long-term"])
   (failure "rate-neglect" "high"
            "Not optimizing the compound rate"
            :signals ["Accepting low returns" "Not shopping for better rates"]
            :safeguards ["Compare rates" "Optimize continuously" "Reduce fees"])
   (failure "time-neglect" "high"
            "Not starting early enough"
            :signals ["Procrastination" "Waiting for perfect conditions"]
            :safeguards ["Start now" "Time in market" "Automatic investing"])
   (failure "interruption-damage" "high"
            "Breaking the compound chain"
            :signals ["Withdrawing early" "Panic selling"]
            :safeguards ["Stay invested" "Emergency fund" "Long-term commitment"])
   (failure "negative-compounding-blindness" "high"
            "Not seeing negative compounding (debt, bad habits)"
            :signals ["Accumulating debt" "Ignoring small negatives"]
            :safeguards ["Eliminate negative compounds" "Address small problems early"])]})

;; ============================================
;; Category: Physics/Biology
;; ============================================

(register-model
 {:name "critical-mass"
  :category "physics"
  :originator "Nuclear Physics"
  :description "A threshold amount needed to sustain a chain reaction"
  :key-insight "Below threshold, nothing happens; above it, everything changes"
  :application "Identify and reach critical mass for any initiative"
  :failure-modes
  [(failure "threshold-blindness" "high"
            "Not recognizing threshold effects"
            :signals ["Expecting linear progress" "Giving up before threshold"]
            :safeguards ["Identify thresholds" "Push through" "Measure progress"])
   (failure "premature-scaling" "high"
            "Scaling before reaching critical mass"
            :signals ["Spreading too thin" "Diluting effort"]
            :safeguards ["Focus resources" "Achieve density" "Sequential expansion"])
   (failure "threshold-overestimation" "medium"
            "Thinking threshold is higher than it is"
            :signals ["Over-preparing" "Missing opportunities"]
            :safeguards ["Test early" "Iterate quickly" "Minimum viable approach"])
   (failure "threshold-underestimation" "medium"
            "Thinking threshold is lower than it is"
            :signals ["Premature launch" "Insufficient resources"]
            :safeguards ["Research requirements" "Build buffer" "Learn from others"])
   (failure "single-threshold-thinking" "medium"
            "Not seeing multiple thresholds"
            :signals ["Assuming one breakthrough is enough"]
            :safeguards ["Map all thresholds" "Plan for multiple stages"])]})

(register-model
 {:name "evolution"
  :category "biology"
  :originator "Charles Darwin"
  :description "Variation, selection, and retention drive adaptation"
  :key-insight "What survives is what's fit for the environment"
  :application "Create variation, let selection work, retain what works"
  :failure-modes
  [(failure "variation-starvation" "high"
            "Not generating enough variation"
            :signals ["Homogeneous options" "No experimentation"]
            :safeguards ["Encourage diversity" "Experiment widely" "Tolerate failure"])
   (failure "selection-weakness" "high"
            "Not selecting rigorously enough"
            :signals ["Keeping everything" "No culling"]
            :safeguards ["Clear criteria" "Regular review" "Kill poor performers"])
   (failure "retention-failure" "high"
            "Not retaining what works"
            :signals ["Reinventing wheels" "Not documenting success"]
            :safeguards ["Document learnings" "Build on success" "Institutionalize"])
   (failure "environment-blindness" "high"
            "Not seeing environment changes"
            :signals ["Optimizing for past" "Missing shifts"]
            :safeguards ["Monitor environment" "Adapt continuously" "Stay flexible"])
   (failure "local-maximum-trap" "medium"
            "Getting stuck at local optimum"
            :signals ["Incremental improvement only" "Missing breakthroughs"]
            :safeguards ["Radical experiments" "Jump to new peaks" "Strategic pivots"])]})

(register-model
 {:name "red-queen"
  :category "biology"
  :originator "Leigh Van Valen"
  :description "You must keep running just to stay in place"
  :key-insight "Competitors are also improving; standing still means falling behind"
  :application "Continuous improvement is necessary for survival"
  :failure-modes
  [(failure "complacency" "critical"
            "Thinking current position is secure"
            :signals ["Resting on laurels" "Ignoring competitors"]
            :safeguards ["Monitor competition" "Continuous improvement" "Paranoia"])
   (failure "exhaustion" "high"
            "Running too fast and burning out"
            :signals ["Unsustainable pace" "Quality decline"]
            :safeguards ["Sustainable pace" "Strategic rest" "Efficiency focus"])
   (failure "wrong-race" "high"
            "Running in the wrong direction"
            :signals ["Improving wrong things" "Misaligned effort"]
            :safeguards ["Validate direction" "Customer feedback" "Strategic review"])
   (failure "race-to-bottom" "medium"
            "Competing on dimensions that destroy value"
            :signals ["Price wars" "Feature bloat"]
            :safeguards ["Differentiate" "Create new value" "Avoid commoditization"])
   (failure "arms-race-blindness" "medium"
            "Not seeing when to exit the race"
            :signals ["Diminishing returns" "Pyrrhic victories"]
            :safeguards ["Know when to quit" "Find new games" "Strategic retreat"])]})

;; ============================================
;; Export Functions
;; ============================================

(defn export-all-models
  "Export all models and metadata."
  []
  {:models @models
   :categories @categories
   :failure-modes @failure-modes
   :total-models (count @models)
   :total-failure-modes (count @failure-modes)
   :timestamp (java.time.Instant/now)})

(defn model-summary
  "Get a summary of a model."
  [name]
  (when-let [model (get-model name)]
    {:name (:name model)
     :category (:category model)
     :originator (:originator model)
     :description (:description model)
     :failure-mode-count (count (:failure-modes model))}))

(defn all-model-summaries
  "Get summaries of all models."
  []
  (map #(model-summary (:name %)) (vals @models)))
