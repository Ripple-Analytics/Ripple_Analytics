(ns mental-models.scanner.classifier
  "Auto-classification pipeline for ALL 167 mental models
   
   Categories (Munger's Hierarchy):
   - Hard Sciences: Mathematics (15), Accounting (10), Physics (12), Chemistry (8), Engineering (11)
   - Life Sciences: Biology (12), Physiology (9), Psychology (25), Economics (18)
   - Applied Knowledge: Moats (15), Organizational (12), Thinking Tools (20)
   
   Total: 167 models with keyword detection, regex patterns, and LLM prompts"
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; ALL 167 MENTAL MODELS BY CATEGORY
;; =============================================================================

(def mental-models
  {;; =========================================================================
   ;; HARD SCIENCES (Foundation)
   ;; =========================================================================
   
   :mathematics
   [{:id "compound-interest" :name "Compound Interest"
     :keywords ["compound" "exponential" "growth" "interest" "accumulate" "snowball"]
     :description "Growth that builds on itself over time"}
    {:id "permutations-combinations" :name "Permutations & Combinations"
     :keywords ["combination" "permutation" "probability" "arrangements" "possibilities"]
     :description "Counting possible arrangements"}
    {:id "algebraic-equivalence" :name "Algebraic Equivalence"
     :keywords ["equation" "equal" "equivalent" "solve" "variable" "formula"]
     :description "Different expressions of the same value"}
    {:id "randomness" :name "Randomness"
     :keywords ["random" "chance" "luck" "unpredictable" "stochastic" "noise"]
     :description "Events without deterministic cause"}
    {:id "stochastic-processes" :name "Stochastic Processes"
     :keywords ["stochastic" "random walk" "markov" "monte carlo" "simulation"]
     :description "Processes with random elements"}
    {:id "compounding" :name "Compounding"
     :keywords ["compound" "multiply" "geometric" "exponential" "reinvest"]
     :description "Returns generating more returns"}
    {:id "multiplying-by-zero" :name "Multiplying by Zero"
     :keywords ["zero" "wipe out" "eliminate" "nullify" "destroy"]
     :description "One failure can destroy everything"}
    {:id "churn" :name "Churn"
     :keywords ["churn" "turnover" "attrition" "replacement" "loss rate"]
     :description "Rate of loss requiring replacement"}
    {:id "law-of-large-numbers" :name "Law of Large Numbers"
     :keywords ["average" "converge" "sample size" "expected value" "mean"]
     :description "Averages stabilize with more samples"}
    {:id "normal-distribution" :name "Normal Distribution"
     :keywords ["bell curve" "gaussian" "standard deviation" "normal" "average"]
     :description "Most values cluster around mean"}
    {:id "power-laws" :name "Power Laws"
     :keywords ["power law" "pareto" "80/20" "winner take all" "fat tail"]
     :description "Few items dominate the total"}
    {:id "regression-to-mean" :name "Regression to Mean"
     :keywords ["regression" "mean" "average" "revert" "normalize"]
     :description "Extreme values tend toward average"}
    {:id "order-of-magnitude" :name "Order of Magnitude"
     :keywords ["order of magnitude" "10x" "scale" "rough estimate" "ballpark"]
     :description "Approximate scale of quantities"}
    {:id "bayes-theorem" :name "Bayes Theorem"
     :keywords ["bayes" "prior" "posterior" "probability" "update" "likelihood"]
     :description "Updating beliefs with evidence"}
    {:id "fat-tails" :name "Fat Tails"
     :keywords ["fat tail" "black swan" "extreme" "outlier" "rare event"]
     :description "Extreme events more likely than expected"}]
   
   :accounting
   [{:id "double-entry" :name "Double-Entry Bookkeeping"
     :keywords ["debit" "credit" "balance" "ledger" "journal entry"]
     :description "Every transaction has two sides"}
    {:id "depreciation" :name "Depreciation"
     :keywords ["depreciation" "amortization" "useful life" "write-off" "asset value"]
     :description "Spreading cost over useful life"}
    {:id "accrual-accounting" :name "Accrual Accounting"
     :keywords ["accrual" "recognize" "earned" "incurred" "matching"]
     :description "Record when earned, not when paid"}
    {:id "cash-flow" :name "Cash Flow"
     :keywords ["cash flow" "operating" "investing" "financing" "liquidity"]
     :description "Actual money movement"}
    {:id "working-capital" :name "Working Capital"
     :keywords ["working capital" "current assets" "current liabilities" "liquidity"]
     :description "Short-term financial health"}
    {:id "leverage" :name "Financial Leverage"
     :keywords ["leverage" "debt" "borrowed" "magnify" "amplify"]
     :description "Using debt to amplify returns"}
    {:id "margin-of-safety" :name "Margin of Safety"
     :keywords ["margin of safety" "buffer" "cushion" "conservative" "discount"]
     :description "Room for error in estimates"}
    {:id "sunk-cost" :name "Sunk Cost"
     :keywords ["sunk cost" "already spent" "irrecoverable" "past investment"]
     :description "Costs that cannot be recovered"}
    {:id "opportunity-cost" :name "Opportunity Cost"
     :keywords ["opportunity cost" "alternative" "trade-off" "foregone" "next best"]
     :description "Value of best alternative foregone"}
    {:id "present-value" :name "Present Value"
     :keywords ["present value" "discount" "future value" "time value" "dcf"]
     :description "Today's value of future money"}]
   
   :physics
   [{:id "critical-mass" :name "Critical Mass"
     :keywords ["critical mass" "threshold" "tipping point" "chain reaction"]
     :description "Point where change becomes self-sustaining"}
    {:id "equilibrium" :name "Equilibrium"
     :keywords ["equilibrium" "balance" "stable" "steady state" "homeostasis"]
     :description "State of balance"}
    {:id "inertia" :name "Inertia"
     :keywords ["inertia" "momentum" "resist change" "status quo" "continue"]
     :description "Tendency to maintain current state"}
    {:id "friction" :name "Friction"
     :keywords ["friction" "resistance" "obstacle" "impediment" "drag"]
     :description "Forces opposing motion"}
    {:id "viscosity" :name "Viscosity"
     :keywords ["viscosity" "thick" "slow" "resistant" "sticky"]
     :description "Resistance to flow"}
    {:id "velocity" :name "Velocity"
     :keywords ["velocity" "speed" "rate" "pace" "momentum"]
     :description "Speed and direction of change"}
    {:id "leverage-physics" :name "Leverage (Physics)"
     :keywords ["lever" "fulcrum" "mechanical advantage" "force multiplier"]
     :description "Small force creating large effect"}
    {:id "activation-energy" :name "Activation Energy"
     :keywords ["activation" "threshold" "barrier" "catalyst" "trigger"]
     :description "Energy needed to start reaction"}
    {:id "entropy" :name "Entropy"
     :keywords ["entropy" "disorder" "chaos" "decay" "degradation"]
     :description "Tendency toward disorder"}
    {:id "half-life" :name "Half-Life"
     :keywords ["half-life" "decay" "diminish" "fade" "depreciate"]
     :description "Time for half to decay"}
    {:id "resonance" :name "Resonance"
     :keywords ["resonance" "amplify" "frequency" "vibration" "reinforce"]
     :description "Amplification at natural frequency"}
    {:id "relativity" :name "Relativity"
     :keywords ["relative" "perspective" "frame of reference" "observer"]
     :description "Measurements depend on observer"}]
   
   :chemistry
   [{:id "catalysts" :name "Catalysts"
     :keywords ["catalyst" "accelerate" "enable" "facilitate" "trigger"]
     :description "Speeds reaction without being consumed"}
    {:id "autocatalysis" :name "Autocatalysis"
     :keywords ["autocatalysis" "self-reinforcing" "positive feedback" "accelerating"]
     :description "Product catalyzes its own production"}
    {:id "reaction-rates" :name "Reaction Rates"
     :keywords ["reaction rate" "speed" "kinetics" "fast" "slow"]
     :description "Speed of chemical change"}
    {:id "saturation" :name "Saturation"
     :keywords ["saturate" "full" "maximum" "capacity" "limit"]
     :description "Maximum capacity reached"}
    {:id "alloying" :name "Alloying"
     :keywords ["alloy" "combine" "blend" "mixture" "composite"]
     :description "Combining elements for new properties"}
    {:id "phase-transitions" :name "Phase Transitions"
     :keywords ["phase" "transition" "transform" "state change" "threshold"]
     :description "Sudden change in state"}
    {:id "bonding" :name "Chemical Bonding"
     :keywords ["bond" "connect" "link" "attach" "relationship"]
     :description "Forces holding things together"}
    {:id "concentration" :name "Concentration"
     :keywords ["concentrate" "dilute" "density" "intensity" "focus"]
     :description "Amount per unit volume"}]
   
   :engineering
   [{:id "redundancy" :name "Redundancy"
     :keywords ["redundant" "backup" "failover" "duplicate" "spare"]
     :description "Multiple systems for reliability"}
    {:id "margin-of-safety-eng" :name "Margin of Safety (Engineering)"
     :keywords ["safety factor" "overdesign" "buffer" "tolerance" "headroom"]
     :description "Building stronger than needed"}
    {:id "breakpoints" :name "Breakpoints"
     :keywords ["breakpoint" "failure point" "limit" "threshold" "stress"]
     :description "Points where systems fail"}
    {:id "feedback-loops" :name "Feedback Loops"
     :keywords ["feedback" "loop" "cycle" "reinforce" "dampen"]
     :description "Output affecting input"}
    {:id "bottlenecks" :name "Bottlenecks"
     :keywords ["bottleneck" "constraint" "limiting factor" "choke point"]
     :description "Narrowest point limiting flow"}
    {:id "scaling" :name "Scaling"
     :keywords ["scale" "grow" "expand" "size" "capacity"]
     :description "How systems change with size"}
    {:id "backup-systems" :name "Backup Systems"
     :keywords ["backup" "failsafe" "contingency" "plan b" "fallback"]
     :description "Alternative when primary fails"}
    {:id "tight-coupling" :name "Tight Coupling"
     :keywords ["coupled" "dependent" "interconnected" "linked" "integrated"]
     :description "Systems highly dependent on each other"}
    {:id "algorithms" :name "Algorithms"
     :keywords ["algorithm" "process" "procedure" "step-by-step" "method"]
     :description "Systematic problem-solving procedures"}
    {:id "network-effects" :name "Network Effects"
     :keywords ["network effect" "viral" "adoption" "users" "platform"]
     :description "Value increases with users"}
    {:id "systems-thinking" :name "Systems Thinking"
     :keywords ["system" "holistic" "interconnected" "emergent" "complexity"]
     :description "Understanding whole systems"}]
   
   ;; =========================================================================
   ;; LIFE SCIENCES
   ;; =========================================================================
   
   :biology
   [{:id "evolution" :name "Evolution"
     :keywords ["evolve" "adapt" "natural selection" "survival" "fitness"]
     :description "Change through selection pressure"}
    {:id "natural-selection" :name "Natural Selection"
     :keywords ["selection" "survive" "reproduce" "fittest" "competition"]
     :description "Survival of the fittest"}
    {:id "adaptation" :name "Adaptation"
     :keywords ["adapt" "adjust" "modify" "respond" "flexible"]
     :description "Changing to fit environment"}
    {:id "ecosystems" :name "Ecosystems"
     :keywords ["ecosystem" "environment" "habitat" "niche" "interdependent"]
     :description "Interconnected living systems"}
    {:id "niches" :name "Niches"
     :keywords ["niche" "specialized" "unique position" "role" "segment"]
     :description "Specialized role in system"}
    {:id "red-queen" :name "Red Queen Effect"
     :keywords ["red queen" "arms race" "keep up" "competitive" "evolving"]
     :description "Must keep improving to stay in place"}
    {:id "replication" :name "Replication"
     :keywords ["replicate" "copy" "reproduce" "duplicate" "spread"]
     :description "Creating copies"}
    {:id "cooperation" :name "Cooperation"
     :keywords ["cooperate" "collaborate" "mutual" "symbiosis" "alliance"]
     :description "Working together for benefit"}
    {:id "self-organization" :name "Self-Organization"
     :keywords ["self-organize" "emergent" "bottom-up" "spontaneous" "decentralized"]
     :description "Order arising without central control"}
    {:id "hierarchies" :name "Hierarchies"
     :keywords ["hierarchy" "rank" "level" "structure" "pecking order"]
     :description "Levels of organization"}
    {:id "incentives-bio" :name "Incentives (Biology)"
     :keywords ["incentive" "reward" "motivation" "drive" "benefit"]
     :description "What drives behavior"}
    {:id "survival" :name "Survival"
     :keywords ["survive" "persist" "endure" "last" "resilient"]
     :description "Continuing to exist"}]
   
   :physiology
   [{:id "homeostasis" :name "Homeostasis"
     :keywords ["homeostasis" "balance" "regulate" "stable" "equilibrium"]
     :description "Maintaining internal stability"}
    {:id "stress-response" :name "Stress Response"
     :keywords ["stress" "cortisol" "fight or flight" "anxiety" "pressure"]
     :description "Body's reaction to threats"}
    {:id "fatigue" :name "Fatigue"
     :keywords ["fatigue" "tired" "exhausted" "depleted" "burnout"]
     :description "Reduced capacity from overuse"}
    {:id "recovery" :name "Recovery"
     :keywords ["recover" "rest" "heal" "regenerate" "restore"]
     :description "Returning to normal state"}
    {:id "thresholds" :name "Thresholds"
     :keywords ["threshold" "trigger" "minimum" "activation" "sensitivity"]
     :description "Points where response begins"}
    {:id "tolerance" :name "Tolerance"
     :keywords ["tolerance" "adapt" "desensitize" "accustom" "habituate"]
     :description "Reduced response over time"}
    {:id "addiction" :name "Addiction"
     :keywords ["addiction" "dependent" "craving" "withdrawal" "compulsive"]
     :description "Compulsive need despite harm"}
    {:id "aging" :name "Aging"
     :keywords ["aging" "decline" "deteriorate" "senescence" "wear"]
     :description "Gradual decline over time"}
    {:id "circadian" :name "Circadian Rhythms"
     :keywords ["circadian" "cycle" "rhythm" "sleep" "daily"]
     :description "Daily biological cycles"}]
   
   :psychology
   [{:id "reward-punishment" :name "Reward and Punishment Superresponse"
     :keywords ["incentive" "reward" "punishment" "bonus" "commission" "penalty"]
     :description "Behavior shaped by consequences"}
    {:id "liking-loving" :name "Liking/Loving Tendency"
     :keywords ["love" "admire" "loyal" "devoted" "attached" "fond"]
     :description "Distorted judgment from affection"}
    {:id "disliking-hating" :name "Disliking/Hating Tendency"
     :keywords ["hate" "despise" "enemy" "rival" "opponent" "hostile"]
     :description "Distorted judgment from dislike"}
    {:id "doubt-avoidance" :name "Doubt-Avoidance Tendency"
     :keywords ["certain" "decisive" "quick decision" "eliminate doubt"]
     :description "Rush to eliminate uncertainty"}
    {:id "inconsistency-avoidance" :name "Inconsistency-Avoidance Tendency"
     :keywords ["consistent" "habit" "routine" "tradition" "status quo"]
     :description "Resist changing beliefs/habits"}
    {:id "curiosity" :name "Curiosity Tendency"
     :keywords ["curious" "explore" "discover" "learn" "investigate"]
     :description "Drive to understand"}
    {:id "kantian-fairness" :name "Kantian Fairness Tendency"
     :keywords ["fair" "equal" "justice" "reciprocal" "deserve"]
     :description "Expectation of fair treatment"}
    {:id "envy-jealousy" :name "Envy/Jealousy Tendency"
     :keywords ["envy" "jealous" "covet" "resentment" "compare"]
     :description "Pain from others' success"}
    {:id "reciprocation" :name "Reciprocation Tendency"
     :keywords ["reciprocate" "return favor" "payback" "tit for tat"]
     :description "Repay in kind"}
    {:id "influence-association" :name "Influence-from-Mere-Association"
     :keywords ["associate" "connection" "linked" "related" "brand"]
     :description "Judgment by association"}
    {:id "pain-avoidance" :name "Simple Pain-Avoiding Psychological Denial"
     :keywords ["deny" "ignore" "avoid" "painful" "uncomfortable truth"]
     :description "Deny painful realities"}
    {:id "excessive-self-regard" :name "Excessive Self-Regard Tendency"
     :keywords ["overconfident" "ego" "self-esteem" "pride" "narcissist"]
     :description "Overvalue self and possessions"}
    {:id "overoptimism" :name "Overoptimism Tendency"
     :keywords ["optimistic" "hopeful" "best case" "rosy" "upside"]
     :description "Expect better than warranted"}
    {:id "deprival-superreaction" :name "Deprival-Superreaction Tendency"
     :keywords ["loss" "take away" "lose" "deprive" "losing"]
     :description "Overreact to losses"}
    {:id "social-proof" :name "Social-Proof Tendency"
     :keywords ["everyone" "popular" "trend" "crowd" "herd" "consensus"]
     :description "Follow the crowd"}
    {:id "contrast-misreaction" :name "Contrast-Misreaction Tendency"
     :keywords ["compare" "relative" "contrast" "anchor" "reference"]
     :description "Misjudge by comparison"}
    {:id "stress-influence" :name "Stress-Influence Tendency"
     :keywords ["stress" "pressure" "deadline" "urgent" "crisis"]
     :description "Stress changes behavior"}
    {:id "availability-misweighing" :name "Availability-Misweighing Tendency"
     :keywords ["recent" "memorable" "vivid" "salient" "top of mind"]
     :description "Overweight easily recalled"}
    {:id "use-it-or-lose-it" :name "Use-It-or-Lose-It Tendency"
     :keywords ["practice" "skill" "atrophy" "rusty" "maintain"]
     :description "Skills decay without use"}
    {:id "drug-misinfluence" :name "Drug-Misinfluence Tendency"
     :keywords ["drug" "alcohol" "substance" "addiction" "intoxicated"]
     :description "Substances impair judgment"}
    {:id "senescence-misinfluence" :name "Senescence-Misinfluence Tendency"
     :keywords ["age" "elderly" "decline" "cognitive" "memory"]
     :description "Age affects judgment"}
    {:id "authority-misinfluence" :name "Authority-Misinfluence Tendency"
     :keywords ["authority" "expert" "leader" "boss" "credentials"]
     :description "Defer excessively to authority"}
    {:id "twaddle" :name "Twaddle Tendency"
     :keywords ["nonsense" "babble" "filler" "meaningless" "verbose"]
     :description "Produce meaningless talk"}
    {:id "reason-respecting" :name "Reason-Respecting Tendency"
     :keywords ["because" "reason" "why" "explain" "justify"]
     :description "Comply when given reasons"}
    {:id "lollapalooza-tendency" :name "Lollapalooza Tendency"
     :keywords ["combine" "multiple" "converge" "compound" "synergy"]
     :description "Multiple tendencies combining"}]
   
   :economics
   [{:id "supply-demand" :name "Supply and Demand"
     :keywords ["supply" "demand" "price" "market" "equilibrium"]
     :description "Price determined by supply and demand"}
    {:id "scarcity" :name "Scarcity"
     :keywords ["scarce" "limited" "rare" "shortage" "finite"]
     :description "Limited resources"}
    {:id "comparative-advantage" :name "Comparative Advantage"
     :keywords ["comparative advantage" "specialize" "trade" "efficient"]
     :description "Focus on relative strengths"}
    {:id "creative-destruction" :name "Creative Destruction"
     :keywords ["creative destruction" "disrupt" "obsolete" "innovate" "replace"]
     :description "New replaces old"}
    {:id "diminishing-returns" :name "Diminishing Returns"
     :keywords ["diminishing" "returns" "marginal" "decreasing" "plateau"]
     :description "Each additional unit adds less"}
    {:id "economies-of-scale" :name "Economies of Scale"
     :keywords ["economies of scale" "volume" "efficiency" "cost per unit"]
     :description "Cost decreases with volume"}
    {:id "utility" :name "Utility"
     :keywords ["utility" "satisfaction" "value" "benefit" "usefulness"]
     :description "Satisfaction from consumption"}
    {:id "incentives-econ" :name "Incentives (Economics)"
     :keywords ["incentive" "motivation" "reward" "penalty" "behavior"]
     :description "What motivates economic behavior"}
    {:id "specialization" :name "Specialization"
     :keywords ["specialize" "focus" "expert" "niche" "division of labor"]
     :description "Focus on specific tasks"}
    {:id "tradeoffs" :name "Tradeoffs"
     :keywords ["tradeoff" "sacrifice" "choice" "compromise" "balance"]
     :description "Giving up one thing for another"}
    {:id "tragedy-of-commons" :name "Tragedy of the Commons"
     :keywords ["commons" "shared" "deplete" "overuse" "public"]
     :description "Shared resources get overused"}
    {:id "externalities" :name "Externalities"
     :keywords ["externality" "spillover" "side effect" "unintended" "third party"]
     :description "Effects on uninvolved parties"}
    {:id "moral-hazard" :name "Moral Hazard"
     :keywords ["moral hazard" "risk" "insured" "reckless" "protected"]
     :description "More risk when protected"}
    {:id "adverse-selection" :name "Adverse Selection"
     :keywords ["adverse selection" "asymmetric" "information" "hidden"]
     :description "Bad risks more likely to participate"}
    {:id "game-theory" :name "Game Theory"
     :keywords ["game theory" "strategy" "nash" "prisoner" "payoff"]
     :description "Strategic interaction analysis"}
    {:id "principal-agent" :name "Principal-Agent Problem"
     :keywords ["principal" "agent" "misaligned" "incentive" "conflict"]
     :description "Agents may not act in principal's interest"}
    {:id "asymmetric-information" :name "Asymmetric Information"
     :keywords ["asymmetric" "information" "hidden" "insider" "advantage"]
     :description "Unequal information access"}
    {:id "market-failure" :name "Market Failure"
     :keywords ["market failure" "inefficient" "monopoly" "externality"]
     :description "Markets not producing optimal outcomes"}]
   
   ;; =========================================================================
   ;; APPLIED KNOWLEDGE
   ;; =========================================================================
   
   :moats
   [{:id "brand" :name "Brand"
     :keywords ["brand" "reputation" "recognition" "trust" "loyalty"]
     :description "Customer recognition and trust"}
    {:id "switching-costs" :name "Switching Costs"
     :keywords ["switching cost" "lock-in" "sticky" "retention" "embedded"]
     :description "Cost to change providers"}
    {:id "network-effects-moat" :name "Network Effects (Moat)"
     :keywords ["network effect" "platform" "users" "viral" "ecosystem"]
     :description "Value increases with users"}
    {:id "cost-advantages" :name "Cost Advantages"
     :keywords ["cost advantage" "low cost" "efficient" "scale" "cheap"]
     :description "Ability to produce cheaper"}
    {:id "intangible-assets" :name "Intangible Assets"
     :keywords ["patent" "license" "intellectual property" "trademark" "copyright"]
     :description "Protected intellectual property"}
    {:id "efficient-scale" :name "Efficient Scale"
     :keywords ["efficient scale" "natural monopoly" "limited market" "niche"]
     :description "Market only supports few competitors"}
    {:id "regulatory-capture" :name "Regulatory Capture"
     :keywords ["regulation" "license" "permit" "barrier" "compliance"]
     :description "Regulations favor incumbents"}
    {:id "distribution" :name "Distribution"
     :keywords ["distribution" "channel" "access" "reach" "network"]
     :description "Access to customers"}
    {:id "customer-captivity" :name "Customer Captivity"
     :keywords ["captive" "habit" "routine" "embedded" "entrenched"]
     :description "Customers locked in by habit"}
    {:id "toll-bridge" :name "Toll Bridge"
     :keywords ["toll" "gatekeeper" "must use" "unavoidable" "essential"]
     :description "Must pass through to access market"}
    {:id "secret" :name "Secret/Know-How"
     :keywords ["secret" "proprietary" "know-how" "expertise" "trade secret"]
     :description "Unique knowledge or process"}
    {:id "culture" :name "Culture"
     :keywords ["culture" "values" "way we do things" "tradition" "mindset"]
     :description "Organizational culture as advantage"}
    {:id "first-mover" :name "First Mover Advantage"
     :keywords ["first mover" "pioneer" "early" "establish" "lead"]
     :description "Advantage from being first"}
    {:id "ecosystem" :name "Ecosystem"
     :keywords ["ecosystem" "platform" "partners" "complementary" "integrated"]
     :description "Network of complementary products"}
    {:id "data-advantage" :name "Data Advantage"
     :keywords ["data" "information" "insights" "analytics" "learning"]
     :description "Competitive advantage from data"}]
   
   :organizational
   [{:id "culture-org" :name "Organizational Culture"
     :keywords ["culture" "values" "norms" "behavior" "environment"]
     :description "Shared values and behaviors"}
    {:id "principal-agent-org" :name "Principal-Agent (Organizational)"
     :keywords ["principal" "agent" "alignment" "incentive" "delegation"]
     :description "Aligning employee and owner interests"}
    {:id "bureaucracy" :name "Bureaucracy"
     :keywords ["bureaucracy" "red tape" "process" "approval" "hierarchy"]
     :description "Formal procedures and hierarchy"}
    {:id "span-of-control" :name "Span of Control"
     :keywords ["span" "control" "direct reports" "management" "layers"]
     :description "Number of direct reports"}
    {:id "decentralization" :name "Decentralization"
     :keywords ["decentralize" "autonomous" "distributed" "local" "empower"]
     :description "Distributing decision-making"}
    {:id "trust" :name "Trust"
     :keywords ["trust" "reliable" "dependable" "honest" "integrity"]
     :description "Confidence in others"}
    {:id "incentive-alignment" :name "Incentive Alignment"
     :keywords ["aligned" "incentive" "skin in game" "ownership" "stake"]
     :description "Matching incentives to goals"}
    {:id "communication" :name "Communication"
     :keywords ["communicate" "information" "transparent" "share" "clarity"]
     :description "Effective information flow"}
    {:id "coordination" :name "Coordination"
     :keywords ["coordinate" "synchronize" "align" "collaborate" "integrate"]
     :description "Working together effectively"}
    {:id "specialization-org" :name "Specialization (Organizational)"
     :keywords ["specialize" "division" "expert" "focus" "dedicated"]
     :description "Dividing work by expertise"}
    {:id "succession" :name "Succession"
     :keywords ["succession" "transition" "replacement" "next generation" "continuity"]
     :description "Leadership transition planning"}
    {:id "institutional-memory" :name "Institutional Memory"
     :keywords ["institutional memory" "knowledge" "history" "experience" "lessons"]
     :description "Organizational knowledge retention"}]
   
   :thinking-tools
   [{:id "inversion" :name "Inversion"
     :keywords ["invert" "reverse" "opposite" "avoid" "what not to do"]
     :description "Think backwards from failure"}
    {:id "first-principles" :name "First Principles"
     :keywords ["first principles" "fundamental" "basic" "ground up" "from scratch"]
     :description "Reason from fundamentals"}
    {:id "second-order" :name "Second-Order Thinking"
     :keywords ["second order" "consequence" "then what" "ripple" "downstream"]
     :description "Consider consequences of consequences"}
    {:id "circle-of-competence" :name "Circle of Competence"
     :keywords ["competence" "expertise" "know" "understand" "capable"]
     :description "Stay within what you know"}
    {:id "occams-razor" :name "Occam's Razor"
     :keywords ["simple" "simplest" "parsimony" "elegant" "minimal"]
     :description "Prefer simpler explanations"}
    {:id "hanlons-razor" :name "Hanlon's Razor"
     :keywords ["malice" "stupidity" "incompetence" "mistake" "error"]
     :description "Don't assume malice when stupidity explains"}
    {:id "map-territory" :name "Map is Not the Territory"
     :keywords ["map" "model" "representation" "reality" "abstraction"]
     :description "Models are not reality"}
    {:id "thought-experiment" :name "Thought Experiment"
     :keywords ["thought experiment" "imagine" "hypothetical" "what if" "scenario"]
     :description "Mental simulation"}
    {:id "probabilistic-thinking" :name "Probabilistic Thinking"
     :keywords ["probability" "likely" "odds" "chance" "uncertain"]
     :description "Think in probabilities not certainties"}
    {:id "bayesian-updating" :name "Bayesian Updating"
     :keywords ["update" "evidence" "prior" "belief" "revise"]
     :description "Update beliefs with new evidence"}
    {:id "falsification" :name "Falsification"
     :keywords ["falsify" "disprove" "test" "refute" "contradict"]
     :description "Try to disprove hypotheses"}
    {:id "counterfactual" :name "Counterfactual Thinking"
     :keywords ["counterfactual" "what if" "alternative" "different" "scenario"]
     :description "Consider alternative outcomes"}
    {:id "pre-mortem" :name "Pre-Mortem"
     :keywords ["pre-mortem" "failure" "anticipate" "prevent" "foresee"]
     :description "Imagine future failure to prevent it"}
    {:id "devils-advocate" :name "Devil's Advocate"
     :keywords ["devil's advocate" "challenge" "argue against" "critique" "question"]
     :description "Argue the opposite position"}
    {:id "steelmanning" :name "Steelmanning"
     :keywords ["steelman" "strongest" "best argument" "charitable" "fair"]
     :description "Address strongest opposing argument"}
    {:id "mr-market" :name "Mr. Market"
     :keywords ["mr market" "emotional" "irrational" "mood" "sentiment"]
     :description "Market as emotional partner"}
    {:id "margin-of-safety-thinking" :name "Margin of Safety (Thinking)"
     :keywords ["margin" "safety" "buffer" "conservative" "cushion"]
     :description "Build in room for error"}
    {:id "via-negativa" :name "Via Negativa"
     :keywords ["via negativa" "subtract" "remove" "eliminate" "less"]
     :description "Improve by removing"}
    {:id "checklists" :name "Checklists"
     :keywords ["checklist" "list" "verify" "systematic" "ensure"]
     :description "Systematic verification"}
    {:id "journaling" :name "Decision Journaling"
     :keywords ["journal" "record" "document" "track" "review"]
     :description "Record decisions for learning"}]})

;; =============================================================================
;; CLASSIFICATION FUNCTIONS
;; =============================================================================

(defn detect-model
  "Detect if a mental model applies to text"
  [text model]
  (let [text-lower (str/lower-case text)
        matches (filter #(str/includes? text-lower %) (:keywords model))
        score (if (empty? (:keywords model))
                0
                (min 1.0 (* 2 (/ (count matches) (count (:keywords model))))))]
    (when (> score 0.1)
      {:id (:id model)
       :name (:name model)
       :category nil  ; Will be set by caller
       :score score
       :matches matches
       :description (:description model)})))

(defn classify-text
  "Classify text against all 167 mental models"
  [text]
  (let [results (for [[category models] mental-models
                      model models
                      :let [result (detect-model text model)]
                      :when result]
                  (assoc result :category (name category)))]
    {:detected-models (sort-by :score > results)
     :total-detected (count results)
     :by-category (group-by :category results)}))

(defn detect-lollapalooza
  "Detect Lollapalooza effect (3+ models converging >70%)"
  [classification-result & {:keys [threshold min-models]
                            :or {threshold 0.7 min-models 3}}]
  (let [high-scoring (filter #(>= (:score %) threshold) 
                             (:detected-models classification-result))]
    {:lollapalooza? (>= (count high-scoring) min-models)
     :converging-models high-scoring
     :count (count high-scoring)}))

(defn get-top-models
  "Get top N detected models"
  [classification-result n]
  (take n (:detected-models classification-result)))

(defn get-category-summary
  "Get summary by category"
  [classification-result]
  (into {} 
        (for [[cat models] (:by-category classification-result)]
          [cat {:count (count models)
                :avg-score (if (empty? models) 
                             0 
                             (/ (reduce + (map :score models)) (count models)))
                :top-model (first (sort-by :score > models))}])))
