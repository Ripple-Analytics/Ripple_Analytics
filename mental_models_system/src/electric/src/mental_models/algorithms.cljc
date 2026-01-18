(ns mental-models.algorithms
  "Mental Models Algorithms - Electric Clojure
   
   Individual algorithms and LM Studio prompts for all registered mental models.
   Uses a single LM Studio model with different prompts to save memory.
   Model count is dynamically computed from the models registry.
   
   Each model has:
   1. Pattern detection algorithm (rule-based)
   2. LM Studio prompt template for deep analysis
   3. Failure mode detection logic
   4. Lollapalooza interaction detection
   
   This is a .cljc file - runs on both client and server!"
  #?(:clj (:require [clojure.string :as str]
                    [mental-models.models :as models])
     :cljs (:require [clojure.string :as str]
                     [mental-models.models :as models])))

;; ============================================
;; LM Studio Configuration (Single Model)
;; ============================================

(def lm-studio-config
  "Single LM Studio model configuration to save memory.
   All mental models use the same underlying LLM with different prompts."
  {:base-url "http://localhost:1234"
   :model "local-model"
   :timeout 60000
   :max-tokens 2000
   :temperature 0.7})

;; ============================================
;; Prompt Templates for Each Mental Model
;; ============================================

(def model-prompts
  "LM Studio prompt templates for each mental model.
   Each prompt is designed to analyze text through the lens of that specific model."
  {
   ;; Decision Making Models
   "circle-of-competence"
   {:system "You are an expert analyst specializing in Circle of Competence analysis. Your role is to identify whether decisions fall within or outside someone's area of expertise."
    :user-template "Analyze the following text for Circle of Competence issues:

TEXT: {{text}}

Identify:
1. What competencies are being claimed or assumed?
2. Is the person operating within their circle of competence?
3. What signals indicate overconfidence or boundary blindness?
4. What areas might be outside their circle?
5. Recommendations for staying within competence boundaries.

Provide specific quotes and evidence from the text."
    :detection-keywords ["expertise" "experience" "knowledge" "understand" "know" "competent" "qualified" "specialist"]
    :failure-signals ["I'm sure" "definitely" "no doubt" "obviously" "everyone knows" "it's simple"]}

   "margin-of-safety"
   {:system "You are an expert analyst specializing in Margin of Safety analysis. Your role is to identify whether adequate buffers exist for uncertainty."
    :user-template "Analyze the following text for Margin of Safety issues:

TEXT: {{text}}

Identify:
1. What assumptions are being made?
2. Is there adequate buffer for uncertainty?
3. What could go wrong that isn't accounted for?
4. Are estimates too precise given the uncertainty?
5. Recommendations for building in safety margins.

Provide specific quotes and evidence from the text."
    :detection-keywords ["estimate" "projection" "forecast" "expect" "assume" "buffer" "cushion" "reserve"]
    :failure-signals ["exactly" "precisely" "guaranteed" "certain" "no risk" "best case"]}

   "second-order-thinking"
   {:system "You are an expert analyst specializing in Second-Order Thinking. Your role is to identify consequences of consequences."
    :user-template "Analyze the following text for Second-Order Thinking:

TEXT: {{text}}

Identify:
1. What first-order effects are mentioned?
2. What second-order effects might occur?
3. What third-order effects could emerge?
4. Are there feedback loops or cascading effects?
5. What time horizons should be considered?

Provide specific quotes and trace the causal chains."
    :detection-keywords ["consequence" "result" "effect" "impact" "lead to" "cause" "because" "therefore"]
    :failure-signals ["simple" "straightforward" "just" "only" "won't affect"]}

   "inversion"
   {:system "You are an expert analyst specializing in Inversion thinking. Your role is to identify what could go wrong and how to avoid failure."
    :user-template "Analyze the following text using Inversion:

TEXT: {{text}}

Identify:
1. What is the goal or desired outcome?
2. How could this fail? List all failure modes.
3. What would guarantee failure?
4. What should be avoided at all costs?
5. How can these failure modes be prevented?

Provide specific quotes and invert the problem."
    :detection-keywords ["goal" "objective" "success" "achieve" "want" "need" "plan" "strategy"]
    :failure-signals ["can't fail" "foolproof" "guaranteed success" "no way to lose"]}

   "opportunity-cost"
   {:system "You are an expert analyst specializing in Opportunity Cost analysis. Your role is to identify what is being given up with each choice."
    :user-template "Analyze the following text for Opportunity Costs:

TEXT: {{text}}

Identify:
1. What choice or decision is being made?
2. What alternatives are being foregone?
3. What is the value of the next best alternative?
4. Is time being properly valued?
5. Are there hidden opportunity costs?

Provide specific quotes and quantify costs where possible."
    :detection-keywords ["choose" "decide" "option" "alternative" "instead" "rather" "trade-off" "sacrifice"]
    :failure-signals ["only option" "no choice" "must" "have to" "no alternative"]}

   ;; Psychology Models
   "incentives"
   {:system "You are an expert analyst specializing in Incentive analysis. Your role is to identify what motivates behavior."
    :user-template "Analyze the following text for Incentive structures:

TEXT: {{text}}

Identify:
1. What incentives are at play?
2. Who benefits from what outcomes?
3. Are incentives aligned with stated goals?
4. What perverse incentives might exist?
5. How might incentives be driving behavior?

Follow the money and identify hidden motivations."
    :detection-keywords ["reward" "benefit" "gain" "profit" "bonus" "commission" "payment" "compensation"]
    :failure-signals ["altruistic" "selfless" "no ulterior motive" "purely" "just wants to help"]}

   "social-proof"
   {:system "You are an expert analyst specializing in Social Proof detection. Your role is to identify herd behavior and conformity."
    :user-template "Analyze the following text for Social Proof influence:

TEXT: {{text}}

Identify:
1. Is behavior being justified by what others do?
2. What reference groups are being cited?
3. Is there evidence of herd following?
4. Could this be manufactured social proof?
5. Is independent analysis being done?

Identify specific instances of conformity or herd behavior."
    :detection-keywords ["everyone" "popular" "trending" "most people" "consensus" "majority" "common" "normal"]
    :failure-signals ["everyone does it" "it's normal" "that's how it's done" "industry standard"]}

   "commitment-consistency"
   {:system "You are an expert analyst specializing in Commitment and Consistency bias. Your role is to identify escalation of commitment."
    :user-template "Analyze the following text for Commitment-Consistency bias:

TEXT: {{text}}

Identify:
1. What prior commitments have been made?
2. Is there evidence of escalation of commitment?
3. Are sunk costs influencing decisions?
4. Is identity tied to positions?
5. Would a fresh observer make the same choice?

Identify specific instances of consistency bias."
    :detection-keywords ["committed" "invested" "already" "so far" "can't stop now" "too late" "identity"]
    :failure-signals ["we've come too far" "can't back out now" "I've always" "that's who I am"]}

   "availability-heuristic"
   {:system "You are an expert analyst specializing in Availability Heuristic detection. Your role is to identify when recent or vivid events are overweighted."
    :user-template "Analyze the following text for Availability Heuristic bias:

TEXT: {{text}}

Identify:
1. Are recent events being overweighted?
2. Are vivid/dramatic events influencing judgment?
3. Is media coverage affecting perception?
4. What base rates are being ignored?
5. What statistical evidence should be considered?

Identify specific instances of availability bias."
    :detection-keywords ["recently" "just happened" "saw on news" "heard about" "remember when" "dramatic"]
    :failure-signals ["it just happened" "I saw it on TV" "everyone's talking about" "viral"]}

   "loss-aversion"
   {:system "You are an expert analyst specializing in Loss Aversion detection. Your role is to identify when fear of loss is driving decisions."
    :user-template "Analyze the following text for Loss Aversion:

TEXT: {{text}}

Identify:
1. Is fear of loss driving the decision?
2. Are losses being weighted more than equivalent gains?
3. Is there excessive risk aversion?
4. Are losing positions being held too long?
5. What would an expected value analysis show?

Identify specific instances of loss aversion."
    :detection-keywords ["lose" "risk" "protect" "safe" "secure" "preserve" "defend" "avoid"]
    :failure-signals ["can't afford to lose" "too risky" "play it safe" "protect what we have"]}

   "confirmation-bias"
   {:system "You are an expert analyst specializing in Confirmation Bias detection. Your role is to identify selective evidence gathering."
    :user-template "Analyze the following text for Confirmation Bias:

TEXT: {{text}}

Identify:
1. Is evidence being selectively gathered?
2. Are contrary views being dismissed?
3. Is there a pre-existing belief being defended?
4. What disconfirming evidence is being ignored?
5. How would a skeptic view this?

Identify specific instances of confirmation bias."
    :detection-keywords ["proves" "confirms" "supports" "evidence" "shows" "demonstrates" "validates"]
    :failure-signals ["this proves" "see, I was right" "as I expected" "obviously"]}

   "hindsight-bias"
   {:system "You are an expert analyst specializing in Hindsight Bias detection. Your role is to identify 'I knew it all along' thinking."
    :user-template "Analyze the following text for Hindsight Bias:

TEXT: {{text}}

Identify:
1. Is the outcome being treated as predictable after the fact?
2. Was this actually predicted beforehand?
3. What uncertainty existed at the time?
4. Are people claiming they 'knew it all along'?
5. What would have been reasonable to expect?

Identify specific instances of hindsight bias."
    :detection-keywords ["obvious" "predictable" "should have known" "clearly" "inevitable" "bound to happen"]
    :failure-signals ["I knew it" "it was obvious" "anyone could see" "should have seen it coming"]}

   "anchoring"
   {:system "You are an expert analyst specializing in Anchoring Bias detection. Your role is to identify when initial numbers unduly influence judgment."
    :user-template "Analyze the following text for Anchoring Bias:

TEXT: {{text}}

Identify:
1. What initial numbers or values are mentioned?
2. Are subsequent estimates anchored to these?
3. Is the anchor relevant or arbitrary?
4. How might the anchor be influencing judgment?
5. What would an independent estimate look like?

Identify specific instances of anchoring."
    :detection-keywords ["originally" "started at" "initial" "first" "asking price" "list price" "MSRP"]
    :failure-signals ["compared to the original" "down from" "up from" "relative to"]}

   "dunning-kruger"
   {:system "You are an expert analyst specializing in Dunning-Kruger Effect detection. Your role is to identify overconfidence from incompetence."
    :user-template "Analyze the following text for Dunning-Kruger Effect:

TEXT: {{text}}

Identify:
1. Is there overconfidence without demonstrated competence?
2. Are limitations being acknowledged?
3. Is there awareness of what they don't know?
4. Are experts being dismissed?
5. What evidence of actual competence exists?

Identify specific instances of Dunning-Kruger."
    :detection-keywords ["easy" "simple" "obvious" "anyone can" "don't need" "experts are wrong"]
    :failure-signals ["it's not that hard" "I figured it out" "experts overcomplicate" "common sense"]}

   "status-quo-bias"
   {:system "You are an expert analyst specializing in Status Quo Bias detection. Your role is to identify preference for the current state."
    :user-template "Analyze the following text for Status Quo Bias:

TEXT: {{text}}

Identify:
1. Is the current state being preferred without justification?
2. Are change costs being overweighted?
3. Is inertia driving the decision?
4. What would a fresh start analysis show?
5. Are there valid reasons for the status quo?

Identify specific instances of status quo bias."
    :detection-keywords ["always" "tradition" "how we do things" "never changed" "works fine" "if it ain't broke"]
    :failure-signals ["we've always done it this way" "why change" "it's working" "don't fix what isn't broken"]}

   "narrative-fallacy"
   {:system "You are an expert analyst specializing in Narrative Fallacy detection. Your role is to identify false stories imposed on random events."
    :user-template "Analyze the following text for Narrative Fallacy:

TEXT: {{text}}

Identify:
1. Is a coherent story being imposed on random events?
2. Is causation being inferred from correlation?
3. Are coincidences being given meaning?
4. What role does randomness play?
5. What alternative narratives could explain the same facts?

Identify specific instances of narrative fallacy."
    :detection-keywords ["because" "therefore" "led to" "caused" "resulted in" "story" "journey"]
    :failure-signals ["it was meant to be" "everything happens for a reason" "the story of"]}

   ;; Systems Thinking Models
   "feedback-loops"
   {:system "You are an expert analyst specializing in Feedback Loop analysis. Your role is to identify reinforcing and balancing loops."
    :user-template "Analyze the following text for Feedback Loops:

TEXT: {{text}}

Identify:
1. What reinforcing (positive) feedback loops exist?
2. What balancing (negative) feedback loops exist?
3. Are there delays in the feedback?
4. What could cause runaway effects?
5. How can loops be managed or broken?

Map the feedback structures in the system."
    :detection-keywords ["cycle" "loop" "reinforce" "amplify" "dampen" "stabilize" "snowball" "spiral"]
    :failure-signals ["keeps growing" "out of control" "can't stop" "accelerating"]}

   "emergence"
   {:system "You are an expert analyst specializing in Emergence analysis. Your role is to identify properties that arise from system interactions."
    :user-template "Analyze the following text for Emergence:

TEXT: {{text}}

Identify:
1. What emergent properties might arise?
2. How do individual parts interact?
3. What collective behaviors could emerge?
4. Are there phase transitions or tipping points?
5. What can't be predicted from parts alone?

Identify emergent phenomena in the system."
    :detection-keywords ["emerge" "arise" "collective" "system" "interaction" "complex" "unpredictable"]
    :failure-signals ["sum of parts" "predictable" "linear" "simple addition"]}

   "leverage-points"
   {:system "You are an expert analyst specializing in Leverage Points analysis. Your role is to identify high-impact intervention points."
    :user-template "Analyze the following text for Leverage Points:

TEXT: {{text}}

Identify:
1. What are the key leverage points in this system?
2. Where would small changes have large effects?
3. What are the system's goals and paradigms?
4. What feedback structures could be changed?
5. What interventions would be most effective?

Rank leverage points by potential impact."
    :detection-keywords ["key" "critical" "pivot" "turning point" "catalyst" "trigger" "unlock"]
    :failure-signals ["everything is important" "no priorities" "equal weight"]}

   "nonlinearity"
   {:system "You are an expert analyst specializing in Nonlinearity analysis. Your role is to identify disproportionate cause-effect relationships."
    :user-template "Analyze the following text for Nonlinearity:

TEXT: {{text}}

Identify:
1. Are effects proportional to causes?
2. Where might small changes have large effects?
3. Are there thresholds or tipping points?
4. What exponential or power-law relationships exist?
5. Where does linear thinking fail?

Identify nonlinear dynamics in the system."
    :detection-keywords ["exponential" "threshold" "tipping point" "critical mass" "hockey stick" "inflection"]
    :failure-signals ["proportional" "linear" "gradual" "steady" "predictable growth"]}

   ;; Economics Models
   "supply-demand"
   {:system "You are an expert analyst specializing in Supply and Demand analysis. Your role is to identify market dynamics."
    :user-template "Analyze the following text for Supply and Demand dynamics:

TEXT: {{text}}

Identify:
1. What are the supply factors?
2. What are the demand factors?
3. How is price being determined?
4. What could shift supply or demand?
5. Are there market inefficiencies?

Analyze the market equilibrium."
    :detection-keywords ["price" "supply" "demand" "market" "shortage" "surplus" "equilibrium" "scarcity"]
    :failure-signals ["price doesn't matter" "unlimited supply" "infinite demand"]}

   "comparative-advantage"
   {:system "You are an expert analyst specializing in Comparative Advantage analysis. Your role is to identify specialization opportunities."
    :user-template "Analyze the following text for Comparative Advantage:

TEXT: {{text}}

Identify:
1. What are the relative strengths?
2. Where is there comparative advantage?
3. What should be specialized in?
4. What should be traded or outsourced?
5. Are opportunity costs being considered?

Identify specialization opportunities."
    :detection-keywords ["specialize" "focus" "strength" "advantage" "outsource" "trade" "efficient"]
    :failure-signals ["do everything" "self-sufficient" "no need to trade"]}

   "diminishing-returns"
   {:system "You are an expert analyst specializing in Diminishing Returns analysis. Your role is to identify when additional inputs yield less output."
    :user-template "Analyze the following text for Diminishing Returns:

TEXT: {{text}}

Identify:
1. What inputs are being added?
2. Are returns diminishing?
3. Where is the point of diminishing returns?
4. Is there over-investment in any area?
5. Where should resources be reallocated?

Identify diminishing returns dynamics."
    :detection-keywords ["more" "additional" "increase" "add" "invest" "returns" "yield" "output"]
    :failure-signals ["more is always better" "keep adding" "no limit"]}

   "network-effects"
   {:system "You are an expert analyst specializing in Network Effects analysis. Your role is to identify value that increases with users."
    :user-template "Analyze the following text for Network Effects:

TEXT: {{text}}

Identify:
1. Does value increase with more users?
2. What type of network effect exists?
3. Is there a critical mass threshold?
4. What are the winner-take-all dynamics?
5. How defensible is the network?

Analyze the network effect dynamics."
    :detection-keywords ["network" "users" "platform" "ecosystem" "viral" "adoption" "critical mass"]
    :failure-signals ["standalone value" "doesn't need users" "no network"]}

   "principal-agent"
   {:system "You are an expert analyst specializing in Principal-Agent analysis. Your role is to identify conflicts between owners and managers."
    :user-template "Analyze the following text for Principal-Agent problems:

TEXT: {{text}}

Identify:
1. Who is the principal (owner)?
2. Who is the agent (manager)?
3. What conflicts of interest exist?
4. How is the agent incentivized?
5. What monitoring or alignment mechanisms exist?

Identify agency problems and solutions."
    :detection-keywords ["management" "owner" "shareholder" "employee" "delegate" "represent" "behalf"]
    :failure-signals ["aligned interests" "no conflict" "same goals"]}

   ;; Strategy Models
   "competitive-advantage"
   {:system "You are an expert analyst specializing in Competitive Advantage analysis. Your role is to identify sustainable advantages."
    :user-template "Analyze the following text for Competitive Advantage:

TEXT: {{text}}

Identify:
1. What competitive advantages are claimed?
2. Are they sustainable or temporary?
3. What is the source of advantage?
4. How defensible is the moat?
5. What could erode the advantage?

Analyze the competitive position."
    :detection-keywords ["advantage" "moat" "differentiation" "unique" "proprietary" "barrier" "defensible"]
    :failure-signals ["no competition" "can't be copied" "permanent advantage"]}

   "first-mover"
   {:system "You are an expert analyst specializing in First Mover analysis. Your role is to identify timing advantages and disadvantages."
    :user-template "Analyze the following text for First Mover dynamics:

TEXT: {{text}}

Identify:
1. Is there a first mover advantage?
2. What are the first mover disadvantages?
3. Is fast following a better strategy?
4. What switching costs exist?
5. How important is timing?

Analyze the timing strategy."
    :detection-keywords ["first" "pioneer" "early" "leader" "follower" "timing" "market entry"]
    :failure-signals ["first always wins" "too late" "missed the boat"]}

   "game-theory"
   {:system "You are an expert analyst specializing in Game Theory analysis. Your role is to identify strategic interactions."
    :user-template "Analyze the following text for Game Theory dynamics:

TEXT: {{text}}

Identify:
1. Who are the players?
2. What are their strategies?
3. What are the payoffs?
4. Is this a zero-sum or positive-sum game?
5. What is the Nash equilibrium?

Analyze the strategic interaction."
    :detection-keywords ["strategy" "opponent" "compete" "cooperate" "payoff" "move" "counter"]
    :failure-signals ["they won't respond" "no reaction" "independent"]}

   "red-queen"
   {:system "You are an expert analyst specializing in Red Queen Effect analysis. Your role is to identify competitive treadmills."
    :user-template "Analyze the following text for Red Queen Effect:

TEXT: {{text}}

Identify:
1. Is there a competitive treadmill?
2. Is running just to stay in place?
3. What is driving the arms race?
4. Can the treadmill be escaped?
5. What would happen if you stopped running?

Analyze the competitive dynamics."
    :detection-keywords ["keep up" "race" "competition" "evolve" "adapt" "survive" "treadmill"]
    :failure-signals ["stable" "no need to change" "we've arrived"]}

   ;; Science & Math Models
   "bayes-theorem"
   {:system "You are an expert analyst specializing in Bayesian analysis. Your role is to identify how evidence should update beliefs."
    :user-template "Analyze the following text for Bayesian reasoning:

TEXT: {{text}}

Identify:
1. What is the prior probability?
2. What new evidence is presented?
3. How should the posterior be updated?
4. Is base rate neglect occurring?
5. What would proper Bayesian updating show?

Apply Bayesian reasoning to the claims."
    :detection-keywords ["probability" "likely" "evidence" "update" "prior" "base rate" "odds"]
    :failure-signals ["proves" "definitely" "100%" "impossible"]}

   "regression-to-mean"
   {:system "You are an expert analyst specializing in Regression to Mean analysis. Your role is to identify when extreme values will normalize."
    :user-template "Analyze the following text for Regression to Mean:

TEXT: {{text}}

Identify:
1. Are extreme values being observed?
2. Is regression to mean likely?
3. Is performance being attributed to skill vs luck?
4. What is the expected reversion?
5. Are trends being extrapolated incorrectly?

Identify regression to mean dynamics."
    :detection-keywords ["extreme" "outlier" "exceptional" "best ever" "worst ever" "streak" "trend"]
    :failure-signals ["new normal" "paradigm shift" "this time is different"]}

   "power-laws"
   {:system "You are an expert analyst specializing in Power Law analysis. Your role is to identify highly skewed distributions."
    :user-template "Analyze the following text for Power Law dynamics:

TEXT: {{text}}

Identify:
1. Is the distribution highly skewed?
2. Do a few items dominate?
3. Is this a winner-take-all situation?
4. What are the implications of the power law?
5. How should strategy differ from normal distributions?

Analyze the distribution shape."
    :detection-keywords ["top" "dominant" "winner" "80/20" "pareto" "skewed" "concentrated"]
    :failure-signals ["evenly distributed" "average" "normal" "bell curve"]}

   "scientific-method"
   {:system "You are an expert analyst specializing in Scientific Method analysis. Your role is to identify rigorous hypothesis testing."
    :user-template "Analyze the following text for Scientific Method:

TEXT: {{text}}

Identify:
1. Is there a clear hypothesis?
2. Is it falsifiable?
3. What evidence would disprove it?
4. Are controls being used?
5. Is the conclusion supported by evidence?

Evaluate the scientific rigor."
    :detection-keywords ["hypothesis" "test" "experiment" "evidence" "control" "variable" "prove"]
    :failure-signals ["can't be wrong" "unfalsifiable" "just believe" "no need to test"]}

   "occams-razor"
   {:system "You are an expert analyst specializing in Occam's Razor analysis. Your role is to identify unnecessary complexity."
    :user-template "Analyze the following text for Occam's Razor:

TEXT: {{text}}

Identify:
1. What explanations are being offered?
2. Which is the simplest explanation?
3. Are there unnecessary assumptions?
4. Is complexity justified?
5. What would the simplest sufficient explanation be?

Apply parsimony to the explanations."
    :detection-keywords ["explanation" "theory" "because" "reason" "complex" "simple" "assume"]
    :failure-signals ["complicated" "many factors" "special case" "unique situation"]}})

;; ============================================
;; Pattern Detection Algorithms
;; ============================================

(defn detect-keywords
  "Detect presence of keywords in text."
  [text keywords]
  (let [lower-text (str/lower-case (or text ""))]
    (filter #(str/includes? lower-text (str/lower-case %)) keywords)))

(defn detect-failure-signals
  "Detect failure signals in text."
  [text signals]
  (let [lower-text (str/lower-case (or text ""))]
    (filter #(str/includes? lower-text (str/lower-case %)) signals)))

(defn calculate-model-relevance
  "Calculate how relevant a model is to the given text.
   Returns a score from 0 to 1."
  [model-name text]
  (if-let [prompt-config (get model-prompts model-name)]
    (let [keywords (:detection-keywords prompt-config [])
          signals (:failure-signals prompt-config [])
          keyword-matches (count (detect-keywords text keywords))
          signal-matches (count (detect-failure-signals text signals))
          keyword-score (if (pos? (count keywords))
                          (/ keyword-matches (count keywords))
                          0)
          signal-score (if (pos? (count signals))
                         (/ signal-matches (count signals))
                         0)]
      (+ (* 0.6 keyword-score) (* 0.4 signal-score)))
    0))

(defn rank-models-by-relevance
  "Rank all models by relevance to the given text."
  [text]
  (->> (keys model-prompts)
       (map (fn [model-name]
              {:model model-name
               :relevance (calculate-model-relevance model-name text)
               :keywords-found (detect-keywords text (get-in model-prompts [model-name :detection-keywords] []))
               :signals-found (detect-failure-signals text (get-in model-prompts [model-name :failure-signals] []))}))
       (sort-by :relevance >)
       (filter #(> (:relevance %) 0))))

(defn get-top-models
  "Get the top N most relevant models for the text."
  [text n]
  (take n (rank-models-by-relevance text)))

;; ============================================
;; LM Studio Prompt Generation
;; ============================================

(defn generate-prompt
  "Generate an LM Studio prompt for a specific model and text."
  [model-name text]
  (if-let [prompt-config (get model-prompts model-name)]
    {:system (:system prompt-config)
     :user (str/replace (:user-template prompt-config) "{{text}}" text)}
    nil))

(defn generate-multi-model-prompt
  "Generate a combined prompt for multiple models."
  [model-names text]
  (let [model-sections (for [model-name model-names
                             :let [config (get model-prompts model-name)]
                             :when config]
                         (str "## " (str/upper-case (str/replace model-name "-" " ")) "\n"
                              (:user-template config)))]
    {:system "You are an expert analyst who applies multiple mental models to analyze situations. Provide comprehensive analysis using each requested model."
     :user (str "Analyze the following text using multiple mental models:\n\n"
                "TEXT: " text "\n\n"
                (str/join "\n\n" (map #(str/replace % "{{text}}" "[see above]") model-sections)))}))

;; ============================================
;; Lollapalooza Detection
;; ============================================

(def lollapalooza-combinations
  "Known powerful combinations of mental models that create lollapalooza effects."
  [["incentives" "social-proof" "commitment-consistency"]
   ["loss-aversion" "social-proof" "availability-heuristic"]
   ["confirmation-bias" "commitment-consistency" "narrative-fallacy"]
   ["anchoring" "social-proof" "loss-aversion"]
   ["dunning-kruger" "confirmation-bias" "overconfidence"]
   ["network-effects" "social-proof" "first-mover"]
   ["feedback-loops" "nonlinearity" "emergence"]
   ["incentives" "principal-agent" "game-theory"]])

(defn detect-lollapalooza
  "Detect potential lollapalooza effects when multiple models are active."
  [text]
  (let [relevant-models (set (map :model (get-top-models text 10)))]
    (for [combo lollapalooza-combinations
          :let [matching (filter relevant-models combo)]
          :when (>= (count matching) 2)]
      {:combination combo
       :matching matching
       :strength (/ (count matching) (count combo))
       :warning (str "LOLLAPALOOZA ALERT: " (count matching) "/" (count combo) 
                     " models from powerful combination detected: " 
                     (str/join ", " matching))})))

;; ============================================
;; Failure Mode Analysis
;; ============================================

(defn analyze-failure-modes
  "Analyze text for failure mode signals across all models."
  [text]
  (for [[model-name config] model-prompts
        :let [signals (detect-failure-signals text (:failure-signals config []))]
        :when (seq signals)]
    {:model model-name
     :failure-signals signals
     :severity (cond
                 (>= (count signals) 3) "critical"
                 (>= (count signals) 2) "high"
                 :else "medium")
     :recommendation (str "Review " model-name " failure modes. Detected signals: " 
                          (str/join ", " signals))}))

;; ============================================
;; Comprehensive Analysis
;; ============================================

(defn comprehensive-analysis
  "Perform comprehensive analysis using all algorithms."
  [text]
  {:relevant-models (get-top-models text 5)
   :lollapalooza-effects (detect-lollapalooza text)
   :failure-modes (analyze-failure-modes text)
   :recommended-prompts (map #(generate-prompt (:model %) text) 
                             (get-top-models text 3))})

;; ============================================
;; Export for API
;; ============================================

(defn get-all-model-prompts
  "Get all model prompt configurations."
  []
  model-prompts)

(defn get-model-prompt
  "Get prompt configuration for a specific model."
  [model-name]
  (get model-prompts model-name))

(defn analyze-text
  "Main entry point for text analysis."
  [text & {:keys [models top-n include-prompts]
           :or {top-n 5 include-prompts true}}]
  (let [analysis (comprehensive-analysis text)]
    (if include-prompts
      analysis
      (dissoc analysis :recommended-prompts))))
