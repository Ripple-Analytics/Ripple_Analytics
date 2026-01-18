(ns mental-models.ui.navigation
  "Navigation organized by Munger's Fundamental Organizing Ethos
   
   From hard sciences to soft sciences, with meta-skills at the end:
   1. Mathematics - The foundation of all quantitative thinking
   2. Accounting - The language of business
   3. Physics - Understanding forces and systems
   4. Chemistry - Reactions and transformations
   5. Engineering - Building reliable systems
   6. Biology/Physiology - Living systems and evolution
   7. Psychology - Human behavior and cognition
   8. Economics - Incentives and markets
   9. Flight School - Practical application under pressure
   10. Flight Simulators - Safe practice and iteration
   11. Avoidance of Ideology - Clear thinking
   12. Avoidance of Stupidities - Inversion and error prevention
   13. Fanaticism Awareness - Recognizing extremes
   14. Continual Learning - Lifelong improvement")

;; =============================================================================
;; MUNGER'S HIERARCHY OF KNOWLEDGE
;; =============================================================================

(def munger-hierarchy
  "Ordered from most fundamental (hard sciences) to most complex (human behavior),
   followed by meta-skills for clear thinking"
  
  [{:id :mathematics
    :order 1
    :name "Mathematics"
    :icon "∑"
    :description "The foundation of all quantitative thinking"
    :models ["compound-interest" "probability" "permutations" "algebraic-equivalence"
             "randomness" "stochastic-processes" "compounding" "law-of-large-numbers"
             "normal-distribution" "power-laws" "regression-to-mean" "bayes-theorem"
             "fat-tails" "correlation-vs-causation" "inversion" "multiplicative-systems"]}
   
   {:id :accounting
    :order 2
    :name "Accounting"
    :icon "📊"
    :description "The language of business"
    :models ["double-entry" "depreciation" "amortization" "present-value"
             "opportunity-cost" "sunk-cost" "margin-of-safety" "float"
             "goodwill" "book-value" "intrinsic-value" "cash-flow"]}
   
   {:id :physics
    :order 3
    :name "Physics"
    :icon "⚛️"
    :description "Understanding forces and systems"
    :models ["critical-mass" "momentum" "equilibrium" "leverage"
             "friction" "viscosity" "velocity" "acceleration"
             "thermodynamics" "entropy" "conservation-laws" "relativity"
             "quantum-effects" "feedback-loops" "resonance" "inertia"]}
   
   {:id :chemistry
    :order 4
    :name "Chemistry"
    :icon "🧪"
    :description "Reactions and transformations"
    :models ["catalysts" "activation-energy" "autocatalysis" "chain-reactions"
             "equilibrium-reactions" "saturation" "concentration-gradients"
             "phase-transitions" "half-life" "reaction-rates"]}
   
   {:id :engineering
    :order 5
    :name "Engineering"
    :icon "⚙️"
    :description "Building reliable systems"
    :models ["redundancy" "margin-of-safety" "breakpoints" "backup-systems"
             "fail-safes" "feedback-loops" "bottlenecks" "constraints"
             "quality-control" "tolerances" "stress-testing" "iteration"]}
   
   {:id :biology
    :order 6
    :name "Biology & Physiology"
    :icon "🧬"
    :description "Living systems and evolution"
    :models ["evolution" "natural-selection" "adaptation" "niches"
             "ecosystems" "symbiosis" "parasitism" "red-queen"
             "homeostasis" "immune-system" "hormesis" "senescence"
             "replication" "mutation" "fitness-landscape" "punctuated-equilibrium"]}
   
   {:id :psychology
    :order 7
    :name "Psychology"
    :icon "🧠"
    :description "Human behavior and cognition"
    :models ["incentives" "association" "denial" "reciprocity"
             "social-proof" "consistency" "liking" "authority"
             "scarcity" "contrast" "anchoring" "availability"
             "hindsight-bias" "confirmation-bias" "loss-aversion" "overconfidence"
             "narrative-fallacy" "survivorship-bias" "dunning-kruger" "halo-effect"
             "fundamental-attribution-error" "self-serving-bias" "groupthink"
             "commitment-escalation" "stress-influence" "senescence-misinfluence"
             "drug-misinfluence" "twaddle-tendency" "reason-respecting"
             "lollapalooza-effects"]}
   
   {:id :economics
    :order 8
    :name "Economics"
    :icon "📈"
    :description "Incentives and markets"
    :models ["supply-demand" "comparative-advantage" "creative-destruction"
             "diminishing-returns" "economies-of-scale" "network-effects"
             "monopoly" "oligopoly" "price-discrimination" "externalities"
             "moral-hazard" "adverse-selection" "principal-agent" "tragedy-of-commons"
             "game-theory" "prisoners-dilemma" "nash-equilibrium" "auction-theory"
             "market-efficiency" "arbitrage" "speculation" "bubbles"]}
   
   {:id :flight-school
    :order 9
    :name "Flight School"
    :icon "✈️"
    :description "Practical application under pressure"
    :models ["checklists" "standard-procedures" "crew-resource-management"
             "situational-awareness" "decision-making-under-pressure"
             "risk-assessment" "go-no-go-decisions" "abort-criteria"
             "emergency-procedures" "communication-protocols"]}
   
   {:id :flight-simulators
    :order 10
    :name "Flight Simulators"
    :icon "🎮"
    :description "Safe practice and iteration"
    :models ["simulation" "scenario-planning" "stress-testing" "war-gaming"
             "pre-mortems" "red-teaming" "tabletop-exercises" "monte-carlo"
             "backtesting" "paper-trading" "dry-runs" "rehearsals"]}
   
   {:id :ideology-avoidance
    :order 11
    :name "Avoidance of Ideology"
    :icon "🚫"
    :description "Clear thinking free from dogma"
    :models ["first-principles" "steel-manning" "ideological-immunity"
             "tribal-blindness" "political-bias" "religious-bias"
             "scientific-dogma" "academic-politics" "groupthink"
             "echo-chambers" "filter-bubbles" "motivated-reasoning"]}
   
   {:id :stupidity-avoidance
    :order 12
    :name "Avoidance of Stupidities"
    :icon "⚠️"
    :description "Inversion and error prevention"
    :models ["inversion" "via-negativa" "antifragility" "margin-of-safety"
             "redundancy" "diversification" "position-sizing" "stop-losses"
             "pre-mortems" "checklists" "second-order-thinking"
             "unintended-consequences" "iatrogenics" "intervention-bias"]}
   
   {:id :fanaticism-awareness
    :order 13
    :name "Fanaticism Awareness"
    :icon "🔥"
    :description "Recognizing and avoiding extremes"
    :models ["extremism-detection" "cult-patterns" "true-believer-syndrome"
             "ideological-possession" "purity-spirals" "moral-panics"
             "mass-hysteria" "mob-psychology" "deindividuation"
             "milgram-obedience" "stanford-prison" "asch-conformity"]}
   
   {:id :continual-learning
    :order 14
    :name "Continual Learning"
    :icon "📚"
    :description "Lifelong improvement and adaptation"
    :models ["deliberate-practice" "spaced-repetition" "interleaving"
             "desirable-difficulties" "growth-mindset" "feedback-loops"
             "reflection" "journaling" "mentorship" "reading-widely"
             "cross-disciplinary" "t-shaped-knowledge" "circle-of-competence"
             "expanding-competence" "intellectual-humility"]}])

;; =============================================================================
;; NAVIGATION STRUCTURE
;; =============================================================================

(def nav-sections
  "Main navigation sections following Munger's hierarchy"
  
  [{:id :dashboard
    :name "Command Center"
    :icon "⚡"
    :path "/"
    :description "Real-time processing stats and system health"}
   
   {:id :beast
    :name "Beast Mode"
    :icon "🔥"
    :path "/beast"
    :description "Point at folders and devour everything"}
   
   {:id :knowledge
    :name "Knowledge Base"
    :icon "🧠"
    :path "/knowledge"
    :description "All mental models organized by Munger's hierarchy"
    :children (mapv (fn [cat]
                      {:id (:id cat)
                       :name (:name cat)
                       :icon (:icon cat)
                       :path (str "/knowledge/" (name (:id cat)))
                       :model-count (count (:models cat))})
                    munger-hierarchy)}
   
   {:id :patterns
    :name "Pattern Detection"
    :icon "🔍"
    :path "/patterns"
    :description "Automatically detected patterns across all data"}
   
   {:id :insights
    :name "Insights"
    :icon "💡"
    :path "/insights"
    :description "AI-generated insights and recommendations"}
   
   {:id :mesh
    :name "Compute Mesh"
    :icon "🌐"
    :path "/mesh"
    :description "Distributed compute across all devices"}
   
   {:id :storage
    :name "Data Lake"
    :icon "🗄️"
    :path "/storage"
    :description "Petabyte-scale distributed storage"}
   
   {:id :settings
    :name "Settings"
    :icon "⚙️"
    :path "/settings"
    :description "System configuration"}])

;; =============================================================================
;; MODEL LOOKUP BY CATEGORY
;; =============================================================================

(defn models-by-category
  "Get all models for a category"
  [category-id]
  (->> munger-hierarchy
       (filter #(= category-id (:id %)))
       first
       :models))

(defn category-for-model
  "Find which category a model belongs to"
  [model-id]
  (->> munger-hierarchy
       (filter #(some #{model-id} (:models %)))
       first))

(defn all-models
  "Get all models across all categories"
  []
  (mapcat :models munger-hierarchy))

(defn model-count-by-category
  "Get model counts per category"
  []
  (mapv (fn [cat]
          {:category (:name cat)
           :count (count (:models cat))})
        munger-hierarchy))

;; =============================================================================
;; NAVIGATION HELPERS
;; =============================================================================

(defn get-nav-item
  "Get navigation item by id"
  [id]
  (first (filter #(= id (:id %)) nav-sections)))

(defn get-breadcrumbs
  "Generate breadcrumbs for a path"
  [path]
  (let [parts (filter seq (clojure.string/split path #"/"))]
    (loop [remaining parts
           crumbs [{:name "Home" :path "/"}]
           current-path ""]
      (if (empty? remaining)
        crumbs
        (let [part (first remaining)
              new-path (str current-path "/" part)]
          (recur (rest remaining)
                 (conj crumbs {:name (clojure.string/capitalize part)
                               :path new-path})
                 new-path))))))

;; =============================================================================
;; SEARCH ACROSS NAVIGATION
;; =============================================================================

(defn search-navigation
  "Search across all navigation items and models"
  [query]
  (let [q (clojure.string/lower-case query)
        nav-matches (->> nav-sections
                         (filter #(clojure.string/includes? 
                                    (clojure.string/lower-case (:name %)) q)))
        category-matches (->> munger-hierarchy
                              (filter #(clojure.string/includes?
                                         (clojure.string/lower-case (:name %)) q)))
        model-matches (->> munger-hierarchy
                           (mapcat (fn [cat]
                                     (map (fn [m] {:model m :category (:name cat)})
                                          (filter #(clojure.string/includes? % q)
                                                  (:models cat))))))]
    {:nav-matches nav-matches
     :category-matches category-matches
     :model-matches model-matches
     :total (+ (count nav-matches) 
               (count category-matches) 
               (count model-matches))}))
