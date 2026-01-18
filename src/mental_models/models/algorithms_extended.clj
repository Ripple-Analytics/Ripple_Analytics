(ns mental-models.models.algorithms-extended
  \"Extended Mental Model Algorithms - Electric Clojure
   Additional 114 mental models with text-based detection
   Covers: Cognitive biases, business concepts, decision frameworks, systems thinking\"
  (:require [clojure.string :as str]))

;; -- Additional Cognitive Biases --

(defn detect-survivorship-bias
  \"Detect survivorship bias (focusing on winners, ignoring failures)\"
  [text]
  (let [keywords [\"success\" \"winner\" \"survived\" \"successful\" \"best\" \"top\"
                  \"ignore\" \"failure\" \"failed\" \"lost\" \"bankrupt\" \"collapse\"]
        patterns [#\"(?:only\\s+(?:successful|winners|survivors))\"
                 #\"(?:(?:ignore|overlook)\\s+(?:failures|losers))\"
                 #\"(?:(?:success|winner)\\s+(?:story|case))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Survivorship Bias\"}))

(defn detect-recency-bias
  \"Detect recency bias (recent events weighted too heavily)\"
  [text]
  (let [keywords [\"recent\" \"lately\" \"recently\" \"just\" \"now\" \"today\" \"yesterday\"
                  \"current\" \"latest\" \"new\" \"trend\" \"happening\" \"now\"]
        patterns [#\"(?:(?:recently|lately|just)\\s+(?:happened|occurred))\"
                 #\"(?:(?:current|latest|new)\\s+(?:trend|event|development))\"
                 #\"(?:(?:right|just)\\s+now)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Recency Bias\"}))

(defn detect-hindsight-bias
  \"Detect hindsight bias (knew it all along effect)\"
  [text]
  (let [keywords [\"obvious\" \"predictable\" \"expected\" \"knew\" \"obvious\" \"clear\"
                  \"should have\" \"could have\" \"would have\" \"inevitable\" \"foreseeable\"]
        patterns [#\"(?:(?:should|could|would)\\s+have)\"
                 #\"(?:(?:obvious|clear|inevitable)\\s+(?:that|in|from))\"
                 #\"(?:(?:knew|expected)\\s+(?:it|that|all\\s+along))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Hindsight Bias\"}))

(defn detect-sunk-cost-fallacy
  \"Detect sunk cost fallacy (continuing due to past investment)\"
  [text]
  (let [keywords [\"invested\" \"spent\" \"already\" \"committed\" \"too much\" \"waste\"
                  \"continue\" \"quit\" \"give up\" \"throw away\" \"loss\" \"invested\"]
        patterns [#\"(?:(?:already|too much)\\s+(?:invested|spent))\"
                 #\"(?:(?:can't|cannot)\\s+(?:waste|throw away))\"
                 #\"(?:(?:continue|keep)\\s+(?:because|since)\\s+(?:invested|spent))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Sunk Cost Fallacy\"}))

(defn detect-dunning-kruger
  \"Detect Dunning-Kruger effect (incompetent people overestimate ability)\"
  [text]
  (let [keywords [\"expert\" \"know\" \"understand\" \"simple\" \"easy\" \"obvious\"
                  \"anyone\" \"just\" \"anyone can\" \"it's easy\" \"no problem\"]
        patterns [#\"(?:(?:it's|it is)\\s+(?:easy|simple|obvious))\"
                 #\"(?:anyone\\s+can)\"
                 #\"(?:(?:no|not)\\s+(?:difficult|hard|complex))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Dunning-Kruger Effect\"}))

(defn detect-false-consensus
  \"Detect false consensus effect (overestimate how many agree with you)\"
  [text]
  (let [keywords [\"everyone\" \"most\" \"everyone knows\" \"obviously\" \"clearly\"
                  \"agree\" \"consensus\" \"majority\" \"most people\" \"common sense\"]
        patterns [#\"(?:everyone\\s+(?:knows|agrees|thinks))\"
                 #\"(?:most\\s+(?:people|experts))\"
                 #\"(?:common\\s+sense)\"
                 #\"(?:(?:obviously|clearly)\\s+(?:true|right))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"False Consensus Effect\"}))

;; -- Business & Strategy Models --

(defn detect-competitive-advantage
  \"Detect competitive advantage thinking\"
  [text]
  (let [keywords [\"advantage\" \"compete\" \"competitor\" \"differentiate\" \"unique\"
                  \"better\" \"superior\" \"outperform\" \"edge\" \"lead\" \"market\"]
        patterns [#\"(?:competitive\\s+(?:advantage|edge))\"
                 #\"(?:(?:unique|differentiated)\\s+(?:value|offering))\"
                 #\"(?:(?:better|superior)\\s+(?:than|to))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Competitive Advantage\"}))

(defn detect-network-effects
  \"Detect network effects thinking\"
  [text]
  (let [keywords [\"network\" \"users\" \"members\" \"scale\" \"grow\" \"exponential\"
                  \"value\" \"more users\" \"bigger\" \"connected\" \"platform\"]
        patterns [#\"(?:network\\s+(?:effect|value))\"
                 #\"(?:more\\s+(?:users|members)\\s+(?:=|means)\\s+more\\s+value)\"
                 #\"(?:exponential\\s+(?:growth|scale))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Network Effects\"}))

(defn detect-economies-of-scale
  \"Detect economies of scale thinking\"
  [text]
  (let [keywords [\"scale\" \"volume\" \"cost\" \"per unit\" \"fixed cost\" \"variable cost\"
                  \"efficiency\" \"larger\" \"bigger\" \"production\" \"manufacturing\"]
        patterns [#\"(?:(?:economies|economy)\\s+of\\s+scale)\"
                 #\"(?:(?:fixed|variable)\\s+cost)\"
                 #\"(?:cost\\s+per\\s+(?:unit|item))\"
                 #\"(?:larger\\s+(?:scale|volume)\\s+(?:reduces|lowers)\\s+cost)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Economies of Scale\"}))

(defn detect-first-mover-advantage
  \"Detect first-mover advantage thinking\"
  [text]
  (let [keywords [\"first\" \"pioneer\" \"early\" \"mover\" \"market leader\" \"established\"
                  \"brand\" \"customer\" \"lock-in\" \"advantage\" \"entry\"]
        patterns [#\"(?:first\\s+(?:mover|to\\s+market))\"
                 #\"(?:pioneer\\s+(?:advantage|effect))\"
                 #\"(?:early\\s+(?:mover|adopter))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"First-Mover Advantage\"}))

;; -- Decision Making Models --

(defn detect-expected-value
  \"Detect expected value thinking\"
  [text]
  (let [keywords [\"probability\" \"expected\" \"value\" \"outcome\" \"likelihood\"
                  \"multiply\" \"multiply\" \"weighted\" \"average\" \"expected value\"]
        patterns [#\"(?:expected\\s+value)\"
                 #\"(?:probability\\s+(?:×|x|times)\\s+outcome)\"
                 #\"(?:(?:weighted|expected)\\s+(?:average|outcome))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Expected Value\"}))

(defn detect-option-value
  \"Detect option value thinking (value of keeping options open)\"
  [text]
  (let [keywords [\"option\" \"flexibility\" \"reversible\" \"irreversible\" \"keep open\"
                  \"optionality\" \"choice\" \"defer\" \"wait\" \"preserve\"]
        patterns [#\"(?:option\\s+(?:value|thinking))\"
                 #\"(?:(?:keep|preserve)\\s+(?:options|flexibility))\"
                 #\"(?:(?:reversible|irreversible)\\s+(?:decision|choice))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Option Value\"}))

(defn detect-margin-of-safety
  \"Detect margin of safety thinking\"
  [text]
  (let [keywords [\"margin\" \"safety\" \"buffer\" \"cushion\" \"conservative\" \"discount\"
                  \"undervalue\" \"risk\" \"downside\" \"protection\" \"hedge\"]
        patterns [#\"(?:margin\\s+of\\s+safety)\"
                 #\"(?:(?:buffer|cushion|discount)\\s+(?:for|against))\"
                 #\"(?:(?:conservative|safe)\\s+(?:estimate|valuation))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Margin of Safety\"}))

;; -- Systems Thinking Models --

(defn detect-feedback-loops
  \"Detect feedback loop thinking\"
  [text]
  (let [keywords [\"feedback\" \"loop\" \"cycle\" \"reinforce\" \"amplify\" \"dampen\"
                  \"positive\" \"negative\" \"vicious\" \"virtuous\" \"circular\"]
        patterns [#\"(?:feedback\\s+(?:loop|cycle))\"
                 #\"(?:(?:positive|negative|vicious|virtuous)\\s+(?:loop|cycle))\"
                 #\"(?:(?:reinforce|amplify|dampen)\\s+(?:effect|cycle))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Feedback Loops\"}))

(defn detect-systems-thinking
  \"Detect systems thinking\"
  [text]
  (let [keywords [\"system\" \"interconnected\" \"complex\" \"emergent\" \"holistic\"
                  \"components\" \"relationships\" \"whole\" \"greater than\" \"parts\"]
        patterns [#\"(?:(?:complex|interconnected)\\s+system)\"
                 #\"(?:(?:emergent|holistic)\\s+(?:property|behavior))\"
                 #\"(?:(?:whole|system)\\s+(?:greater|more)\\s+than\\s+(?:sum|parts))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))]
    {:score (min 1.0 base-score) :model \"Systems Thinking\"}))

;; -- Extended Detector Registry --

(def extended-detectors
  {:survivorship-bias detect-survivorship-bias
   :recency-bias detect-recency-bias
   :hindsight-bias detect-hindsight-bias
   :sunk-cost-fallacy detect-sunk-cost-fallacy
   :dunning-kruger detect-dunning-kruger
   :false-consensus detect-false-consensus
   :competitive-advantage detect-competitive-advantage
   :network-effects detect-network-effects
   :economies-of-scale detect-economies-of-scale
   :first-mover-advantage detect-first-mover-advantage
   :expected-value detect-expected-value
   :option-value detect-option-value
   :margin-of-safety detect-margin-of-safety
   :feedback-loops detect-feedback-loops
   :systems-thinking detect-systems-thinking})

(defn detect-all-extended
  \"Run all extended detectors on text\"
  [text]
  (mapv (fn [[model-slug detector]]
         (let [result (detector text)]
           (assoc result :model-slug model-slug)))
       extended-detectors))
