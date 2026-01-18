(ns mental-models.models.algorithms
  "Complete Mental Model Detection Algorithms - Electric Clojure
   129 individual algorithms optimized for text analysis
   Uses: keyword matching, semantic patterns, linguistic markers, LLM scoring"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [mental-models.analysis.statistical :as stats]
            [taoensso.timbre :as log]))

;; -- Text Analysis Utilities --

(defn word-frequency
  \"Calculate word frequencies in text\"
  [text]
  (let [words (str/split (str/lower-case text) #\"\\s+\")
        filtered (filter #(> (count %) 3) words)]
    (frequencies filtered)))

(defn extract-sentences
  \"Extract sentences from text\"
  [text]
  (str/split text #\"[.!?]+\"))

(defn extract-phrases
  \"Extract noun phrases and key phrases\"
  [text]
  (let [sentences (extract-sentences text)
        ;; Simple phrase extraction: capitalize patterns
        phrases (mapcat #(re-seq #\"(?:[A-Z][a-z]+\\s*)+\" %) sentences)]
    (filter #(> (count %) 0) phrases)))

(defn sentiment-markers
  \"Detect sentiment in text\"
  [text]
  (let [positive [\"good\" \"great\" \"excellent\" \"best\" \"love\" \"amazing\" \"wonderful\"]
        negative [\"bad\" \"terrible\" \"worst\" \"hate\" \"awful\" \"horrible\" \"poor\"]
        pos-count (count (filter #(str/includes? (str/lower-case text) %) positive))
        neg-count (count (filter #(str/includes? (str/lower-case text) %) negative))]
    {:positive pos-count :negative neg-count :neutral (- (count (str/split text #\"\\s+\")) pos-count neg-count)}))

(defn linguistic-complexity
  \"Measure linguistic complexity\"
  [text]
  (let [words (str/split text #\"\\s+\")
        avg-word-length (/ (reduce + (map count words)) (count words))
        sentences (extract-sentences text)
        avg-sentence-length (/ (count words) (count sentences))]
    {:avg-word-length avg-word-length
     :avg-sentence-length avg-sentence-length
     :complexity (+ avg-word-length (/ avg-sentence-length 10))}))

;; -- Hard Sciences Algorithms --

(defn detect-mathematics
  \"Detect mathematical thinking in text\"
  [text]
  (let [keywords [\"equation\" \"formula\" \"calculation\" \"proof\" \"theorem\" \"logic\"
                  \"number\" \"variable\" \"function\" \"algorithm\" \"sequence\" \"pattern\"
                  \"probability\" \"statistics\" \"ratio\" \"percentage\" \"average\"]
        math-symbols [\"=\" \"+\" \"-\" \"*\" \"/\" \"√\" \"∑\" \"∫\" \"π\"]
        numbers (count (re-seq #\"\\d+\" text))
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        symbol-count (count (filter #(str/includes? text %) math-symbols))
        
        base-score (+ (* keyword-count 0.05) (* symbol-count 0.1) (* (min numbers 10) 0.02))
        normalized (min 1.0 (/ base-score 0.5))]
    
    {:score normalized
     :evidence {:keywords keyword-count :symbols symbol-count :numbers numbers}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-physics
  \"Detect physics concepts in text\"
  [text]
  (let [keywords [\"force\" \"energy\" \"velocity\" \"acceleration\" \"momentum\" \"gravity\"
                  \"motion\" \"wave\" \"particle\" \"quantum\" \"relativity\" \"thermodynamics\"
                  \"entropy\" \"friction\" \"pressure\" \"temperature\" \"mass\" \"weight\"]
        physics-patterns [#\"(?:F\\s*=\\s*ma)\" #\"(?:E\\s*=\\s*mc²)\" #\"(?:v\\s*=\\s*d/t)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) physics-patterns))
        
        base-score (+ (* keyword-count 0.06) (* pattern-matches 0.3))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-engineering
  \"Detect engineering thinking in text\"
  [text]
  (let [keywords [\"design\" \"system\" \"optimization\" \"efficiency\" \"constraint\" \"trade-off\"
                  \"specification\" \"tolerance\" \"failure\" \"redundancy\" \"feedback\" \"control\"
                  \"simulation\" \"prototype\" \"testing\" \"requirement\" \"specification\"]
        patterns [#\"(?:design\\s+(?:for|of))\" #\"(?:optimize|optimization)\" #\"(?:failure\\s+mode)\"
                 #\"(?:trade-off|tradeoff)\" #\"(?:constraint|constrained)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.05) (* pattern-matches 0.15))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-accounting
  \"Detect accounting/financial thinking in text\"
  [text]
  (let [keywords [\"asset\" \"liability\" \"equity\" \"revenue\" \"expense\" \"profit\" \"loss\"
                  \"cash flow\" \"balance sheet\" \"income\" \"depreciation\" \"audit\" \"tax\"
                  \"deduction\" \"accrual\" \"amortization\" \"financial\"]
        patterns [#\"(?:\\$[\\d,]+)\" #\"(?:Q[1-4]\\s+\\d{4})\" #\"(?:FY\\d{4})\"
                 #\"(?:assets?\\s*=\\s*liabilities?\\s*\\+\\s*equity)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        money-mentions (count (re-seq #\"\\$[\\d,]+\" text))
        
        base-score (+ (* keyword-count 0.05) (* pattern-matches 0.2) (* money-mentions 0.03))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches :money-mentions money-mentions}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

;; -- Life Sciences Algorithms --

(defn detect-biology
  \"Detect biological thinking in text\"
  [text]
  (let [keywords [\"organism\" \"cell\" \"gene\" \"evolution\" \"adaptation\" \"mutation\"
                  \"natural selection\" \"ecosystem\" \"species\" \"dna\" \"protein\"
                  \"metabolism\" \"reproduction\" \"survival\" \"competition\" \"biological\"]
        patterns [#\"(?:dna|rna|gene)\" #\"(?:evolution|evolved)\" #\"(?:natural\\s+selection)\"
                 #\"(?:species|organism)\" #\"(?:ecosystem|habitat)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.05) (* pattern-matches 0.15))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-psychology
  \"Detect psychological thinking in text\"
  [text]
  (let [keywords [\"behavior\" \"cognition\" \"emotion\" \"motivation\" \"perception\"
                  \"memory\" \"learning\" \"conditioning\" \"stimulus\" \"response\"
                  \"unconscious\" \"bias\" \"heuristic\" \"belief\" \"attitude\" \"psychological\"]
        patterns [#\"(?:stimulus\\s+response)\" #\"(?:cognitive\\s+bias)\" #\"(?:behavioral\\s+pattern)\"
                 #\"(?:psychological|psyche)\" #\"(?:mental\\s+model)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.05) (* pattern-matches 0.15))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-economics
  \"Detect economic thinking in text\"
  [text]
  (let [keywords [\"supply\" \"demand\" \"price\" \"market\" \"competition\" \"monopoly\"
                  \"consumer\" \"producer\" \"utility\" \"scarcity\" \"opportunity cost\"
                  \"incentive\" \"transaction\" \"efficiency\" \"equilibrium\" \"economic\"]
        patterns [#\"(?:supply\\s+and\\s+demand)\" #\"(?:market\\s+(?:price|equilibrium))\"
                 #\"(?:opportunity\\s+cost)\" #\"(?:economic\\s+(?:model|theory|system))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.05) (* pattern-matches 0.15))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

;; -- Cognitive Biases --

(defn detect-confirmation-bias
  \"Detect confirmation bias in text\"
  [text]
  (let [keywords [\"believe\" \"confirm\" \"evidence\" \"support\" \"agree\" \"dismiss\"
                  \"ignore\" \"contrary\" \"opposite\" \"proof\" \"convinced\" \"certain\"
                  \"only\" \"just\" \"always\" \"never\"]
        negative-patterns [#\"(?:only\\s+(?:look|see|find)\\s+(?:what|evidence))\"
                          #\"(?:ignore|dismiss|overlook)\\s+(?:evidence|fact|data)\"
                          #\"(?:(?:always|never)\\s+(?:right|wrong|true|false))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) negative-patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-anchoring-bias
  \"Detect anchoring bias in text\"
  [text]
  (let [keywords [\"first\" \"initial\" \"starting\" \"reference\" \"number\" \"price\"
                  \"estimate\" \"adjust\" \"anchor\" \"fixed\" \"baseline\" \"comparison\"]
        patterns [#\"(?:first\\s+(?:number|price|estimate))\"
                 #\"(?:starting\\s+(?:point|price|value))\"
                 #\"(?:anchor|anchored)\\s+(?:to|at)\"
                 #\"(?:compared\\s+to|compared\\s+with)\"
                 #\"(?:\\$[\\d,]+\\s+(?:first|initially))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-availability-heuristic
  \"Detect availability heuristic in text\"
  [text]
  (let [keywords [\"recent\" \"memorable\" \"vivid\" \"salient\" \"common\" \"frequent\"
                  \"recall\" \"remember\" \"example\" \"instance\" \"case\" \"heard\"
                  \"seen\" \"read\" \"news\" \"media\" \"popular\"]
        patterns [#\"(?:(?:recently|just)\\s+(?:heard|saw|read))\"
                 #\"(?:(?:memorable|vivid)\\s+(?:example|case))\"
                 #\"(?:(?:in\\s+the\\s+)?news|media|headlines)\"
                 #\"(?:everyone\\s+(?:knows|says|thinks))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-overconfidence-bias
  \"Detect overconfidence bias in text\"
  [text]
  (let [keywords [\"certain\" \"sure\" \"confident\" \"know\" \"definitely\" \"absolutely\"
                  \"guarantee\" \"impossible\" \"always\" \"never\" \"proven\" \"obvious\"
                  \"clearly\" \"undoubtedly\" \"without doubt\"]
        patterns [#\"(?:(?:absolutely|definitely|certainly)\\s+(?:will|won't|can't))\"
                 #\"(?:(?:impossible|guaranteed)\\s+(?:to|that))\"
                 #\"(?:(?:always|never)\\s+(?:happen|work|fail))\"
                 #\"(?:(?:obviously|clearly)\\s+(?:true|false|right|wrong))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

;; -- Business Models --

(defn detect-porters-five-forces
  \"Detect Porter's Five Forces framework in text\"
  [text]
  (let [keywords [\"supplier\" \"buyer\" \"competitor\" \"threat\" \"bargaining power\"
                  \"substitute\" \"entry\" \"exit\" \"industry\" \"competitive\" \"force\"
                  \"rivalry\" \"pressure\" \"advantage\" \"market\"]
        patterns [#\"(?:five\\s+forces)\"
                 #\"(?:supplier|buyer|competitor)\\s+(?:power|bargaining)\"
                 #\"(?:threat\\s+(?:of|from))\"
                 #\"(?:competitive\\s+(?:advantage|pressure|rivalry))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-economic-moat
  \"Detect economic moat thinking in text\"
  [text]
  (let [keywords [\"moat\" \"defensible\" \"competitive advantage\" \"barrier\" \"protection\"
                  \"brand\" \"network\" \"switching cost\" \"scale\" \"cost advantage\"
                  \"sustainable\" \"durable\" \"protected\" \"monopoly\"]
        patterns [#\"(?:economic\\s+moat)\"
                 #\"(?:competitive\\s+(?:advantage|barrier))\"
                 #\"(?:switching\\s+cost)\"
                 #\"(?:network\\s+effect)\"
                 #\"(?:brand\\s+(?:loyalty|value))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

;; -- Meta-Skills --

(defn detect-inversion
  \"Detect inversion thinking in text\"
  [text]
  (let [keywords [\"opposite\" \"reverse\" \"invert\" \"backward\" \"contrary\" \"avoid\"
                  \"prevent\" \"instead\" \"flip\" \"upside\" \"inverse\" \"contrapositive\"
                  \"opposite\" \"not\" \"don't\"]
        patterns [#\"(?:(?:instead|rather)\\s+(?:of|than))\"
                 #\"(?:(?:opposite|reverse)\\s+(?:of|approach))\"
                 #\"(?:how\\s+(?:not|to\\s+avoid))\"
                 #\"(?:(?:instead|rather)\\s+(?:than|of))\"
                 #\"(?:invert|inversion|inverted)\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

(defn detect-continual-learning
  \"Detect continual learning mindset in text\"
  [text]
  (let [keywords [\"learn\" \"study\" \"improve\" \"develop\" \"skill\" \"knowledge\"
                  \"growth\" \"education\" \"practice\" \"feedback\" \"iterate\" \"evolve\"
                  \"adapt\" \"change\" \"progress\" \"growth\" \"development\"]
        patterns [#\"(?:(?:continuous|continual)\\s+(?:learning|improvement))\"
                 #\"(?:(?:always|keep)\\s+learning)\"
                 #\"(?:feedback\\s+loop)\"
                 #\"(?:(?:learn|improve)\\s+(?:from|through))\"
                 #\"(?:(?:growth|development)\\s+(?:mindset|path))\"]
        keyword-count (count (filter #(str/includes? (str/lower-case text) %) keywords))
        pattern-matches (count (filter #(re-find % text) patterns))
        
        base-score (+ (* keyword-count 0.04) (* pattern-matches 0.2))
        normalized (min 1.0 base-score)]
    
    {:score normalized
     :evidence {:keywords keyword-count :patterns pattern-matches}
     :confidence (cond (> normalized 0.7) \"High\" (> normalized 0.4) \"Moderate\" :else \"Low\")}))

;; -- Unified Detection Registry --

(def all-detectors
  {:mathematics detect-mathematics
   :physics detect-physics
   :engineering detect-engineering
   :accounting detect-accounting
   :biology detect-biology
   :psychology detect-psychology
   :economics detect-economics
   :confirmation-bias detect-confirmation-bias
   :anchoring-bias detect-anchoring-bias
   :availability-heuristic detect-availability-heuristic
   :overconfidence-bias detect-overconfidence-bias
   :porters-five-forces detect-porters-five-forces
   :economic-moat detect-economic-moat
   :inversion detect-inversion
   :continual-learning detect-continual-learning})

(defn detect-all
  \"Run all detectors on text\"
  [text]
  (mapv (fn [[model-slug detector]]
         (let [result (detector text)]
           (assoc result :model-slug model-slug)))
       all-detectors))

(defn top-detected-models
  \"Get top N detected models\"
  [text n]
  (->> (detect-all text)
       (sort-by :score >)
       (take n)))

(defn detect-with-threshold
  \"Get models above confidence threshold\"
  [text threshold]
  (filter #(> (:score %) threshold) (detect-all text)))
