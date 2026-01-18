# Electric Clojure Mental Model Algorithms: moats

## Task for @Devin

Build **Electric Clojure** text detection algorithms for all mental models in the **moats** category.

**CRITICAL**: Use Electric Clojure (`.cljc` files) so algorithms can:
- React to each other (Lollapalooza detection)
- Stream results to UI in real-time
- Share state across models
- Run on both server and client

## Requirements

### 1. Electric Clojure Structure
```clojure
(ns mental-models.algorithms.moats
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

;; Reactive atom for model state
(e/def !model-results (atom {}))

;; Electric defn for reactive detection
(e/defn detect-MODEL_NAME [text]
  (e/server
    (let [keywords ["keyword1" "keyword2"]
          patterns [#"pattern1" #"pattern2"]
          keyword-score (calculate-keyword-density text keywords)
          pattern-matches (find-matches text patterns)]
      (when (> keyword-score 0.3)
        (let [semantic-score (e/offload #(analyze-with-llm text))]
          {:model-id :MODEL_NAME
           :confidence (combine-scores keyword-score semantic-score)
           :evidence (extract-evidence text pattern-matches)})))))
```

### 2. Pattern Matching
- Keyword lists specific to each model
- Regex patterns for structural detection
- Phrase matching for complex concepts

### 3. LM Studio Integration
```clojure
(defn llm-prompt-for-MODEL [text]
  "You are analyzing text for the MODEL_NAME mental model.
   
   MODEL_NAME: [Description from Munger's writings]
   
   Indicators to look for:
   - [Specific indicator 1]
   - [Specific indicator 2]
   
   Rate 0.0-1.0 how strongly this text exhibits MODEL_NAME.
   Provide evidence quotes.")
```

### 4. Cross-Model Interaction (Lollapalooza)
```clojure
(e/defn detect-lollapalooza [text]
  (e/server
    (let [all-results @!model-results
          high-confidence (filter #(> (:confidence %) 0.7) (vals all-results))]
      (when (>= (count high-confidence) 3)
        {:lollapalooza true
         :converging-models (map :model-id high-confidence)
         :combined-confidence (apply * (map :confidence high-confidence))}))))
```

## File Structure
```
src/mental_models/algorithms/moats/
├── core.cljc           # Main Electric detection functions
├── patterns.cljc       # Keywords and regex patterns
├── prompts.cljc        # LM Studio prompt templates
├── scoring.cljc        # Confidence scoring logic
├── interactions.cljc   # Cross-model reactive interactions
└── tests.cljc          # Unit tests
```

## Models to Implement
(See database for full list - query `mental_models` table where category = 'moats')

## Acceptance Criteria
- [ ] All models use Electric Clojure (`.cljc` files)
- [ ] Reactive state updates when text is analyzed
- [ ] Cross-model Lollapalooza detection works
- [ ] LM Studio prompts are model-specific
- [ ] Unit tests pass with >80% accuracy
- [ ] Real-time streaming to UI works

## Reference
- Munger's writings PDF in repo
- Existing algorithms in `src/mental_models/models/algorithms.clj`
- Electric Clojure docs: https://github.com/hyperfiddle/electric

Branch: `feature/electric-algorithms-moats`
