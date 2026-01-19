# Mental Model Detection Algorithms: Chemistry

## Task for @Devin

Build text detection algorithms for all mental models in the **Chemistry** category.

### Requirements

1. **Pattern Matching**: Create regex patterns and keyword lists for each model
2. **Semantic Analysis**: LM Studio prompts for deeper detection
3. **Scoring System**: 0-1 confidence score for model applicability
4. **Evidence Extraction**: Pull relevant text snippets as evidence
5. **Failure Mode Detection**: Identify when model is being misapplied

### File Structure
```
src/mental_models/models/algorithms/chemistry/
├── core.clj          # Main detection functions
├── patterns.clj      # Regex and keyword patterns
├── prompts.clj       # LM Studio prompt templates
├── scoring.clj       # Confidence scoring logic
└── tests.clj         # Unit tests
```

### Models to Implement
(Query database for models in this category)

### Example Algorithm Structure
```clojure
(defn detect-model [text model-id]
  (let [keywords (get-keywords model-id)
        patterns (get-patterns model-id)
        keyword-score (calculate-keyword-density text keywords)
        pattern-matches (find-pattern-matches text patterns)
        semantic-score (when (> keyword-score 0.3)
                        (analyze-with-llm text model-id))]
    {:model-id model-id
     :confidence (combine-scores keyword-score pattern-matches semantic-score)
     :evidence (extract-evidence text pattern-matches)
     :failure-modes (detect-failure-modes text model-id)}))
```

### Acceptance Criteria
- [ ] All models in category have detection algorithms
- [ ] Unit tests pass with >80% accuracy on test corpus
- [ ] LM Studio prompts are specific to each model
- [ ] Evidence extraction works correctly
- [ ] Failure mode detection implemented

Branch: `feature/mental-model-algorithms-chemistry`

