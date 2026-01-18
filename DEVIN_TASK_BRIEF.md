# DEVIN TASK BRIEF: Mental Models System Build-Out

**Repository:** https://github.com/Ripple-Analytics/Ripple_Analytics
**Priority:** HIGH
**Estimated Scope:** Large (multiple days of work)

---

## OVERVIEW

Build a unified Electric Clojure system for mental model analysis. The system consists of:

1. **CLJFX Desktop App** - Native Windows/Mac/Linux app for file watching and analysis
2. **Text Analysis Algorithms** - Detection algorithms for 200+ mental models
3. **Complete Mental Model Database** - Expand from 167 to 200+ models
4. **Integration** - Desktop app syncs with web app via Electric Clojure

**Key Principle:** Everything in Clojure. No Python. No JavaScript (except Electron shell if needed). Unified codebase.

---

## TASK 1: CLJFX Desktop Application

Build a native desktop app using CLJFX (ClojureFX) that shares code with the backend.

### Requirements

```clojure
;; deps.edn additions needed
{:deps {cljfx/cljfx {:mvn/version "1.7.24"}
        org.openjfx/javafx-controls {:mvn/version "21"}
        org.openjfx/javafx-base {:mvn/version "21"}
        org.apache.pdfbox/pdfbox {:mvn/version "3.0.1"}
        org.apache.poi/poi-ooxml {:mvn/version "5.2.5"}}}
```

### Features to Implement

1. **Main Window**
   - System tray icon with status indicator
   - Watched folders list with add/remove
   - Recent analyses list
   - Real-time analysis progress

2. **Folder Watcher** (Java NIO WatchService)
   ```clojure
   ;; Use src/mental_models/io/file_ingestion.clj as base
   ;; Implement recursive folder watching
   ;; Support: .txt, .md, .pdf, .docx, .csv, .json
   ```

3. **File Ingestion**
   - Text extraction from all supported formats
   - Chunking for large files (>10KB chunks)
   - Metadata extraction (filename, date, size)

4. **Analysis Integration**
   - Use `src/mental_models/models/unified_detector.clj`
   - Run all 200+ model detectors on ingested text
   - Store results locally and sync to web app

5. **Electric Sync**
   - Real-time sync with web app
   - Push analysis results to backend
   - Pull model updates from backend

### File Structure

```
src/mental_models/desktop/
├── core.clj          ;; Main entry point
├── ui/
│   ├── main_window.clj
│   ├── tray.clj
│   ├── settings.clj
│   └── analysis_view.clj
├── watcher/
│   ├── folder_watcher.clj
│   └── file_processor.clj
└── sync/
    └── electric_client.clj
```

### Native Packaging

```bash
# Use jpackage for native installers
jpackage --name "Mental Models" \
  --input target \
  --main-jar mental-models.jar \
  --main-class mental_models.desktop.core \
  --type msi  # or dmg for Mac, deb for Linux
```

---

## TASK 2: Complete Mental Model Algorithms

Expand the detection algorithms in `src/mental_models/models/` to cover ALL mental models.

### Current State

- `algorithms.clj` - 15 base models implemented
- `algorithms_extended.clj` - 15 additional models
- `unified_detector.clj` - Orchestration engine

### Required: Implement Remaining ~170 Models

Each model needs:

```clojure
(defn detect-[model-name]
  "Detect [Model Name] in text.
   
   Pattern: [description of what to look for]
   Keywords: [list of keywords]
   
   Returns {:score 0.0-1.0
            :evidence [extracted quotes]
            :confidence :high/:moderate/:low}"
  [text]
  (let [keywords [...]
        patterns [...]
        keyword-score (calculate-keyword-density text keywords)
        pattern-score (calculate-pattern-matches text patterns)
        semantic-score (llm-semantic-analysis text model-prompt)]
    {:model-id :model-name
     :score (weighted-average [keyword-score pattern-score semantic-score]
                              [0.3 0.3 0.4])
     :evidence (extract-evidence text patterns)
     :confidence (score->confidence score)}))
```

### Models to Implement by Category

**Mathematics (expand to 25 models)**
- Compound Interest, Probability Theory, Permutations/Combinations
- Law of Large Numbers, Regression to Mean, Normal Distribution
- Bayes' Theorem, Expected Value, Variance/Standard Deviation
- Game Theory basics, Decision Trees, Monte Carlo methods

**Physics (expand to 20 models)**
- Newton's Laws, Thermodynamics (all 4 laws), Entropy
- Critical Mass, Tipping Points, Equilibrium
- Momentum, Inertia, Friction, Leverage
- Relativity (time dilation in decisions), Quantum uncertainty

**Chemistry (expand to 15 models)**
- Catalysts, Activation Energy, Chemical Equilibrium
- Autocatalysis, Chain Reactions, Saturation
- Bonding (weak vs strong ties), Dissolution, Precipitation

**Biology (expand to 25 models)**
- Natural Selection, Adaptation, Mutation
- Red Queen Effect, Niches, Symbiosis/Parasitism
- Homeostasis, Feedback Loops, Carrying Capacity
- Immune System (organizational), Cancer (uncontrolled growth)

**Psychology (expand to 40 models)**
- All cognitive biases from Kahneman/Tversky
- Social proof, Authority, Scarcity, Reciprocity, Commitment
- Narrative fallacy, Hindsight bias, Outcome bias
- Stress response, Habit formation, Addiction patterns

**Economics (expand to 30 models)**
- Supply/Demand, Elasticity, Marginal Utility
- Opportunity Cost, Comparative Advantage, Specialization
- Incentives (all types), Principal-Agent, Moral Hazard
- Network Effects, Economies of Scale, Switching Costs

**Engineering (expand to 20 models)**
- Redundancy, Margin of Safety, Fail-safes
- Bottlenecks, Critical Path, Dependencies
- Feedback Control, PID loops, Damping
- Modularity, Interfaces, Abstraction layers

### Algorithm Template

```clojure
;; src/mental_models/models/psychology_models.clj

(ns mental-models.models.psychology-models
  (:require [mental-models.models.detection :as d]
            [mental-models.services.lm-studio :as llm]))

(def confirmation-bias-prompt
  "Analyze this text for confirmation bias - the tendency to search for, 
   interpret, favor, and recall information that confirms prior beliefs.
   
   Look for:
   - Selective attention to supporting evidence
   - Dismissal of contradicting evidence
   - Interpretation of ambiguous info as supportive
   - Memory favoring confirming experiences
   
   Score 0.0-1.0 based on how strongly this bias appears.")

(defn detect-confirmation-bias [text]
  (d/detect-model
    {:id :confirmation-bias
     :name "Confirmation Bias"
     :category :psychology
     :keywords ["confirms" "supports my view" "as I expected" "proves"
                "validates" "consistent with" "aligns with" "reinforces"]
     :anti-keywords ["contradicts" "challenges" "disproves" "contrary to"]
     :patterns [#"(?i)this (proves|shows|confirms) (that |what )?(I|we) (thought|believed|expected)"
                #"(?i)just as (I|we) (predicted|expected|thought)"
                #"(?i)(ignore|dismiss|disregard).*evidence"]
     :llm-prompt confirmation-bias-prompt
     :weights {:keyword 0.25 :pattern 0.25 :semantic 0.5}}
    text))

;; Implement ALL psychology models following this pattern
```

---

## TASK 3: LM Studio Integration

The system uses LM Studio for semantic analysis. Implement proper integration.

### Current: `src/mental_models/services/analyzer.clj`

Expand to support:

```clojure
(ns mental-models.services.lm-studio
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(def lm-studio-url "http://localhost:1234/v1/chat/completions")

(defn analyze-with-model
  "Send text to LM Studio for analysis with specific model prompt.
   
   Returns {:score float :reasoning string :evidence [strings]}"
  [text model-prompt]
  (let [response (http/post lm-studio-url
                   {:body (json/generate-string
                            {:messages [{:role "system" 
                                        :content model-prompt}
                                       {:role "user"
                                        :content text}]
                             :temperature 0.3
                             :max_tokens 500})
                    :content-type :json
                    :as :json})]
    (parse-llm-response (:body response))))

(defn batch-analyze
  "Analyze text against multiple model prompts efficiently.
   Uses single LLM call with combined prompt for speed."
  [text model-prompts]
  ;; Implement batched analysis
  )
```

### Prompt Templates

Create `src/mental_models/prompts/` with templates for each model:

```clojure
;; src/mental_models/prompts/templates.clj

(def model-prompts
  {:confirmation-bias
   {:system "You are an expert in cognitive psychology..."
    :user-template "Analyze this text for confirmation bias:\n\n{{text}}\n\nScore 0.0-1.0:"}
   
   :anchoring
   {:system "You are an expert in behavioral economics..."
    :user-template "Analyze this text for anchoring bias:\n\n{{text}}\n\nScore 0.0-1.0:"}
   
   ;; ... 200+ more prompts
   })
```

---

## TASK 4: Database Expansion

Add remaining mental models to the database.

### Current: 167 models, 1480 failure modes

### Target: 200+ models, 2000+ failure modes

### SQL to run (via web app or direct):

```sql
-- Add new categories if needed
INSERT INTO categories (name, slug, description, color, icon) VALUES
('Systems Thinking', 'systems', 'Understanding complex systems', '#6366f1', 'network'),
('Decision Making', 'decisions', 'Frameworks for better decisions', '#f59e0b', 'target');

-- Add models (example batch)
INSERT INTO mental_models (name, slug, description, category_id, thinker, complexity) VALUES
('Compound Interest', 'compound-interest', 'Exponential growth through reinvestment', 
 (SELECT id FROM categories WHERE slug = 'mathematics'), 'Einstein', 2),
('Second-Order Effects', 'second-order-effects', 'Consequences of consequences',
 (SELECT id FROM categories WHERE slug = 'systems'), 'Munger', 4);

-- Add failure modes for each model (5 per model minimum)
INSERT INTO failure_modes (name, description, severity, mental_model_id) VALUES
('Ignoring time horizon', 'Underestimating long-term compound effects', 'high',
 (SELECT id FROM mental_models WHERE slug = 'compound-interest'));
```

---

## TASK 5: Electric Clojure Integration

Wire everything together with Electric's reactive model.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    CLJFX Desktop App                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │ Folder      │  │ File        │  │ Analysis            │ │
│  │ Watcher     │──│ Processor   │──│ Engine              │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
│         │                                    │              │
│         └────────────────┬───────────────────┘              │
│                          │                                  │
│                   Electric Sync                             │
└──────────────────────────┼──────────────────────────────────┘
                           │
                    ┌──────┴──────┐
                    │   Server    │
                    │  (Ring +    │
                    │  Electric)  │
                    └──────┬──────┘
                           │
              ┌────────────┼────────────┐
              │            │            │
        ┌─────┴─────┐ ┌────┴────┐ ┌────┴────┐
        │ Database  │ │ LM      │ │ Web     │
        │ (MySQL)   │ │ Studio  │ │ App     │
        └───────────┘ └─────────┘ └─────────┘
```

### Electric Components

```clojure
;; src/mental_models/electric/analysis.cljc

(ns mental-models.electric.analysis
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(e/defn AnalysisStream [text-atom]
  (e/server
    (let [text (e/watch text-atom)
          results (analyze-all-models text)]
      (e/client
        (dom/div
          (e/for [result results]
            (ModelResult. result)))))))

(e/defn ModelResult [{:keys [model-id score evidence]}]
  (dom/div (dom/props {:class "model-result"})
    (dom/span (dom/text (name model-id)))
    (dom/span (dom/text (str (* 100 score) "%")))
    (dom/ul
      (e/for [e evidence]
        (dom/li (dom/text e))))))
```

---

## DELIVERABLES CHECKLIST

### Desktop App
- [ ] CLJFX project setup with deps.edn
- [ ] Main window with system tray
- [ ] Folder watcher (recursive, all file types)
- [ ] File ingestion (txt, md, pdf, docx)
- [ ] Analysis integration with unified_detector
- [ ] Electric sync with backend
- [ ] Native installer (Windows .msi, Mac .dmg)

### Algorithms
- [ ] Mathematics models (25 total)
- [ ] Physics models (20 total)
- [ ] Chemistry models (15 total)
- [ ] Biology models (25 total)
- [ ] Psychology models (40 total)
- [ ] Economics models (30 total)
- [ ] Engineering models (20 total)
- [ ] Accounting models (15 total)
- [ ] Physiology models (15 total)
- [ ] All models have keyword detection
- [ ] All models have regex patterns
- [ ] All models have LLM prompts
- [ ] Scoring system validated

### Database
- [ ] 200+ mental models in database
- [ ] 2000+ failure modes
- [ ] All categories complete
- [ ] Descriptions are substantive (not placeholder)

### Integration
- [ ] Desktop → Server sync working
- [ ] Server → Web app sync working
- [ ] LM Studio integration tested
- [ ] End-to-end pipeline validated

---

## IMPORTANT NOTES

1. **No Python** - All new code must be Clojure/ClojureScript
2. **Munger's Hierarchy** - Organize everything by epistemological foundation
3. **Real Data Only** - No fake/mock data anywhere
4. **Clickable Numbers** - Every metric must be traceable to source
5. **Electric First** - Use Electric's reactive model for all state

---

## GETTING STARTED

```bash
# Clone repo
git clone https://github.com/Ripple-Analytics/Ripple_Analytics.git
cd Ripple_Analytics

# Install Clojure deps
clj -P

# Run existing backend
clj -M:dev

# Start LM Studio (separate terminal)
# Load a model like Llama 3 or Mistral

# Run tests
clj -M:test
```

---

## QUESTIONS?

Refer to existing code in:
- `src/mental_models/models/` - Algorithm patterns
- `src/mental_models/analysis/` - Analysis engines
- `src/mental_models/services/` - LM Studio integration
- `src/mental_models/io/` - File ingestion

The patterns are established. Scale them up.
