# Mental Models System - Clojure Implementation

A comprehensive mental models analysis system built in idiomatic Clojure for maximum expressiveness and rapid feature development.

## Why Clojure?

Clojure was chosen for this implementation because:

1. **Lisp Power** - Macros, homoiconicity, and REPL-driven development enable rapid iteration
2. **Immutable Data** - Persistent data structures make reasoning about state trivial
3. **Concurrency** - Built-in support for concurrent programming (atoms, refs, agents)
4. **JVM Ecosystem** - Access to all Java libraries and excellent tooling
5. **Expressiveness** - Write less code that does more

## Project Structure

```
src/clojure/
├── deps.edn                           # Project dependencies
├── README.md                          # This file
└── src/mental_models/
    ├── core.clj                       # Web server (Ring/Compojure)
    ├── models.clj                     # Mental models library (129 models)
    ├── analysis.clj                   # Analysis engine
    ├── statistics.clj                 # Statistical functions
    └── data_processing.clj            # Text processing
```

## Quick Start

### Prerequisites

- Java 11+ (JDK)
- Clojure CLI tools

### Install Clojure CLI

```bash
# macOS
brew install clojure/tools/clojure

# Linux
curl -O https://download.clojure.org/install/linux-install-1.11.1.1435.sh
chmod +x linux-install-1.11.1.1435.sh
sudo ./linux-install-1.11.1.1435.sh
```

### Run the Server

```bash
cd mental_models_system/src/clojure
clj -M:run
```

The server will start on port 8000.

### REPL Development

```bash
cd mental_models_system/src/clojure
clj -M:dev
```

## API Endpoints

### Health Check
- `GET /` - Health check
- `GET /health` - Health check

### Models
- `GET /api/models` - List all models
- `GET /api/models/:name` - Get specific model
- `GET /api/models/category/:category` - Get models by category
- `GET /api/models/search?q=query` - Search models

### Analysis
- `POST /api/analysis/comprehensive` - Full analysis
- `POST /api/analysis/latticework` - Latticework analysis
- `POST /api/analysis/lollapalooza` - Lollapalooza detection
- `POST /api/analysis/inversion` - Inversion analysis
- `POST /api/analysis/two-track` - Two-track analysis
- `POST /api/analysis/bias-detection` - Detect cognitive biases
- `POST /api/analysis/decision-checklist` - Generate checklist

### Statistics
- `POST /api/statistics/descriptive` - Descriptive stats
- `POST /api/statistics/correlation` - Correlation analysis
- `POST /api/statistics/regression` - Linear regression
- `POST /api/statistics/comprehensive` - Full statistical analysis

### Data Processing
- `POST /api/data/analyze` - Analyze document
- `POST /api/data/chunk` - Chunk text
- `POST /api/data/entities` - Extract entities
- `POST /api/data/readability` - Readability score
- `POST /api/data/classify` - Classify by mental models
- `POST /api/data/process` - Full processing pipeline

## Example Usage

### Analyze with Mental Models

```bash
curl -X POST http://localhost:8000/api/analysis/comprehensive \
  -H "Content-Type: application/json" \
  -d '{"context": {"situation": "Considering a major investment"}, "models": ["margin-of-safety", "circle-of-competence"]}'
```

### Detect Lollapalooza Effects

```bash
curl -X POST http://localhost:8000/api/analysis/lollapalooza \
  -H "Content-Type: application/json" \
  -d '{"context": {"situation": "Multiple biases converging"}, "models": []}'
```

### Statistical Analysis

```bash
curl -X POST http://localhost:8000/api/statistics/correlation \
  -H "Content-Type: application/json" \
  -d '{"x": [1, 2, 3, 4, 5], "y": [2, 4, 5, 4, 5]}'
```

### Process Document

```bash
curl -X POST http://localhost:8000/api/data/process \
  -H "Content-Type: application/json" \
  -d '{"text": "Your document text here...", "chunk_size": 1000, "overlap": 100}'
```

## Mental Models Included

The system includes 129 mental models across categories:

- **Decision Making**: Circle of Competence, Margin of Safety, Second-Order Thinking, Inversion, Opportunity Cost
- **Psychology**: Incentives, Social Proof, Commitment/Consistency, Availability Heuristic, Loss Aversion
- **Systems**: Feedback Loops, Emergence, Network Effects
- **Economics**: Supply/Demand, Comparative Advantage, Compound Interest
- **Physics/Biology**: Critical Mass, Evolution, Red Queen

Each model includes:
- Description and key insight
- Application guidance
- 5 failure modes with signals and safeguards

## Development

### Run Tests

```bash
clj -M:test
```

### Build Uberjar

```bash
clj -T:build uber
```

## License

MIT License - See LICENSE file for details.
