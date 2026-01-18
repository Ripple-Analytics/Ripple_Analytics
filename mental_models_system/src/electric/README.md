# Mental Models System - Electric Clojure

A comprehensive mental models analysis system built with Electric Clojure for reactive full-stack development.

## Why Electric Clojure?

Electric Clojure was chosen for this implementation because:

1. **Unified Codebase** - Frontend and backend code in the same files (.cljc)
2. **Reactive by Default** - UI automatically updates when data changes
3. **No API Boilerplate** - Direct function calls between client and server
4. **Full Lisp Power** - Macros and expressiveness on both ends
5. **Differential Sync** - Only changes are sent over the wire

## Project Structure

```
src/electric/
├── deps.edn                              # Project dependencies
├── README.md                             # This file
├── resources/public/                     # Static assets
└── src/mental_models/
    ├── main.clj                          # Server entry point
    ├── models.cljc                       # Mental models library (shared)
    ├── analysis.cljc                     # Analysis engine (shared)
    ├── statistics.cljc                   # Statistical functions (shared)
    ├── data_processing.cljc              # Text processing (shared)
    ├── tech_debt.cljc                    # Tech Debt Eliminator (shared)
    └── ui.cljc                           # Reactive UI components (shared)
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
cd mental_models_system/src/electric
clj -M:prod
```

The server will start on port 8000. Open http://localhost:8000 in your browser.

### Development Mode

```bash
cd mental_models_system/src/electric
clj -M:dev
```

## Features

### Dashboard
- Real-time metrics display
- Category breakdown with coverage bars
- Quick analysis panel
- Bias detection

### Models Browser
- Search and filter 129 mental models
- Detailed view with failure modes
- Category grouping

### Analysis Tools
- **Latticework Analysis** - Apply multiple models to a problem
- **Lollapalooza Detection** - Find reinforcing model effects
- **Inversion** - Think about how to fail
- **Two-Track Analysis** - Rational + psychological factors
- **Bias Detection** - Identify cognitive biases in text

### Statistics
- Correlation analysis (Pearson, Spearman)
- Descriptive statistics
- Regression analysis
- Hypothesis testing

### Data Processing
- Document analysis
- Entity extraction
- Mental model classification
- Readability scoring

### Tech Debt Eliminator
- **DAG Analysis** - Encode codebase as directed acyclic graph
- **Tangle Detection** - Find strongly connected components (cycles)
- **Coupling Metrics** - Identify high fan-in/fan-out nodes
- **Refactoring Plans** - Prioritized steps to reduce tech debt
- **LLM Integration** - AI-powered refactoring suggestions
- **Visualization** - D3.js-compatible DAG export

## API Endpoints

### Models
- `GET /api/models` - Get all models and metadata

### Analysis
- `POST /api/analysis/latticework` - Latticework analysis
- `POST /api/analysis/lollapalooza` - Lollapalooza detection
- `POST /api/analysis/inversion` - Inversion analysis
- `POST /api/analysis/two-track` - Two-track analysis
- `POST /api/analysis/bias-detection` - Detect cognitive biases

### Statistics
- `POST /api/statistics/correlation` - Correlation analysis

### Data
- `POST /api/data/analyze` - Document analysis

### Tech Debt Eliminator
- `POST /api/techdebt/analyze` - Analyze code DAG for tech debt
- `POST /api/techdebt/tangles` - Detect tangled components (cycles, high coupling)
- `POST /api/techdebt/plan` - Generate prioritized refactoring plan
- `POST /api/techdebt/llm-refactor` - Get LLM-powered refactoring suggestions
- `POST /api/techdebt/visualize` - Export DAG for D3.js visualization

### LLM (LM Studio Integration)
- `GET /api/llm/status` - Check LM Studio connection status
- `POST /api/llm/analyze` - Analyze situation with LLM + mental models
- `POST /api/llm/biases` - Detect biases using LLM
- `POST /api/llm/checklist` - Generate decision checklist with LLM
- `POST /api/llm/classify` - Classify document by mental models with LLM

**LM Studio Configuration:**
```bash
# Set environment variables (optional, defaults shown)
export LM_STUDIO_URL=http://localhost:1234
export LM_STUDIO_MODEL=local-model
```

## Mental Models Included

The system includes **129 mental models** with **645 failure modes** across 34 categories:

- **Decision Making**: Circle of Competence, Margin of Safety, Second-Order Thinking, Inversion, Opportunity Cost
- **Psychology**: Incentives, Social Proof, Commitment/Consistency, Availability Heuristic, Loss Aversion, Confirmation Bias, Hindsight Bias, Dunning-Kruger, Status Quo Bias, Narrative Fallacy
- **Systems**: Feedback Loops, Emergence, Network Effects
- **Economics**: Supply/Demand, Comparative Advantage, Compound Interest
- **Physics/Biology**: Critical Mass, Evolution, Red Queen
- **Mathematics**: Bayes Theorem, Regression to Mean, Power Laws, Normal Distribution, Expected Value
- **Strategy**: Competitive Advantage, Moats, First-Mover Advantage, Game Theory, Optionality
- **Communication**: Hanlon's Razor, Occam's Razor, Map-Territory
- **Learning**: Deliberate Practice, First Principles, Mental Models Meta
- **Productivity**: Leverage, Parkinson's Law, Eisenhower Matrix
- **Risk**: Black Swan, Antifragility, Fat Tails
- **Innovation**: Creative Destruction, S-Curves, Innovator's Dilemma
- **Human Nature**: Reciprocity, Scarcity, Authority
- **Finance**: Time Value of Money, Asymmetric Information, Sunk Costs
- **Negotiation**: BATNA, Anchoring, Win-Win
- **Operations**: Bottleneck, Redundancy, Queuing Theory
- **Investing**: Mr. Market, Circle of Competence, Margin of Safety
- **Science**: Falsifiability, Replication, Survivorship Bias
- **Technology**: Moore's Law, Network Topology, Technical Debt
- **Ecology**: Carrying Capacity, Niche
- **Philosophy**: Stoicism, Via Negativa, Skin in the Game
- **Military**: Fog of War, Force Multiplier, Schwerpunkt
- **Design**: Form Follows Function, Affordances, Constraints
- **Management**: Principal-Agent, Span of Control, Peter Principle
- **History**: Lindy Effect, Chesterton's Fence, Path Dependence
- **Complexity**: Cynefin, Tight Coupling, Normal Accidents
- **Behavioral Economics**: Prospect Theory, Hyperbolic Discounting, Mental Accounting
- **Cognitive Science**: Cognitive Load, Dual Process Theory, Attention Economy
- **Systems Dynamics**: Stocks and Flows, Leverage Points, System Archetypes
- **Decision Theory**: Regret Minimization, Reversibility, Satisficing
- **Information Theory**: Signal vs Noise, Information Asymmetry
- **Game Theory**: Nash Equilibrium, Mechanism Design, Coordination Games
- **Epistemology**: Epistemic Humility, Bayesian Updating, Falsificationism
- **Organizational**: Conway's Law, Goodhart's Law, Parkinson's Law (Bureaucracy)
- **Evolution**: Fitness Landscape, Punctuated Equilibrium
- **Network Science**: Small World Networks, Preferential Attachment
- **Rhetoric**: Ethos-Pathos-Logos, Steelmanning

Each model includes:
- Description and key insight
- Application guidance
- 5 failure modes with signals and safeguards

**Last updated**: January 18, 2026 09:44 UTC
**Lines of code**: 6,500+ (Electric Clojure modules)

## Design Philosophy

Following Steve Jobs-level attention to detail:
- **Information Density** - Value Line-style compact displays
- **Instant Feedback** - Reactive updates without page reloads
- **Clean Typography** - 11px base font, tight line height
- **Minimal Chrome** - Focus on content, not decoration

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Electric Clojure                      │
│  ┌─────────────────────────────────────────────────┐   │
│  │              Shared Code (.cljc)                 │   │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐           │   │
│  │  │ Models  │ │Analysis │ │  Stats  │           │   │
│  │  └─────────┘ └─────────┘ └─────────┘           │   │
│  │  ┌─────────┐ ┌─────────┐                       │   │
│  │  │  Data   │ │   UI    │                       │   │
│  │  └─────────┘ └─────────┘                       │   │
│  └─────────────────────────────────────────────────┘   │
│                         │                               │
│         ┌───────────────┴───────────────┐              │
│         ▼                               ▼              │
│  ┌─────────────┐                 ┌─────────────┐      │
│  │   Server    │ ◄──Reactive──► │   Client    │      │
│  │  (JVM CLJ)  │                 │ (Browser)   │      │
│  └─────────────┘                 └─────────────┘      │
└─────────────────────────────────────────────────────────┘
```

## License

MIT License - See LICENSE file for details.
