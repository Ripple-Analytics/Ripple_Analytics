# Mental Models System Architecture

**The Oligarch's Operating System** - A comprehensive, industrial-scale mental models framework integrating wisdom from Munger, Soros, Dalio, Simons, Franklin, Seneca, Lee Kuan Yew, Rockefeller, and 15+ other historical thinkers.

Built for 100 years. Planck knowledge, not chauffeur knowledge.

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Core Principles](#core-principles)
3. [Architecture Components](#architecture-components)
4. [Data Model](#data-model)
5. [Analysis Pipeline](#analysis-pipeline)
6. [API Design](#api-design)
7. [Dashboard](#dashboard)
8. [Deployment](#deployment)
9. [Future Enhancements](#future-enhancements)

---

## System Overview

The Mental Models System is a decision-making framework that codifies wisdom from history's greatest thinkers into a queryable, analyzable database. It enables users to:

- **Query mental models** by category, originator, or application context
- **Analyze case studies** where models were applied (or should have been)
- **Detect patterns** using statistical and machine learning techniques
- **Identify Lollapalooza effects** where multiple models combine multiplicatively
- **Predict regime changes** in decision-making contexts using HMM
- **Compare model effectiveness** using Bayesian inference

### Key Metrics

- **113 Mental Models** across multiple disciplines
- **264 Principles** from 16+ legendary thinkers
- **100K+ Case Studies** (target) with deep Planck knowledge
- **Planck Matrix** mapping models to cases with effect sizes

---

## Core Principles

### 1. Elon Musk's 5-Step Algorithm

Applied to every component:

1. **Question every requirement** - Each requirement has a named owner
2. **Delete** - If you don't add back 10%, you didn't delete enough
3. **Simplify and optimize** - Only after deletion
4. **Accelerate cycle time** - Speed up everything
5. **Automate** - Only after steps 1-4

### 2. Munger's Latticework

- Mental models from multiple disciplines form a latticework
- Models must be used in combination, not isolation
- Two-track analysis: rational + psychological
- Invert problems - consider what to avoid

### 3. Planck Knowledge vs Chauffeur Knowledge

**Planck Knowledge** (Deep Understanding):
- Understand WHY models work, not just THAT they work
- Know the underlying mechanisms and first principles
- Can apply models to novel situations
- Recognize when models don't apply

**Chauffeur Knowledge** (Superficial):
- Memorize model names and definitions
- Apply models mechanically without understanding
- Cannot adapt to new contexts
- Dangerous because it appears knowledgeable

**This system prioritizes Planck knowledge in every case study and analysis.**

### 4. Development Principle

> "Development is a function of iteration speed and how big each iteration is. If we win at both, we win."

- Optimize for both rapid iteration AND significant improvements per iteration
- Don't sacrifice quality for speed or vice versa
- Continuous improvement mindset

### 5. Jim Simons' Principles

From Renaissance Technologies:

- **Data-driven decisions** - "We don't predict. We react to what the data tells us."
- **Probabilistic reasoning** - Use Bayesian inference, not deterministic predictions
- **Regime detection** - Markets (and decisions) have different regimes with different rules
- **Systematic approach** - Remove human bias through systematic analysis

---

## Architecture Components

```
┌─────────────────────────────────────────────────────────────────────┐
│                      MENTAL MODELS SYSTEM                            │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐    │
│  │   PostgreSQL    │  │    PySpark      │  │  FastAPI + Dash │    │
│  │   Database      │  │    Analysis     │  │  Interface      │    │
│  ├─────────────────┤  ├─────────────────┤  ├─────────────────┤    │
│  │ • Mental Models │  │ • Monte Carlo   │  │ • REST API      │    │
│  │ • Principles    │  │ • Bayesian      │  │ • Dashboard     │    │
│  │ • Case Studies  │  │ • HMM Regimes   │  │ • Excel Export  │    │
│  │ • Planck Matrix │  │ • Lollapalooza  │  │ • Reports       │    │
│  │ • Frameworks    │  │ • Statistical   │  │                 │    │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘    │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

### Component Details

#### 1. PostgreSQL Database

**Purpose**: Persistent storage of all mental models, principles, case studies, and relationships.

**Tables**:
- `frameworks` - Thinkers/frameworks (Munger, Dalio, etc.)
- `mental_models` - Mental models with metadata
- `thinker_principles` - Principles from each thinker
- `case_studies` - Historical cases with outcomes
- `planck_matrix` - Many-to-many mapping of models to cases with effect sizes

**Why PostgreSQL**:
- ACID compliance for data integrity
- Rich query capabilities (joins, aggregations)
- JSON support for flexible metadata
- Mature ecosystem and tooling
- Open source and battle-tested

#### 2. PySpark Analysis Engine

**Purpose**: Large-scale data analysis and pattern detection.

**Capabilities**:
- **Monte Carlo Simulations** - Probabilistic outcome modeling
- **Bayesian Inference** - Model effectiveness and A/B testing
- **HMM Regime Detection** - Identify decision-making regimes
- **Lollapalooza Calculation** - Detect multiplicative model interactions
- **Statistical Analysis** - Confidence intervals, hypothesis testing

**Why PySpark**:
- Scales to billions of case studies
- Distributed computing for complex analyses
- Rich ML library (MLlib)
- Integrates with PostgreSQL via JDBC

#### 3. FastAPI REST API

**Purpose**: Programmatic access to system data and analyses.

**Endpoints**:
- `/stats` - System statistics
- `/frameworks` - List thinkers/frameworks
- `/models` - Query mental models
- `/principles` - Query principles
- `/cases` - Query case studies
- `/lollapalooza` - High-interaction cases
- `/analysis/*` - Various analyses

**Why FastAPI**:
- Modern Python async framework
- Automatic OpenAPI documentation
- Type validation with Pydantic
- High performance
- Easy to extend

#### 4. Plotly Dash Dashboard

**Purpose**: Interactive visualization and exploration.

**Features**:
- System overview with key metrics
- Mental models browser with filtering
- Case studies explorer
- Lollapalooza effect visualization
- Regime transition timeline
- Model effectiveness rankings

**Why Dash**:
- Python-native (no JavaScript required)
- Rich Plotly visualizations
- Reactive components
- Easy to deploy
- Bootstrap styling

#### 5. Excel Export

**Purpose**: Traditional reporting and offline analysis.

**Features**:
- Multi-sheet workbooks
- Charts and visualizations
- Formatted tables
- Executive summaries

---

## Data Model

### Entity-Relationship Diagram

```
┌─────────────────┐
│   frameworks    │
├─────────────────┤
│ id (PK)         │
│ name            │
│ description     │
│ source          │
└────────┬────────┘
         │
         │ 1:N
         │
┌────────▼────────────┐         ┌─────────────────┐
│ thinker_principles  │         │  mental_models  │
├─────────────────────┤         ├─────────────────┤
│ id (PK)             │         │ id (PK)         │
│ thinker             │         │ name            │
│ principle_name      │         │ category        │
│ principle_desc      │         │ description     │
│ category            │         │ originator      │
│ source              │         │ lindy_age_years │
└─────────────────────┘         └────────┬────────┘
                                         │
                                         │ N:M
                                         │
                                ┌────────▼────────┐
                                │ planck_matrix   │
                                ├─────────────────┤
                                │ id (PK)         │
                                │ case_id (FK)    │
                                │ model_name (FK) │
                                │ effect_size     │
                                │ application     │
                                └────────┬────────┘
                                         │
                                         │ N:1
                                         │
                                ┌────────▼────────┐
                                │  case_studies   │
                                ├─────────────────┤
                                │ id (PK)         │
                                │ name            │
                                │ date            │
                                │ category        │
                                │ region          │
                                │ severity        │
                                │ financial_impact│
                                │ models_involved │
                                │ lollapalooza_sc │
                                └─────────────────┘
```

### Key Design Decisions

1. **Planck Matrix as Bridge Table**
   - Enables many-to-many relationships
   - Stores effect_size for each model-case pair
   - Allows detailed application descriptions (Planck knowledge)

2. **Denormalized Case Studies**
   - `models_involved` and `lollapalooza_score` stored directly
   - Optimizes for read performance
   - Updated via triggers when Planck Matrix changes

3. **Flexible Metadata**
   - JSON columns for extensibility
   - Can add new attributes without schema changes
   - Balances structure with flexibility

---

## Analysis Pipeline

### 1. Monte Carlo Simulation

**Purpose**: Model probabilistic outcomes under uncertainty.

**Process**:
1. Define input distributions (severity, financial impact, etc.)
2. Run N simulations (default 10,000)
3. Calculate percentiles and confidence intervals
4. Identify tail risks

**Use Cases**:
- Estimate range of outcomes for decision
- Quantify uncertainty
- Stress testing

### 2. Bayesian Inference

**Purpose**: Update beliefs based on evidence.

**Process**:
1. Define prior distribution (weakly informative)
2. Observe data (model applications and outcomes)
3. Calculate posterior distribution
4. Derive credible intervals and Bayes factors

**Use Cases**:
- Rank model effectiveness
- A/B test between models
- Calculate Lollapalooza probability

### 3. HMM Regime Detection

**Purpose**: Identify distinct decision-making regimes.

**Process**:
1. Prepare features from case studies
2. Train Gaussian HMM with N regimes
3. Predict regime for each case
4. Detect regime transitions
5. Characterize each regime

**Regimes Detected**:
- **Stable Regime** - Normal conditions, low severity
- **Complex Decision Regime** - Many models involved
- **Crisis Regime** - High severity, rapid decisions
- **Lollapalooza Regime** - Multiple models interacting

**Use Cases**:
- Understand context-dependent model effectiveness
- Predict regime changes
- Adapt decision-making to current regime

### 4. Lollapalooza Detection

**Purpose**: Identify cases where multiple models combine multiplicatively.

**Formula**:
```
Lollapalooza Score = (Σ effect_sizes) × (models_involved / max_models) × interaction_factor
```

**Criteria**:
- Multiple models involved (≥3)
- High individual effect sizes
- Evidence of interaction (not just addition)
- Severe outcomes

**Use Cases**:
- Learn from extreme cases
- Identify dangerous combinations
- Predict multiplicative effects

---

## API Design

### RESTful Principles

- **Resource-oriented** - URLs represent resources, not actions
- **HTTP verbs** - GET for reads, POST for writes (future)
- **Stateless** - No session state on server
- **JSON responses** - Standard, parseable format
- **CORS enabled** - Cross-origin requests allowed

### Authentication (Future)

- API keys for programmatic access
- OAuth2 for user authentication
- Rate limiting to prevent abuse

### Versioning

- URL-based versioning: `/v1/models`, `/v2/models`
- Maintains backward compatibility
- Deprecation notices in headers

---

## Dashboard

### Design Philosophy

- **Dark theme** - Reduces eye strain for extended use
- **Information density** - Show maximum relevant data
- **Interactive filtering** - Users explore, don't just consume
- **Responsive** - Works on desktop, tablet, mobile

### Key Views

1. **Overview** - System stats, top models, decade analysis
2. **Mental Models** - Browse and filter models by category
3. **Case Studies** - Explore cases with severity and Lollapalooza filters
4. **Lollapalooza** - Deep dive into high-interaction cases
5. **Regimes** (Future) - Visualize regime transitions over time

---

## Deployment

### Development

```bash
# Install dependencies
pip install -r requirements.txt

# Set up database
python src/database/setup.py
python src/database/populate.py

# Run API
python src/api/main.py

# Run dashboard (separate terminal)
python src/dashboard/app.py
```

### Production (Future)

- **Docker containers** for reproducibility
- **Kubernetes** for orchestration and scaling
- **PostgreSQL RDS** for managed database
- **CloudFront** for CDN and caching
- **Monitoring** with Prometheus and Grafana

---

## Future Enhancements

### Phase 2: Advanced Features

1. **Natural Language Query**
   - Ask questions in plain English
   - GPT-4 integration for query understanding
   - Generate reports from queries

2. **Recommendation Engine**
   - Given a situation, recommend relevant models
   - Collaborative filtering based on similar cases
   - Explain why each model is relevant

3. **Scenario Simulation**
   - Input a hypothetical situation
   - Simulate outcomes under different model applications
   - Compare strategies

4. **Real-time Data Integration**
   - Ingest news and market data
   - Automatically classify into regimes
   - Alert on regime transitions

### Phase 3: Community Features

1. **User-submitted Cases**
   - Community can add case studies
   - Peer review process
   - Reputation system

2. **Model Discussions**
   - Forums for each mental model
   - Share applications and insights
   - Learn from others' experiences

3. **Personal Decision Journal**
   - Log your decisions
   - Tag with mental models used
   - Review outcomes and learn

### Phase 4: Enterprise Features

1. **Team Collaboration**
   - Shared decision journals
   - Team dashboards
   - Role-based access control

2. **Integration APIs**
   - Slack bot for quick queries
   - Notion integration for documentation
   - Zapier for workflow automation

3. **Custom Models**
   - Organizations can add proprietary models
   - Private case studies
   - Custom analyses

---

## Conclusion

The Mental Models System is built on timeless principles from history's greatest thinkers, implemented with modern technology for scalability and usability. It prioritizes Planck knowledge (deep understanding) over chauffeur knowledge (superficial facts), and is designed to be a 100-year asset that continuously improves decision-making.

**Key Differentiators**:

1. **Depth over breadth** - Each case study includes WHY models work, not just THAT they work
2. **Probabilistic reasoning** - Bayesian inference throughout, not deterministic predictions
3. **Regime awareness** - Recognizes that different contexts require different approaches
4. **Lollapalooza focus** - Emphasizes multiplicative model interactions
5. **Built for longevity** - Open source, standard technologies, clear documentation

This is not just a database of mental models. It's an operating system for decision-making.
