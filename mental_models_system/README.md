# Mental Models System

**The Oligarch's Operating System** - A comprehensive, industrial-scale mental models framework integrating wisdom from Munger, Soros, Dalio, Simons, Franklin, Seneca, Lee Kuan Yew, Rockefeller, and 15+ other historical thinkers.

Built for 100 years. Planck knowledge, not chauffeur knowledge.

## Project Statistics

| Metric | Value |
|--------|-------|
| Total Lines of Code | 86,051 |
| Python Code | 60,000+ lines |
| Swift Code | 4,000+ lines |
| JSON Data | 22,395 lines |
| HTML/Templates | 1,200+ lines |
| YAML/Config | 1,957 lines |
| Total Files | 220+ |
| Mental Models | 129 |
| Failure Modes | 645 |
| Connectors | 11 |
| Task Handlers | 9 |
| Statistical Analysis Functions | 15+ |
| iOS/watchOS Views | 20+ |

**Last Updated:** January 18, 2026 08:18 UTC

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    MENTAL MODELS SYSTEM                      │
├─────────────────────────────────────────────────────────────┤
│  PostgreSQL          │  PySpark              │  Excel/API   │
│  ─────────────────   │  ─────────────────    │  ──────────  │
│  • 1.2M+ Case Studies│  • Monte Carlo Sims   │  • Reports   │
│  • 113 Mental Models │  • Bayesian Inference │  • Dashboards│
│  • 264 Principles    │  • HMM Regime Detect  │  • Exports   │
│  • Planck Matrix     │  • Lollapalooza Calc  │  • API       │
└─────────────────────────────────────────────────────────────┘
```

## Core Principles (Built Into Design)

### Elon Musk's 5-Step Algorithm
1. **Question every requirement** - Each requirement has a named owner
2. **Delete** - If you don't add back 10%, you didn't delete enough
3. **Simplify and optimize** - Only after deletion
4. **Accelerate cycle time** - Speed up everything
5. **Automate** - Only after steps 1-4

### Munger's Latticework
- Mental models from multiple disciplines form a latticework
- Models must be used in combination, not isolation
- Two-track analysis: rational + psychological
- Invert problems - consider what to avoid

### Development Principle
> "Development is a function of iteration speed and how big each iteration is. If we win at both, we win."

## Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Set up PostgreSQL database
python src/database/setup.py

# Generate case studies
python src/database/populate.py

# Run PySpark analysis
python src/spark/analysis.py

# Export to Excel
python src/export/excel_builder.py

# Run statistical analysis
python src/analysis/statistical.py

# Start API server
python src/api/main.py
```

## API Endpoints

The system includes a FastAPI REST API for querying the database:

| Endpoint | Description |
|----------|-------------|
| `GET /stats` | System statistics (model count, case count, etc.) |
| `GET /frameworks` | List all thinkers/frameworks |
| `GET /models` | Query mental models (filter by category, search) |
| `GET /models/categories` | List model categories with counts |
| `GET /principles` | Query thinker principles (filter by thinker) |
| `GET /principles/thinkers` | List thinkers with principle counts |
| `GET /cases` | Query case studies (filter by category, region, severity) |
| `GET /lollapalooza` | Get cases with multiple interacting models |
| `GET /analysis/decade` | Decade-by-decade analysis |
| `GET /analysis/model-frequency` | Model frequency from Planck matrix |
| `GET /dashboard` | Web dashboard UI |
| `POST /failure-modes/check` | Check content for failure modes |
| `GET /failure-modes/stats` | Failure mode statistics |

### Huggingface Integration

| Endpoint | Description |
|----------|-------------|
| `POST /huggingface/embeddings` | Generate embeddings using Huggingface models |
| `POST /huggingface/classify` | Zero-shot text classification |
| `POST /huggingface/summarize` | Text summarization |
| `POST /huggingface/classify-mental-models` | Classify by mental model categories |
| `POST /huggingface/detect-biases` | Detect cognitive biases |
| `GET /huggingface/search-models` | Search Huggingface Hub |

### Statistical Analysis

| Endpoint | Description |
|----------|-------------|
| `POST /statistics/synthesize` | Multi-variable synthesis with full statistical analysis |
| `POST /statistics/correlations` | Calculate correlation matrix (Pearson/Spearman) |
| `POST /statistics/regression` | Simple and multiple regression analysis |
| `POST /statistics/covariates` | Analyze covariate effects on target variable |
| `POST /statistics/factor-analysis` | Factor analysis with eigenvalues and loadings |
| `POST /statistics/covariance-matrix` | Calculate covariance matrix |
| `GET /statistics/descriptive/{var}` | Descriptive statistics for a variable |
| `POST /statistics/mental-model-correlations` | Cross-model correlation analysis |

## iOS/watchOS Apps

Native Apple apps with Steve Jobs-level design for continuous learning:

### iOS App Features
- SwiftUI dashboard with real-time learning stats
- Sensor data collection (accelerometer, gyroscope, GPS, HealthKit)
- Background learning with BGTaskScheduler
- 129 mental models browser with category filtering
- Document analysis with bias detection
- Insights feed with pattern discovery
- iOS Shortcuts integration for automation

### watchOS App Features
- Glanceable learning status
- Health data monitoring (heart rate, steps, calories)
- Quick access to top mental models
- Continuous background learning
- Phone sync via WatchConnectivity

## Continuous Learning Pipeline

24/7 desktop service that learns and improves automatically:

- **Web Scrapers**: Headless Selenium scrapers for mental models content
- **Pattern Detection**: Automatic pattern and anomaly detection
- **Insight Generation**: Mental model keyword matching and correlation
- **SQLite Persistence**: Resumable processing with checkpoint/resume
- **Callback System**: Notify on new insights and patterns

## AI Agent Integration

Automated improvement cycles with Devin and Manus:

- **AutomatedImprovementCycle**: Coordinate multiple AI agents
- **ImprovementRequest/Result**: Track improvement lifecycle
- **Agent Selection**: Route tasks to appropriate agent by type
- **Learning Integration**: Generate improvements from insights

## Connectors

The system includes 10 connectors for integrating with external services:

| Connector | Description |
|-----------|-------------|
| GitHub | Repository, PR, and issue integration |
| Slack | Chat and command integration |
| Google Drive | Document storage and retrieval |
| Database | PostgreSQL, MySQL, SQLite support |
| Web Scraper | BeautifulSoup, Scrapy, Selenium (headless) |
| Local Files | Local filesystem access |
| REST API | Generic REST/GraphQL API connector |
| LM Studio | Local LLM inference |
| Zapier | Automation workflows via webhooks |
| Huggingface | Embeddings, classification, summarization |

## Failure Modes

The system documents 645 failure modes (5 per mental model) with detection signals and prevention strategies. Categories include:

- Data Bias
- Reasoning Error
- Incomplete Analysis
- Overconfidence
- Context Blindness
- Temporal Error
- Scale Mismatch
- Feedback Loop
- Edge Case
- Integration Failure

## Running Tests

```bash
# Run all tests
python -m pytest tests/

# Run specific test file
python -m pytest tests/test_database.py
python -m pytest tests/test_analysis.py
```

## Directory Structure

```
mental-models-system/
├── src/
│   ├── database/       # PostgreSQL schemas, migrations, population
│   ├── spark/          # PySpark analysis engines
│   ├── analysis/       # Statistical analysis tools
│   └── export/         # Excel and report generation
├── data/
│   ├── raw/            # Source data files
│   └── processed/      # Processed outputs
├── config/             # Configuration files
├── tests/              # Test suite
└── docs/               # Documentation
```

## Thinkers Integrated

| Category | Thinkers |
|----------|----------|
| Investment | Munger, Soros, Dalio, Buffett, Simons |
| Historical | Rockefeller, Getty, Rothschild, Crassus |
| Modern | Musk, Thiel, Koch |
| Statesmen | Franklin, Lee Kuan Yew |
| Philosophers | Seneca, Polya, Smil |

## License

MIT License - Built as a 100-year heirloom.
