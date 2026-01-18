# Mental Models System

**The Oligarch's Operating System** - A comprehensive, industrial-scale mental models framework integrating wisdom from Munger, Soros, Dalio, Simons, Franklin, Seneca, Lee Kuan Yew, Rockefeller, and 15+ other historical thinkers.

Built for 100 years. Planck knowledge, not chauffeur knowledge.

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
