# Mental Models System - Usage Guide

This guide provides practical instructions for using the Mental Models System effectively.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Database Operations](#database-operations)
3. [Running Analyses](#running-analyses)
4. [Using the API](#using-the-api)
5. [Dashboard](#dashboard)
6. [Advanced Usage](#advanced-usage)
7. [Troubleshooting](#troubleshooting)

---

## Getting Started

### Prerequisites

- Python 3.11+
- PostgreSQL 12+
- 8GB+ RAM (for PySpark)
- 10GB+ disk space

### Installation

```bash
# Clone repository
git clone https://github.com/Joel-Kessels/Ripple__Algo__Stage-4__Machine-Learning-Platform.git
cd Ripple__Algo__Stage-4__Machine-Learning-Platform/mental_models_system

# Install dependencies
pip install -r requirements.txt

# Set up environment variables
cp .env.example .env
# Edit .env with your database credentials
```

### Environment Variables

Create a `.env` file:

```bash
# Database
DB_HOST=localhost
DB_PORT=5432
DB_NAME=mental_models
DB_USER=postgres
DB_PASSWORD=your_password

# API
API_HOST=0.0.0.0
API_PORT=8000
CORS_ORIGINS=*

# Dashboard
DASHBOARD_HOST=0.0.0.0
DASHBOARD_PORT=8050

# Analysis
MC_SIMULATIONS=10000
RANDOM_SEED=42
CONFIDENCE_LEVEL=0.95
MIN_LOLLAPALOOZA_MODELS=3
```

---

## Database Operations

### Initial Setup

```bash
# Create database schema
python src/database/setup.py

# Populate with mental models and principles
python src/database/populate.py

# Generate case studies
python src/database/case_study_generator.py
```

### Querying the Database

#### Using psql

```bash
psql -U postgres -d mental_models

# List all mental models
SELECT name, category FROM mental_models ORDER BY category;

# Find high-severity cases
SELECT name, severity, lollapalooza_score 
FROM case_studies 
WHERE severity > 0.8 
ORDER BY severity DESC;

# Get model frequency
SELECT model_name, COUNT(*) as count
FROM planck_matrix
GROUP BY model_name
ORDER BY count DESC
LIMIT 10;
```

#### Using Python

```python
import psycopg2
import pandas as pd

conn = psycopg2.connect(
    dbname="mental_models",
    user="postgres",
    host="localhost"
)

# Query mental models
df = pd.read_sql_query("SELECT * FROM mental_models", conn)
print(df.head())

conn.close()
```

---

## Running Analyses

### Monte Carlo Simulation

```bash
python src/analysis/statistical.py
```

**What it does**:
- Runs probabilistic simulations on case study outcomes
- Calculates confidence intervals
- Identifies tail risks

**Output**: `data/processed/monte_carlo_results.json`

### Bayesian Model Effectiveness

```bash
python src/analysis/bayesian.py
```

**What it does**:
- Ranks mental models by effectiveness using Bayesian inference
- Calculates success rates with credible intervals
- Performs A/B tests between models

**Output**: `data/processed/model_effectiveness.json`

**Example output**:
```json
{
  "models": [
    {
      "model_name": "Incentive-Caused Bias",
      "success_rate": 0.85,
      "confidence_interval": [0.78, 0.91],
      "n_applications": 127,
      "bayesian_score": 15.3
    }
  ]
}
```

### HMM Regime Detection

```bash
python src/analysis/hmm_regime.py
```

**What it does**:
- Detects distinct decision-making regimes in historical data
- Identifies regime transitions
- Characterizes each regime by dominant models and features

**Output**: `data/processed/regime_analysis.json`

**Example output**:
```json
{
  "n_regimes": 4,
  "regime_characteristics": {
    "0": {
      "label": "Stable Regime",
      "n_cases": 245,
      "dominant_models": ["Availability Bias", "Social Proof"],
      "mean_features": {
        "severity": 0.45,
        "models_involved": 2.3
      }
    }
  }
}
```

### PySpark Analysis

```bash
python src/spark/analysis.py
```

**What it does**:
- Large-scale pattern detection
- Lollapalooza effect calculation
- Decade-by-decade analysis
- Regional comparisons

---

## Using the API

### Starting the API Server

```bash
python src/api/main.py
```

Server runs at `http://localhost:8000`

API documentation at `http://localhost:8000/docs`

### Example API Calls

#### Get System Statistics

```bash
curl http://localhost:8000/stats
```

Response:
```json
{
  "total_models": 113,
  "total_principles": 264,
  "total_cases": 100523,
  "total_frameworks": 16,
  "model_categories": 12
}
```

#### Query Mental Models

```bash
# All models
curl http://localhost:8000/models

# Filter by category
curl "http://localhost:8000/models?category=Psychology"

# Search
curl "http://localhost:8000/models?search=bias"
```

#### Get Lollapalooza Cases

```bash
curl "http://localhost:8000/lollapalooza?min_models=5&limit=20"
```

#### Decade Analysis

```bash
curl http://localhost:8000/analysis/decade
```

### Using Python Requests

```python
import requests

# Get all mental models
response = requests.get("http://localhost:8000/models")
models = response.json()

# Filter case studies
response = requests.get(
    "http://localhost:8000/cases",
    params={
        "category": "Financial Crisis",
        "min_severity": 0.8,
        "limit": 50
    }
)
cases = response.json()

# Get Lollapalooza cases
response = requests.get(
    "http://localhost:8000/lollapalooza",
    params={"min_models": 5}
)
lollapalooza_cases = response.json()
```

---

## Dashboard

### Starting the Dashboard

```bash
python src/dashboard/app.py
```

Dashboard runs at `http://localhost:8050`

### Features

#### Overview Tab
- System statistics (models, principles, cases, frameworks)
- Top 20 most frequently applied models
- Category distribution pie chart
- Decade-by-decade analysis

#### Mental Models Tab
- Browse all mental models
- Filter by category
- View originator and Lindy age

#### Case Studies Tab
- Explore case studies
- Filter by category and region
- Severity distribution histogram
- Lollapalooza score scatter plot

#### Lollapalooza Tab
- High-interaction cases (Lollapalooza score > 0.5)
- Model interaction analysis
- Severity vs models involved scatter plot

### Tips for Effective Use

1. **Start with Overview** - Get a sense of the data distribution
2. **Filter strategically** - Use category and region filters to narrow focus
3. **Look for patterns** - Identify which models appear together frequently
4. **Study extreme cases** - High Lollapalooza cases offer the most learning
5. **Compare decades** - See how model application has evolved over time

---

## Advanced Usage

### Custom Analysis Scripts

Create custom analyses by importing modules:

```python
from src.analysis.bayesian import BayesianAnalyzer
from src.analysis.hmm_regime import HMMRegimeDetector

# Bayesian analysis
analyzer = BayesianAnalyzer()
analyzer.connect()

effectiveness = analyzer.calculate_model_effectiveness("Incentive-Caused Bias")
print(f"Success rate: {effectiveness.success_rate:.2%}")

analyzer.close()

# HMM regime detection
detector = HMMRegimeDetector(n_regimes=5)
detector.connect()

features, metadata = detector.prepare_features()
detector.train(features)
states = detector.predict_regimes(features)

detector.close()
```

### Adding Custom Case Studies

Create a JSON file in `data/raw/`:

```json
[
  {
    "name": "My Custom Case",
    "date": "2023-01-15",
    "category": "Business Decision",
    "region": "North America",
    "description": "Detailed description...",
    "severity": 0.7,
    "financial_impact": 5000000,
    "models_involved": 4,
    "lollapalooza_score": 0.65,
    "mental_models_applied": [
      {
        "model": "Sunk Cost Fallacy",
        "application": "How it applied...",
        "effect_size": 0.8
      }
    ],
    "outcome": "What happened...",
    "lessons_learned": ["Lesson 1", "Lesson 2"],
    "key_decisions": ["Decision 1", "Decision 2"],
    "mistakes_avoided": ["Mistake 1", "Mistake 2"]
  }
]
```

Then import:

```bash
python src/database/import_custom_cases.py data/raw/my_cases.json
```

### Exporting Data

#### Excel Export

```bash
python src/export/excel_builder.py
```

Generates: `data/processed/mental_models_report.xlsx`

Includes:
- Mental models sheet
- Case studies sheet
- Model frequency analysis
- Charts and visualizations

#### JSON Export

```python
import json
import psycopg2

conn = psycopg2.connect(dbname="mental_models", user="postgres")
cur = conn.cursor()

cur.execute("SELECT * FROM mental_models")
models = cur.fetchall()

with open("models_export.json", "w") as f:
    json.dump(models, f, indent=2)

conn.close()
```

---

## Troubleshooting

### Database Connection Issues

**Problem**: `psycopg2.OperationalError: could not connect to server`

**Solution**:
1. Check PostgreSQL is running: `sudo systemctl status postgresql`
2. Verify credentials in `.env`
3. Check firewall rules
4. Ensure database exists: `psql -U postgres -l`

### PySpark Memory Errors

**Problem**: `OutOfMemoryError` during Spark analysis

**Solution**:
1. Increase driver memory in `config/settings.py`:
   ```python
   driver_memory: str = "8g"  # Increase from 4g
   ```
2. Reduce data size for testing
3. Use sampling for large datasets

### API Not Starting

**Problem**: `Address already in use`

**Solution**:
1. Check if port 8000 is in use: `lsof -i :8000`
2. Kill existing process: `kill -9 <PID>`
3. Change port in `.env`: `API_PORT=8001`

### Dashboard Not Loading Data

**Problem**: Dashboard shows "No data available"

**Solution**:
1. Verify database is populated: `psql -U postgres -d mental_models -c "SELECT COUNT(*) FROM mental_models;"`
2. Check database connection in dashboard logs
3. Ensure `.env` variables are loaded
4. Restart dashboard: `python src/dashboard/app.py`

### Import Errors

**Problem**: `ModuleNotFoundError: No module named 'config'`

**Solution**:
1. Ensure you're in the correct directory: `cd mental_models_system`
2. Check Python path: `echo $PYTHONPATH`
3. Add to path if needed: `export PYTHONPATH=$PYTHONPATH:$(pwd)`

---

## Best Practices

### 1. Regular Backups

```bash
# Backup database
pg_dump -U postgres mental_models > backup_$(date +%Y%m%d).sql

# Restore
psql -U postgres mental_models < backup_20240101.sql
```

### 2. Version Control

- Commit schema changes to Git
- Tag releases: `git tag -a v1.0.0 -m "Initial release"`
- Document breaking changes in CHANGELOG.md

### 3. Performance Optimization

- Create indexes on frequently queried columns
- Use connection pooling for API
- Cache expensive queries
- Paginate large result sets

### 4. Security

- Never commit `.env` to Git
- Use strong database passwords
- Enable SSL for production database connections
- Implement API authentication for production

### 5. Monitoring

- Log all API requests
- Monitor database query performance
- Set up alerts for errors
- Track system resource usage

---

## Getting Help

- **Documentation**: See `docs/ARCHITECTURE.md` for system design
- **Issues**: Report bugs on GitHub Issues
- **Discussions**: Ask questions in GitHub Discussions
- **Email**: support@mentalmodelssystem.com (future)

---

## Contributing

We welcome contributions! See `CONTRIBUTING.md` for guidelines.

Key areas for contribution:
- Additional case studies (with Planck knowledge)
- New mental models from other thinkers
- Improved analyses and visualizations
- Bug fixes and performance improvements
- Documentation improvements

---

**Remember**: This system is built for 100 years. Prioritize Planck knowledge (deep understanding) over chauffeur knowledge (superficial facts). Every case study should explain WHY models work, not just THAT they work.
