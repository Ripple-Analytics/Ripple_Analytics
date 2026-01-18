# Ripple Analytics - Mental Models System

**The Oligarch's Operating System** - A comprehensive, industrial-scale mental models framework integrating wisdom from Munger, Soros, Dalio, Simons, Franklin, Seneca, Lee Kuan Yew, Rockefeller, and 15+ other historical thinkers.

Built for 100 years. Planck knowledge, not chauffeur knowledge.

## Quick Start with Docker

```bash
# Start all services (PostgreSQL, API, Grafana, Jupyter)
docker-compose up -d

# View logs
docker-compose logs -f

# Stop all services
docker-compose down
```

Services will be available at:
- **API**: http://localhost:8000
- **Grafana**: http://localhost:3000 (admin/admin)
- **Jupyter**: http://localhost:8888
- **PostgreSQL**: localhost:5432

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    MENTAL MODELS SYSTEM                      │
├─────────────────────────────────────────────────────────────┤
│  PostgreSQL          │  PySpark              │  API/LLM     │
│  ─────────────────   │  ─────────────────    │  ──────────  │
│  • 1.2M+ Case Studies│  • Monte Carlo Sims   │  • REST API  │
│  • 113 Mental Models │  • Bayesian Inference │  • Agentic   │
│  • 264 Principles    │  • HMM Regime Detect  │    Search    │
│  • Planck Matrix     │  • Lollapalooza Calc  │  • LM Studio │
└─────────────────────────────────────────────────────────────┘
```

## LLM Integration (Agentic Search)

The system supports local LLM inference via LM Studio for agentic search over mental models:

### Using LM Studio (Recommended for Local)

1. Download and install [LM Studio](https://lmstudio.ai/)
2. Load a model (e.g., Llama 2, Mistral, or any GGUF model)
3. Start the local server (default: http://localhost:1234)
4. The Docker container will automatically connect via `host.docker.internal`

### Alternative LLM Providers

Configure via environment variables in `docker-compose.yml`:

```yaml
# OpenAI
LLM_PROVIDER: openai
OPENAI_API_KEY: your-api-key

# Ollama
LLM_PROVIDER: ollama
OLLAMA_URL: http://host.docker.internal:11434
```

### Agentic Search API

```bash
# Natural language query
curl "http://localhost:8000/agent/query?q=How%20should%20I%20think%20about%20market%20bubbles"

# Quick semantic search
curl "http://localhost:8000/agent/search?q=incentives%20and%20behavior"

# Munger-style multi-model analysis
curl -X POST "http://localhost:8000/agent/analyze" \
  -H "Content-Type: application/json" \
  -d '{"problem": "Should I invest in this company?"}'
```

## API Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /stats` | System statistics (model count, case count, etc.) |
| `GET /frameworks` | List all thinkers/frameworks |
| `GET /models` | Query mental models (filter by category, search) |
| `GET /principles` | Query thinker principles (filter by thinker) |
| `GET /cases` | Query case studies (filter by category, region, severity) |
| `GET /lollapalooza` | Get cases with multiple interacting models |
| `GET /analysis/decade` | Decade-by-decade analysis |
| `GET /agent/search` | Semantic search over models and principles |
| `GET /agent/query` | Natural language question answering |
| `POST /agent/analyze` | Munger-style multi-model analysis |

## Thinkers Integrated

| Category | Thinkers |
|----------|----------|
| Investment | Munger, Soros, Dalio, Buffett, Simons |
| Historical | Rockefeller, Getty, Rothschild, Crassus |
| Modern | Musk, Thiel, Koch |
| Statesmen | Franklin, Lee Kuan Yew |
| Philosophers | Seneca, Polya, Smil |

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

## Directory Structure

```
Ripple_Analytics/
├── docker/                 # Docker configurations
│   ├── api/               # API Dockerfile
│   ├── grafana/           # Grafana dashboards and provisioning
│   ├── jupyter/           # Jupyter Dockerfile
│   ├── postgres/          # PostgreSQL init scripts
│   └── prometheus/        # Prometheus config
├── docker-compose.yml     # Orchestration
└── mental_models_system/
    ├── src/
    │   ├── api/           # FastAPI REST API
    │   ├── analysis/      # Statistical, Bayesian, HMM analysis
    │   ├── dashboard/     # Dash dashboard
    │   ├── database/      # PostgreSQL setup and population
    │   ├── export/        # Excel report generation
    │   ├── llm/           # LLM integration (LM Studio, OpenAI, Ollama)
    │   └── spark/         # PySpark analysis
    ├── data/              # Raw and processed data
    ├── docs/              # Documentation
    ├── tests/             # Test suite
    └── config/            # Configuration
```

## License

MIT License - Built as a 100-year heirloom.
