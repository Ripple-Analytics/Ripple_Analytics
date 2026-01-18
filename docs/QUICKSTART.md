# Mental Models System - Quickstart Guide

Get up and running in 5 minutes.

## Prerequisites

- Python 3.11+
- Docker & Docker Compose (optional, for full stack)
- 8GB+ RAM (16GB+ recommended for local LLM)

## Option 1: Quick Start (Python Only)

```bash
# Clone the repository
git clone https://github.com/Ripple-Analytics/Ripple_Analytics.git
cd Ripple_Analytics/mental_models_system

# Install dependencies
pip install -r requirements.txt

# Test the CLI
python cli.py list-models
python cli.py failure-stats
python cli.py recommend "Should I invest in this tech startup?"
```

## Option 2: Full Stack (Docker)

```bash
# Clone and start all services
git clone https://github.com/Ripple-Analytics/Ripple_Analytics.git
cd Ripple_Analytics

# Start development stack
docker-compose up -d

# Or start production stack
docker-compose -f docker-compose.prod.yml up -d
```

### Services Available

| Service | Port | URL |
|---------|------|-----|
| API | 8000 | http://localhost:8000 |
| Grafana | 3000 | http://localhost:3000 |
| Prometheus | 9090 | http://localhost:9090 |
| PostgreSQL | 5432 | localhost:5432 |
| Redis | 6379 | localhost:6379 |

Default Grafana credentials: `admin` / `admin`

## Option 3: With Local LLM (Ollama)

```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Pull a model (choose based on your hardware)
ollama pull llama3:8b      # 8GB RAM minimum
ollama pull llama3:70b     # 48GB RAM minimum
ollama pull mistral:7b     # 8GB RAM minimum

# Start Ollama server
ollama serve

# Run analysis with local LLM
python cli.py analyze "Your document text here" --backend ollama --model llama3:8b
```

## Basic Usage

### 1. List Mental Models

```bash
# List all 129 models
python cli.py list-models

# Filter by category
python cli.py list-models --category Psychology
python cli.py list-models --category Economics
```

### 2. Analyze Text

```bash
# Analyze text through mental models lens
python cli.py analyze "Tesla is building a network of charging stations..."

# Analyze a file
python cli.py analyze-file report.pdf
```

### 3. Risk Assessment

```bash
# Assess risk with failure modes
python cli.py assess-risk "Investing in a new cryptocurrency" --models "Social Proof" "FOMO"
```

### 4. Get Recommendations

```bash
# Get model recommendations for a decision
python cli.py recommend "Should I expand into the European market?"
```

### 5. Search Failure Modes

```bash
# Search for relevant failure modes
python cli.py failure-search "overconfidence"
python cli.py failure-search "market timing"
```

## API Usage

### Start the API Server

```bash
cd mental_models_system
python -m src.api.server
```

### Example API Calls

```bash
# Health check
curl http://localhost:8000/health

# List models
curl http://localhost:8000/models

# Analyze text
curl -X POST http://localhost:8000/analyze \
  -H "Content-Type: application/json" \
  -d '{"text": "Amazon is leveraging network effects...", "document_name": "analysis"}'

# Risk assessment
curl -X POST http://localhost:8000/failure-modes/assess-risk \
  -H "Content-Type: application/json" \
  -d '{"context": "Investing in tech startup", "models": ["Network Effects"]}'

# Search failure modes
curl "http://localhost:8000/failure-modes/search?query=overconfidence"
```

## Environment Variables

Create a `.env` file from the template:

```bash
cp .env.example .env
```

Key variables:

```env
# LLM Configuration
LLM_BACKEND=ollama          # ollama, llamacpp, openai
LLM_MODEL=llama3:8b
LLM_URL=http://localhost:11434

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/mental_models

# Redis (for caching)
REDIS_URL=redis://localhost:6379

# Slack (optional)
SLACK_BOT_TOKEN=xoxb-...
SLACK_CHANNEL_ID=C0A9DAP1WTU
```

## Next Steps

1. **Explore the CLI**: Run `python cli.py --help` for all commands
2. **View Grafana Dashboards**: Open http://localhost:3000
3. **Read the API Docs**: Open http://localhost:8000/docs
4. **Set up Webhooks**: Configure alerts for Lollapalooza detection
5. **Schedule Jobs**: Set up periodic analysis runs

## Troubleshooting

### "Module not found" errors

```bash
pip install -r requirements.txt
```

### Ollama connection refused

```bash
# Make sure Ollama is running
ollama serve

# Check if model is downloaded
ollama list
```

### Docker services not starting

```bash
# Check logs
docker-compose logs -f

# Restart services
docker-compose down && docker-compose up -d
```

## Support

- **GitHub Issues**: https://github.com/Ripple-Analytics/Ripple_Analytics/issues
- **Slack**: #all-ripple-analytics
