# Ripple Analytics - Mental Models System

**The Oligarch's Operating System** - A comprehensive, industrial-scale mental models framework integrating wisdom from Munger, Soros, Dalio, Simons, Franklin, Seneca, Lee Kuan Yew, Rockefeller, and 15+ other historical thinkers.

Built for 100 years. Planck knowledge, not chauffeur knowledge.

**Fully open source. Run locally. No API costs required.**

## Quick Start with Docker

```bash
# Start all services (PostgreSQL, API, Grafana, Jupyter, Ollama)
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
- **Ollama**: http://localhost:11434

## Connectors (19 Total)

### Full Connector List

| Category | Connector | Type | Description |
|----------|-----------|------|-------------|
| **Version Control** | `github` | 🟢 OSS | Issues, PRs, repos, actions, webhooks |
| **Web Scraping** | `beautifulsoup` | 🟢 OSS | Simple HTML scraping |
| | `playwright` | 🟢 OSS | Browser automation for JS-heavy sites |
| | `scrapy` | 🟢 OSS | Industrial-strength web crawling |
| | `rss` | 🟢 OSS | RSS/Atom feed reader |
| **Chat** | `slack` | 🔵 Cloud | Slack workspace integration |
| | `discord` | 🔵 Free | Discord server integration |
| | `matrix` | 🟢 OSS | Fully open source, self-hosted |
| | `webhook` | 🟢 OSS | Generic webhook for any service |
| **Data Sources** | `sec` | 🟢 OSS | SEC EDGAR filings (10-K, 10-Q, 8-K) |
| | `yahoo_finance` | 🟢 OSS | Market data, financials |
| | `gdelt` | 🟢 OSS | Global events database |
| | `alpha_vantage` | 🟢 OSS | Market data API |
| **Storage** | `local` | 🟢 OSS | Local filesystem |
| | `s3` | 🟢 OSS | S3/MinIO compatible |
| | `gdrive` | 🟢 OSS | Google Drive via rclone |
| **LLM Providers** | `ollama` | 🟢 OSS | Local LLM inference |
| | `llamacpp` | 🟢 OSS | Efficient local inference |
| | `openai` | 🔵 Cloud | GPT-4, GPT-3.5 |

**Legend:** 🟢 OSS = Fully Open Source | 🔵 Cloud/Free = External Service

### Open Source Alternatives

Every paid service has an open source alternative:

| Paid Service | Open Source Alternative | Notes |
|--------------|------------------------|-------|
| Firecrawl | Scrapy, Playwright, BeautifulSoup | Full scraping capability |
| OpenAI | Ollama, llama.cpp, vLLM | Run locally, no API costs |
| Slack | Matrix, Mattermost | Self-hosted |
| NewsAPI | RSS feeds, GDELT | Free, unlimited |
| AWS S3 | MinIO | S3-compatible, self-hosted |

### GitHub Integration

Full GitHub integration for automated workflows:

```python
from src.connectors import GitHubConnector

github = GitHubConnector()
await github.connect()

# Issue Management
await github.create_issue("Fix bug", "Description", repo="user/repo")
await github.list_issues(state="open")
await github.close_issue(123)

# PR Management
await github.create_pr("Feature", "Description", head="feature-branch")
await github.merge_pr(456, method="squash")

# Repository Operations
await github.clone_repo("user/repo")
await github.create_repo("new-repo", private=True)

# Actions/Workflows
await github.list_workflows()
await github.run_workflow("ci.yml", inputs={"version": "1.0"})

# Delegate to Devin
await github.delegate_to_devin("Implement feature X")
```

### Slack Bot Interface

Run the system via Slack (same workflow as Manus/Devin):

```python
from src.slack_bot import MentalModelsBot

bot = MentalModelsBot(
    slack_bot_token="xoxb-...",
    slack_signing_secret="..."
)
bot.start()
```

**Slack Commands:**
- `/analyze <text>` - Analyze with mental models
- `/models` - List all 129 models
- `/search <query>` - Search knowledge graph
- `/delegate <task>` - Assign to agent
- `/lollapalooza` - Find model convergence

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    MENTAL MODELS SYSTEM                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐    │
│  │   SLACK     │  │   GITHUB    │  │    CLI      │  │    API      │    │
│  │    BOT      │  │ INTEGRATION │  │   TOOL      │  │   SERVER    │    │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘    │
│         │                │                │                │            │
│         └────────────────┴────────────────┴────────────────┘            │
│                                   │                                      │
│                    ┌──────────────┴──────────────┐                      │
│                    │      CONNECTOR REGISTRY      │                      │
│                    │   (19 connectors available)  │                      │
│                    └──────────────┬──────────────┘                      │
│                                   │                                      │
│    ┌──────────────────────────────┼──────────────────────────────┐      │
│    │                              │                              │      │
│    ▼                              ▼                              ▼      │
│ ┌──────────┐              ┌──────────────┐              ┌──────────┐   │
│ │   LLM    │              │   ANALYSIS   │              │  STORAGE │   │
│ │ BACKENDS │              │    ENGINE    │              │  LAYER   │   │
│ │          │              │              │              │          │   │
│ │ • Ollama │              │ • 129 Models │              │ • Local  │   │
│ │ • llama  │◄────────────►│ • 645 Fails  │◄────────────►│ • S3     │   │
│ │ • OpenAI │              │ • Detection  │              │ • GDrive │   │
│ └──────────┘              └──────────────┘              └──────────┘   │
│                                   │                                      │
│                    ┌──────────────┴──────────────┐                      │
│                    │      KNOWLEDGE GRAPH         │                      │
│                    │   Documents ↔ Models ↔ Tags  │                      │
│                    └──────────────────────────────┘                      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Using Connectors

### Initialize Registry

```python
from src.connectors import ConnectorRegistry

registry = ConnectorRegistry()

# List all available connectors
for connector in registry.list_available():
    print(f"{connector['name']}: {connector['description']}")
```

### Web Scraping (Open Source)

```python
from src.connectors import BeautifulSoupConnector, ScrapyConnector

# Simple scraping
bs = BeautifulSoupConnector()
await bs.connect()
page = await bs.scrape("https://example.com")

# Industrial crawling
scrapy = ScrapyConnector()
await scrapy.connect()
pages = await scrapy.crawl(["https://site1.com", "https://site2.com"])
```

### Data Sources

```python
from src.connectors import SECConnector, YahooFinanceConnector

# SEC Filings
sec = SECConnector()
await sec.connect()
filings = await sec.get_company_filings("320193", form_type="10-K")  # Apple

# Market Data
yahoo = YahooFinanceConnector()
await yahoo.connect()
quote = await yahoo.get_quote("AAPL")
history = await yahoo.get_historical("AAPL", period="1y")
```

### LLM Providers

```python
from src.connectors import OllamaConnector, OpenAIConnector

# Local (no API costs)
ollama = OllamaConnector()
await ollama.connect()
response = await ollama.generate(
    "Analyze this through the lens of incentive-caused bias",
    model="llama3:70b"
)

# Streaming
async for chunk in ollama.generate_stream("Explain network effects"):
    print(chunk, end="")

# Embeddings
embedding = await ollama.embed("mental model text")
```

## Mental Models

### Categories (129 Total)

| Category | Count | Examples |
|----------|-------|----------|
| Psychology | 34 | Incentive-caused bias, Social proof, Commitment |
| Thinking Tools | 18 | Inversion, First principles, Occam's razor |
| Economics | 20 | Supply/demand, Opportunity cost, Comparative advantage |
| Moats | 19 | Network effects, Switching costs, Brand |
| Mathematics | 12 | Compounding, Probability, Bayes theorem |
| Physics | 11 | Critical mass, Leverage, Equilibrium |
| Biology | 6 | Evolution, Adaptation, Ecosystem |
| Organizational | 9 | Bureaucracy, Incentive structures, Culture |

### Failure Modes (645+)

Each model has 5 documented failure modes with:
- Warning signals
- Quantitative thresholds
- Safeguards
- Real-world case studies

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

### Development Principle
**Improvement = Iteration Speed × Iteration Magnitude**

Both variables must be maximized for exponential progress.

## API Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /stats` | System statistics |
| `GET /models` | Query mental models |
| `GET /cases` | Query case studies |
| `GET /lollapalooza` | Multi-model convergence |
| `GET /agent/search` | Semantic search |
| `GET /agent/query` | Natural language Q&A |
| `POST /agent/analyze` | Multi-model analysis |

## Directory Structure

```
Ripple_Analytics/
├── docker/                 # Docker configurations
├── docker-compose.yml      # Orchestration
└── mental_models_system/
    ├── src/
    │   ├── api/           # FastAPI REST API
    │   ├── analysis/      # Model analyzer, knowledge graph
    │   ├── connectors/    # 19 connectors (GitHub, Slack, etc.)
    │   ├── detection/     # Lollapalooza detection
    │   ├── harvester/     # Signal harvester
    │   ├── intelligence/  # Cross-document network
    │   ├── integration/   # Manus API integration
    │   ├── journal/       # Decision journal
    │   ├── llm/           # LLM integration
    │   ├── pipeline/      # Terabyte processor
    │   ├── research/      # Knowledge miner
    │   ├── safeguards/    # Failure modes
    │   ├── slack_bot/     # Slack bot interface
    │   └── tracker/       # Effectiveness tracker
    ├── data/              # Raw and processed data
    ├── scripts/           # Utility scripts
    └── cli.py             # Command-line interface
```

## Environment Variables

```bash
# LLM (choose one)
OLLAMA_HOST=http://localhost:11434
OLLAMA_MODEL=llama3:70b

# Or OpenAI
OPENAI_API_KEY=sk-...

# Chat (optional)
SLACK_BOT_TOKEN=xoxb-...
SLACK_SIGNING_SECRET=...
DISCORD_WEBHOOK_URL=https://discord.com/api/webhooks/...

# Storage (optional)
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
S3_ENDPOINT_URL=http://localhost:9000  # MinIO

# GitHub
GITHUB_TOKEN=ghp_...
```

## License

MIT License - Built as a 100-year heirloom.
