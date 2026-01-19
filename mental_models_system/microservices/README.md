# Mental Models Microservices (Electric Clojure)

This directory contains the Electric Clojure microservices architecture for the Mental Models System.

## Architecture Overview

The system is composed of four independent microservices:

```
┌─────────────────────────────────────────────────────────────┐
│                    Desktop App / Client                      │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    API Gateway (8000)                        │
│              Routes requests to backend services             │
└─────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        ▼                     ▼                     ▼
┌───────────────┐   ┌───────────────┐   ┌───────────────┐
│   Analysis    │   │   Harvester   │   │   Storage     │
│   Service     │   │   Service     │   │   Service     │
│   (8001)      │   │   (8002)      │   │   (8003)      │
└───────────────┘   └───────────────┘   └───────────────┘
```

## Services

### API Gateway (Port 8000)
Central entry point for all requests. Routes to appropriate backend services.

**Endpoints:**
- `GET /` - Service info
- `GET /health` - Health check (includes all services)
- `POST /api/analysis/analyze` - Analyze text for mental models
- `GET /api/analysis/models` - Get all mental models
- `GET /api/analysis/categories` - Get model categories
- `POST /api/harvester/scrape` - Start scraping job
- `GET /api/harvester/jobs` - List scraping jobs
- `GET /api/storage/data` - Get stored data
- `POST /api/storage/data` - Store data

### Analysis Service (Port 8001)
Mental model detection and analysis using keyword matching and optional LLM integration.

**Endpoints:**
- `GET /` - Service info
- `GET /health` - Health check
- `POST /analyze` - Analyze text for mental models
- `GET /models` - Get all mental models
- `GET /models/:id` - Get specific model
- `GET /categories` - Get all categories
- `GET /categories/:name` - Get models by category

### Harvester Service (Port 8002)
Web scraping and data collection with background job processing.

**Endpoints:**
- `GET /` - Service info
- `GET /health` - Health check
- `POST /scrape` - Start scraping job
- `GET /jobs` - List all jobs
- `GET /jobs/:id` - Get job status
- `DELETE /jobs/:id` - Delete job
- `GET /status` - Harvester status
- `GET /data` - Get scraped data

### Storage Service (Port 8003)
Data persistence for analyses, settings, and user data.

**Endpoints:**
- `GET /` - Service info
- `GET /health` - Health check
- `GET /stats` - Storage statistics
- `GET /data` - Data overview
- `POST /data` - Store data
- `GET /analyses` - Get all analyses
- `POST /analyses` - Store analysis
- `GET /settings` - Get settings
- `POST /settings` - Update settings

## Quick Start

### Using Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Rebuild and start
docker-compose up -d --build

# Stop all services
docker-compose down
```

### Running Individually

Each service can be run independently:

```bash
# API Gateway
cd api-gateway
clojure -M:run

# Analysis Service
cd analysis-service
clojure -M:run

# Harvester Service
cd harvester-service
clojure -M:run

# Storage Service
cd storage-service
clojure -M:run
```

## Environment Variables

| Variable | Service | Default | Description |
|----------|---------|---------|-------------|
| `PORT` | All | Service-specific | HTTP port |
| `ANALYSIS_SERVICE_URL` | Gateway | `http://analysis-service:8001` | Analysis service URL |
| `HARVESTER_SERVICE_URL` | Gateway | `http://harvester-service:8002` | Harvester service URL |
| `STORAGE_SERVICE_URL` | Gateway | `http://storage-service:8003` | Storage service URL |
| `LM_STUDIO_URL` | Analysis | `http://localhost:1234` | LM Studio URL for AI analysis |

## Hot Reloading

Each service supports hot reloading through Clojure's REPL-driven development:

1. Start the service with a REPL
2. Make code changes
3. Reload the namespace
4. Changes take effect immediately

For Docker deployments, use rolling updates:

```bash
# Rebuild and restart a single service
docker-compose up -d --build analysis-service
```

## Health Checks

All services expose a `/health` endpoint. The API Gateway aggregates health from all services:

```bash
curl http://localhost:8000/health
```

Response:
```json
{
  "status": "healthy",
  "services": {
    "analysis": "healthy",
    "harvester": "healthy",
    "storage": "healthy"
  }
}
```

## Example Usage

### Analyze Text

```bash
curl -X POST http://localhost:8000/api/analysis/analyze \
  -H "Content-Type: application/json" \
  -d '{"text": "We should continue this project because we already invested so much time and money into it.", "top_n": 3}'
```

### Start Scraping Job

```bash
curl -X POST http://localhost:8000/api/harvester/scrape \
  -H "Content-Type: application/json" \
  -d '{"url": "https://example.com"}'
```

### Store Data

```bash
curl -X POST http://localhost:8000/api/storage/data \
  -H "Content-Type: application/json" \
  -d '{"key": "my-analysis", "value": {"result": "test"}, "collection": "analyses"}'
```

## Development

### Prerequisites

- Clojure 1.11+
- Docker and Docker Compose (for containerized deployment)
- Java 21+ (Temurin recommended)

### Project Structure

```
microservices/
├── api-gateway/
│   ├── deps.edn
│   ├── Dockerfile
│   └── src/gateway/core.clj
├── analysis-service/
│   ├── deps.edn
│   ├── Dockerfile
│   └── src/analysis/core.clj
├── harvester-service/
│   ├── deps.edn
│   ├── Dockerfile
│   └── src/harvester/core.clj
├── storage-service/
│   ├── deps.edn
│   ├── Dockerfile
│   └── src/storage/core.clj
├── docker-compose.yml
└── README.md
```

## License

Part of the Mental Models System by Ripple Analytics.
