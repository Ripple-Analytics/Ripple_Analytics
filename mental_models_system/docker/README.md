# Mental Models Microservices

Dockerized microservices architecture for the Mental Models System.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Desktop App (Electron)                    │
│                         Port: N/A                            │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    API Gateway                               │
│                    Port: 8000                                │
│         Routes requests to appropriate services              │
└────────┬──────────────┬──────────────┬─────────────────────┘
         │              │              │
         ▼              ▼              ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  Analysis   │  │  Harvester  │  │   Storage   │
│  Service    │  │  Service    │  │   Service   │
│  Port:8001  │  │  Port:8002  │  │  Port:8003  │
└─────────────┘  └─────────────┘  └─────────────┘
```

## Services

### API Gateway (Port 8000)
Central entry point for all microservices. Routes requests and provides health monitoring.

**Endpoints:**
- `GET /` - Service info
- `GET /health` - Health check for all services
- `POST /api/analysis/analyze` - Analyze text for mental models
- `GET /api/analysis/models` - Get all mental models
- `POST /api/harvester/scrape` - Start web scraping job
- `GET /api/storage/data` - Get stored data

### Analysis Service (Port 8001)
Mental model detection and analysis using keyword matching.

**Endpoints:**
- `GET /health` - Health check
- `POST /analyze` - Analyze text
- `GET /models` - List all models
- `GET /models/{id}` - Get specific model
- `GET /categories` - List categories

### Harvester Service (Port 8002)
Web scraping and data collection with background job processing.

**Endpoints:**
- `GET /health` - Health check
- `POST /scrape` - Start scraping job
- `GET /jobs` - List all jobs
- `GET /jobs/{id}` - Get job status
- `GET /status` - Harvester status

### Storage Service (Port 8003)
Data persistence for analysis results, settings, and user data.

**Endpoints:**
- `GET /health` - Health check
- `POST /data` - Store data
- `GET /data` - Get all data
- `GET /analyses` - Get stored analyses
- `GET /settings` - Get settings

## Quick Start

### Prerequisites
- Docker
- Docker Compose

### Running the Services

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop all services
docker-compose down

# Rebuild and start
docker-compose up -d --build
```

### Testing the Services

```bash
# Check health
curl http://localhost:8000/health

# Analyze text
curl -X POST http://localhost:8000/api/analysis/analyze \
  -H "Content-Type: application/json" \
  -d '{"text": "We should consider the opportunity cost and avoid sunk cost fallacy"}'

# Get all models
curl http://localhost:8000/api/analysis/models
```

## Hot-Loading Updates

Each service can be updated independently:

1. **Update a service:**
   ```bash
   docker-compose build analysis-service
   docker-compose up -d analysis-service
   ```

2. **Rolling update (zero downtime):**
   ```bash
   docker-compose up -d --no-deps --build analysis-service
   ```

3. **Scale a service:**
   ```bash
   docker-compose up -d --scale analysis-service=3
   ```

## Environment Variables

### API Gateway
- `ANALYSIS_SERVICE_URL` - Analysis service URL (default: http://analysis-service:8001)
- `HARVESTER_SERVICE_URL` - Harvester service URL (default: http://harvester-service:8002)
- `STORAGE_SERVICE_URL` - Storage service URL (default: http://storage-service:8003)

### Storage Service
- `DATA_DIR` - Data directory path (default: /data)

## Volumes

- `storage-data` - Persistent storage for the Storage Service

## Network

All services communicate over the `mental-models-network` bridge network.

## Health Checks

Each service has built-in health checks:
- Interval: 30 seconds
- Timeout: 10 seconds
- Retries: 3
- Start period: 10 seconds

## Development

### Adding a New Service

1. Create a new directory under `docker/`
2. Add `app.py`, `Dockerfile`, and `requirements.txt`
3. Add the service to `docker-compose.yml`
4. Update the API Gateway to route to the new service

### Local Development

```bash
# Run a single service locally
cd docker/analysis-service
pip install -r requirements.txt
python app.py
```
