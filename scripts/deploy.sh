#!/bin/bash
# =============================================================================
# MENTAL MODELS SYSTEM - DEPLOYMENT SCRIPT
# =============================================================================
# Usage: ./scripts/deploy.sh [dev|prod]

set -e

ENVIRONMENT=${1:-dev}
COMPOSE_FILE="docker-compose.yml"

if [ "$ENVIRONMENT" == "prod" ]; then
    COMPOSE_FILE="docker-compose.prod.yml"
fi

echo "=============================================="
echo "Mental Models System - Deployment"
echo "Environment: $ENVIRONMENT"
echo "Compose file: $COMPOSE_FILE"
echo "=============================================="
echo ""

# Check prerequisites
echo "Checking prerequisites..."

if ! command -v docker &> /dev/null; then
    echo "Error: Docker is not installed"
    exit 1
fi

if ! command -v docker-compose &> /dev/null; then
    echo "Error: Docker Compose is not installed"
    exit 1
fi

# Check .env file
if [ ! -f ".env" ]; then
    echo "Warning: .env file not found, copying from .env.example"
    cp .env.example .env
fi

# Pull latest images
echo ""
echo "Pulling latest images..."
docker-compose -f $COMPOSE_FILE pull

# Build custom images
echo ""
echo "Building custom images..."
docker-compose -f $COMPOSE_FILE build

# Stop existing containers
echo ""
echo "Stopping existing containers..."
docker-compose -f $COMPOSE_FILE down --remove-orphans

# Start containers
echo ""
echo "Starting containers..."
docker-compose -f $COMPOSE_FILE up -d

# Wait for services to be healthy
echo ""
echo "Waiting for services to be healthy..."
sleep 10

# Run health check
echo ""
./scripts/health_check.sh || true

# Pull Ollama model if needed
echo ""
echo "Checking Ollama models..."
if docker exec mm_ollama ollama list 2>/dev/null | grep -q "llama3"; then
    echo "Ollama model already available"
else
    echo "Pulling Ollama model (this may take a while)..."
    docker exec mm_ollama ollama pull llama3:8b || true
fi

echo ""
echo "=============================================="
echo "Deployment complete!"
echo ""
echo "Services available at:"
echo "  - API:        http://localhost:8000"
echo "  - API Docs:   http://localhost:8000/docs"
echo "  - Grafana:    http://localhost:3000"
echo "  - Prometheus: http://localhost:9090"
echo "  - MinIO:      http://localhost:9001"
echo "=============================================="
