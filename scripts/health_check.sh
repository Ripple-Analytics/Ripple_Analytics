#!/bin/bash
# =============================================================================
# MENTAL MODELS SYSTEM - HEALTH CHECK SCRIPT
# =============================================================================
# Usage: ./scripts/health_check.sh
# Returns: 0 if all services healthy, 1 if any service unhealthy

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

HEALTHY=0
UNHEALTHY=0

echo "=============================================="
echo "Mental Models System - Health Check"
echo "=============================================="
echo ""

# Function to check service health
check_service() {
    local name=$1
    local url=$2
    local expected=$3
    
    printf "Checking %-15s ... " "$name"
    
    response=$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 "$url" 2>/dev/null || echo "000")
    
    if [ "$response" == "$expected" ]; then
        echo -e "${GREEN}HEALTHY${NC} (HTTP $response)"
        ((HEALTHY++))
        return 0
    else
        echo -e "${RED}UNHEALTHY${NC} (HTTP $response, expected $expected)"
        ((UNHEALTHY++))
        return 1
    fi
}

# Function to check TCP port
check_port() {
    local name=$1
    local host=$2
    local port=$3
    
    printf "Checking %-15s ... " "$name"
    
    if nc -z -w 5 "$host" "$port" 2>/dev/null; then
        echo -e "${GREEN}HEALTHY${NC} (port $port open)"
        ((HEALTHY++))
        return 0
    else
        echo -e "${RED}UNHEALTHY${NC} (port $port closed)"
        ((UNHEALTHY++))
        return 1
    fi
}

# Function to check Docker container
check_container() {
    local name=$1
    local container=$2
    
    printf "Checking %-15s ... " "$name"
    
    status=$(docker inspect --format='{{.State.Health.Status}}' "$container" 2>/dev/null || echo "not_found")
    
    case "$status" in
        "healthy")
            echo -e "${GREEN}HEALTHY${NC}"
            ((HEALTHY++))
            return 0
            ;;
        "unhealthy")
            echo -e "${RED}UNHEALTHY${NC}"
            ((UNHEALTHY++))
            return 1
            ;;
        "starting")
            echo -e "${YELLOW}STARTING${NC}"
            return 0
            ;;
        *)
            echo -e "${YELLOW}UNKNOWN${NC} ($status)"
            return 0
            ;;
    esac
}

echo "--- HTTP Services ---"
check_service "API" "http://localhost:8000/health" "200" || true
check_service "Grafana" "http://localhost:3000/api/health" "200" || true
check_service "Prometheus" "http://localhost:9090/-/healthy" "200" || true
check_service "MinIO" "http://localhost:9000/minio/health/live" "200" || true
check_service "Ollama" "http://localhost:11434/api/tags" "200" || true

echo ""
echo "--- TCP Services ---"
check_port "PostgreSQL" "localhost" "5432" || true
check_port "Redis" "localhost" "6379" || true

echo ""
echo "--- Docker Containers ---"
check_container "API Container" "mm_api" || true
check_container "PostgreSQL" "mm_postgres" || true
check_container "Redis" "mm_redis" || true
check_container "Ollama" "mm_ollama" || true
check_container "Grafana" "mm_grafana" || true
check_container "Prometheus" "mm_prometheus" || true

echo ""
echo "=============================================="
echo "Summary: ${GREEN}$HEALTHY healthy${NC}, ${RED}$UNHEALTHY unhealthy${NC}"
echo "=============================================="

if [ $UNHEALTHY -gt 0 ]; then
    exit 1
else
    exit 0
fi
