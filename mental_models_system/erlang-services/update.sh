#!/bin/bash
# Mental Models System - Auto Update Script for Linux/Mac
# This script pulls the latest code from GitHub and rebuilds the containers

echo "========================================"
echo "Mental Models System - Auto Update"
echo "========================================"
echo

# Check if git is available
if ! command -v git &> /dev/null; then
    echo "ERROR: Git is not installed"
    echo "Please install Git: https://git-scm.com/"
    exit 1
fi

# Check if docker is available
if ! command -v docker &> /dev/null; then
    echo "ERROR: Docker is not installed"
    echo "Please install Docker: https://www.docker.com/products/docker-desktop/"
    exit 1
fi

echo "[1/4] Pulling latest code from GitHub..."
git pull origin master || echo "WARNING: Could not pull from GitHub. Continuing with local code..."

echo
echo "[2/4] Stopping existing containers..."
docker-compose down

echo
echo "[3/4] Rebuilding containers with latest code..."
docker-compose build --no-cache

echo
echo "[4/4] Starting updated containers..."
docker-compose up -d

echo
echo "========================================"
echo "Update complete!"
echo "========================================"
echo
echo "Access the application at: http://localhost:3000"
echo
echo "To view logs: docker-compose logs -f"
echo "To stop: docker-compose down"
