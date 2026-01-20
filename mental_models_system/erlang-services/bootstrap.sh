#!/bin/bash
###############################################################################
# BOOTSTRAP WATCHER
# 
# This script runs OUTSIDE of Docker and watches for new commits.
# When it finds one, it pulls the code and rebuilds the auto-updater.
# This solves the chicken-and-egg problem where a broken auto-updater
# can't update itself.
#
# Run this once on your machine:
#   chmod +x bootstrap.sh && ./bootstrap.sh
#
# It will:
# 1. Pull latest code from GitHub
# 2. Rebuild and restart the auto-updater
# 3. The auto-updater then handles all other services
###############################################################################

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=========================================="
echo "BOOTSTRAP: Pulling latest code..."
echo "=========================================="

git fetch origin release4
git reset --hard origin/release4

echo "=========================================="
echo "BOOTSTRAP: Rebuilding auto-updater..."
echo "=========================================="

docker-compose build auto-updater-service

echo "=========================================="
echo "BOOTSTRAP: Restarting auto-updater..."
echo "=========================================="

docker-compose up -d auto-updater-service

echo "=========================================="
echo "BOOTSTRAP: Done! Auto-updater is now running with latest code."
echo "It will automatically update all other services."
echo ""
echo "Monitor with: docker logs -f mental-models-auto-updater"
echo "=========================================="
