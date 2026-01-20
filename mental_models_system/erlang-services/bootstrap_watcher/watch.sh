#!/bin/bash
###############################################################################
# BOOTSTRAP WATCHER
# 
# Runs as a separate container. Monitors GitHub for new commits.
# When found, pulls code and rebuilds the auto-updater container.
# This ensures the auto-updater can always be updated even if broken.
###############################################################################

REPO_PATH="/services"
BRANCH="${GITHUB_BRANCH:-release2}"
CHECK_INTERVAL="${BOOTSTRAP_INTERVAL:-60}"
LAST_COMMIT=""

echo "[BOOTSTRAP] Starting bootstrap watcher..."
echo "[BOOTSTRAP] Branch: $BRANCH"
echo "[BOOTSTRAP] Check interval: ${CHECK_INTERVAL}s"

# Wait for initial setup
sleep 10

while true; do
    echo "[BOOTSTRAP] Checking for updates..."
    
    cd "$REPO_PATH"
    
    # Fetch latest
    git fetch origin "$BRANCH" 2>/dev/null
    
    # Get current and remote commits
    CURRENT=$(git rev-parse HEAD 2>/dev/null | head -c 7)
    REMOTE=$(git rev-parse "origin/$BRANCH" 2>/dev/null | head -c 7)
    
    echo "[BOOTSTRAP] Current: $CURRENT, Remote: $REMOTE, LastProcessed: $LAST_COMMIT"
    
    # Check if update needed
    if [ -n "$REMOTE" ] && [ "$REMOTE" != "$CURRENT" ] && [ "$REMOTE" != "$LAST_COMMIT" ]; then
        echo "[BOOTSTRAP] =========================================="
        echo "[BOOTSTRAP] NEW COMMIT DETECTED: $REMOTE"
        echo "[BOOTSTRAP] Updating auto-updater..."
        echo "[BOOTSTRAP] =========================================="
        
        # Pull the new code
        git reset --hard "origin/$BRANCH"
        
        # Rebuild auto-updater using docker
        echo "[BOOTSTRAP] Rebuilding auto-updater-service..."
        docker-compose -f "$REPO_PATH/docker-compose.yml" build auto-updater-service 2>&1 | tail -20
        
        if [ $? -eq 0 ]; then
            echo "[BOOTSTRAP] Restarting auto-updater-service..."
            docker-compose -f "$REPO_PATH/docker-compose.yml" up -d auto-updater-service
            
            LAST_COMMIT="$REMOTE"
            echo "[BOOTSTRAP] Auto-updater updated to $REMOTE"
        else
            echo "[BOOTSTRAP] Build failed, will retry next cycle"
        fi
    else
        echo "[BOOTSTRAP] No update needed"
    fi
    
    sleep "$CHECK_INTERVAL"
done
