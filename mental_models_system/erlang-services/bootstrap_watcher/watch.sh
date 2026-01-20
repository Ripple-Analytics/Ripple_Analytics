#!/bin/bash
###############################################################################
# BOOTSTRAP WATCHER - Self-Healing GitHub Monitor
# 
# CRITICAL: This script uses the GitHub API (NOT git) to detect new commits.
# This bypasses any broken git commands in the auto-updater.
#
# When a new commit is detected, it forcibly rebuilds the entire system.
# This solves the chicken-and-egg problem where the auto-updater's git
# commands are broken and can't detect the fix that would repair them.
###############################################################################

set -e

REPO="Ripple-Analytics/Ripple_Analytics"
BRANCH="${GITHUB_BRANCH:-release4}"
CHECK_INTERVAL="${CHECK_INTERVAL:-60}"
LAST_COMMIT_FILE="/data/bootstrap_last_commit"
LOG_FILE="/data/bootstrap_watcher.log"
STATUS_FILE="/data/bootstrap_status.json"
SERVICES_PATH="/repo/mental_models_system/erlang-services"

log() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] [BOOTSTRAP] $1"
    echo "$msg"
    echo "$msg" >> "$LOG_FILE" 2>/dev/null || true
}

log_status() {
    local status="$1"
    local commit="$2"
    local message="$3"
    cat > "$STATUS_FILE" 2>/dev/null << EOF || true
{
    "status": "$status",
    "last_commit": "$commit",
    "last_check": "$(date -Iseconds)",
    "message": "$message"
}
EOF
}

get_github_commit() {
    # Use GitHub API to get latest commit - no git required!
    local response
    if [ -n "$GITHUB_TOKEN" ]; then
        response=$(curl -s -H "Authorization: token $GITHUB_TOKEN" \
            "https://api.github.com/repos/$REPO/commits/$BRANCH" 2>/dev/null)
    else
        response=$(curl -s "https://api.github.com/repos/$REPO/commits/$BRANCH" 2>/dev/null)
    fi
    
    # Extract SHA from JSON response
    echo "$response" | grep -o '"sha": "[^"]*"' | head -1 | cut -d'"' -f4 | cut -c1-7
}

get_last_known_commit() {
    if [ -f "$LAST_COMMIT_FILE" ]; then
        cat "$LAST_COMMIT_FILE" 2>/dev/null || echo ""
    else
        echo ""
    fi
}

save_commit() {
    echo "$1" > "$LAST_COMMIT_FILE" 2>/dev/null || true
}

rebuild_system() {
    local commit="$1"
    log "=========================================="
    log "BOOTSTRAP REBUILD TRIGGERED"
    log "New commit: $commit"
    log "=========================================="
    
    log_status "rebuilding" "$commit" "Rebuilding entire system"
    
    # Step 1: Pull latest code using git (in the repo volume)
    log "Step 1: Pulling latest code..."
    cd /repo 2>/dev/null || {
        log "ERROR: /repo not mounted, trying /services"
        cd /services 2>/dev/null || {
            log "ERROR: No repo directory found"
            return 1
        }
    }
    
    git fetch origin $BRANCH 2>&1 | head -10
    git reset --hard origin/$BRANCH 2>&1 | head -5
    
    # Step 2: Rebuild auto-updater first (it contains the fixes)
    log "Step 2: Rebuilding auto-updater..."
    cd "$SERVICES_PATH" 2>/dev/null || cd /services 2>/dev/null || {
        log "ERROR: Cannot find services directory"
        return 1
    }
    
    docker-compose build --no-cache auto-updater-service 2>&1 | tail -30
    
    # Step 3: Restart auto-updater
    log "Step 3: Restarting auto-updater..."
    docker-compose up -d auto-updater-service 2>&1 | tail -10
    
    # Step 4: Rebuild and restart desktop-ui-blue
    log "Step 4: Rebuilding desktop-ui-blue..."
    docker-compose build --no-cache desktop-ui-blue 2>&1 | tail -30 || true
    docker-compose up -d desktop-ui-blue 2>&1 | tail -10 || true
    
    # Step 5: Verify services are running
    log "Step 5: Verifying services..."
    sleep 10
    docker ps --format "table {{.Names}}\t{{.Status}}" 2>&1 | head -20
    
    # Save the commit we just deployed
    save_commit "$commit"
    
    log "=========================================="
    log "BOOTSTRAP REBUILD COMPLETE"
    log "=========================================="
    log_status "idle" "$commit" "Rebuild complete"
}

main() {
    log "=========================================="
    log "BOOTSTRAP WATCHER STARTING"
    log "Repo: $REPO"
    log "Branch: $BRANCH"
    log "Check interval: ${CHECK_INTERVAL}s"
    log "=========================================="
    
    # Ensure data directory exists
    mkdir -p /data 2>/dev/null || true
    
    # Initial check
    local last_commit=$(get_last_known_commit)
    log "Last known commit: ${last_commit:-none}"
    
    # Wait for docker to be ready
    sleep 5
    
    while true; do
        log "Checking GitHub API for new commits..."
        
        local remote_commit=$(get_github_commit)
        
        if [ -z "$remote_commit" ]; then
            log "ERROR: Failed to get commit from GitHub API"
            log_status "error" "$last_commit" "GitHub API failed"
            sleep $CHECK_INTERVAL
            continue
        fi
        
        log "Remote commit: $remote_commit, Last: ${last_commit:-none}"
        
        if [ "$remote_commit" != "$last_commit" ]; then
            log "NEW COMMIT DETECTED!"
            rebuild_system "$remote_commit"
            last_commit="$remote_commit"
        else
            log "No changes detected"
            log_status "idle" "$last_commit" "No changes"
        fi
        
        sleep $CHECK_INTERVAL
    done
}

main "$@"
