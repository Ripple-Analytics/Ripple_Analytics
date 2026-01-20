#!/bin/bash
###############################################################################
# DEBUG LOGGER - Pushes verbose logs to GitHub for remote debugging
#
# This script captures all output and pushes it to a debug_logs/ folder
# in the repository so Manus can see what's happening remotely.
###############################################################################

LOG_DIR="/repo/mental_models_system/erlang-services/debug_logs"
REPO_PATH="/repo"
BRANCH="${GITHUB_BRANCH:-release2}"
MAX_LOG_SIZE=100000  # 100KB max per log file

# Ensure log directory exists
mkdir -p "$LOG_DIR"

log_to_github() {
    local log_type="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d_%H-%M-%S')
    local log_file="$LOG_DIR/${log_type}_${timestamp}.log"
    
    # Write log with full context
    cat > "$log_file" << EOF
================================================================================
DEBUG LOG: $log_type
Timestamp: $(date -Iseconds)
Hostname: $(hostname 2>/dev/null || echo "unknown")
Branch: $BRANCH
================================================================================

$message

================================================================================
SYSTEM INFO
================================================================================
Docker Version: $(docker --version 2>&1 || echo "not available")
Git Version: $(git --version 2>&1 || echo "not available")
Disk Space: $(df -h / 2>&1 | tail -1 || echo "not available")
Memory: $(free -h 2>&1 | head -2 || echo "not available")

================================================================================
RUNNING CONTAINERS
================================================================================
$(docker ps -a --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}" 2>&1 || echo "not available")

================================================================================
RECENT DOCKER LOGS (auto-updater)
================================================================================
$(docker logs mental-models-auto-updater --tail 100 2>&1 || echo "not available")

================================================================================
RECENT DOCKER LOGS (bootstrap)
================================================================================
$(docker logs mental-models-bootstrap --tail 50 2>&1 || echo "not available")

================================================================================
END OF LOG
================================================================================
EOF

    # Truncate if too large
    if [ $(wc -c < "$log_file") -gt $MAX_LOG_SIZE ]; then
        tail -c $MAX_LOG_SIZE "$log_file" > "${log_file}.tmp"
        mv "${log_file}.tmp" "$log_file"
    fi
    
    # Push to GitHub
    push_logs_to_github
}

push_logs_to_github() {
    cd "$REPO_PATH" 2>/dev/null || return 1
    
    # Configure git if needed
    git config user.email "debug-logger@mental-models.local" 2>/dev/null
    git config user.name "Debug Logger" 2>/dev/null
    
    # Add and commit logs
    git add "$LOG_DIR"/*.log 2>/dev/null
    git commit -m "debug: Auto-push debug logs $(date -Iseconds)" 2>/dev/null
    
    # Push to GitHub
    if [ -n "$GITHUB_TOKEN" ]; then
        git push origin "$BRANCH" 2>&1 || echo "Push failed - will retry"
    else
        echo "No GITHUB_TOKEN - cannot push logs"
    fi
    
    # Clean up old logs (keep last 10)
    ls -t "$LOG_DIR"/*.log 2>/dev/null | tail -n +11 | xargs rm -f 2>/dev/null
}

capture_build_output() {
    local service="$1"
    local output
    output=$(docker-compose build --no-cache "$service" 2>&1)
    local exit_code=$?
    
    log_to_github "build_${service}" "Exit Code: $exit_code

BUILD OUTPUT:
$output"
    
    return $exit_code
}

capture_startup_output() {
    local service="$1"
    local output
    output=$(docker-compose up -d "$service" 2>&1)
    local exit_code=$?
    
    # Wait a moment then capture logs
    sleep 5
    local logs=$(docker logs "mental-models-${service}" --tail 200 2>&1)
    
    log_to_github "startup_${service}" "Exit Code: $exit_code

STARTUP OUTPUT:
$output

CONTAINER LOGS:
$logs"
    
    return $exit_code
}

# Export functions for use by other scripts
export -f log_to_github
export -f push_logs_to_github
export -f capture_build_output
export -f capture_startup_output
export LOG_DIR
export REPO_PATH
export BRANCH
