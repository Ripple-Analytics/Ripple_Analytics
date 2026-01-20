#!/bin/bash
# Mental Models System - Auto Updater Service
# Checks GitHub for updates every 5 minutes, with Google Drive as fallback
# Creates GitHub issue to alert Devin when GitHub fails

set -e

# Configuration
REPO_URL="${GITHUB_REPO_URL:-https://github.com/Ripple-Analytics/Ripple_Analytics.git}"
BRANCH="${GITHUB_BRANCH:-master}"
CHECK_INTERVAL="${UPDATE_CHECK_INTERVAL:-300}"  # 5 minutes default
GDRIVE_BACKUP_URL="${GDRIVE_BACKUP_URL:-}"  # Optional Google Drive backup URL
GITHUB_TOKEN="${GITHUB_TOKEN:-}"  # GitHub token for creating issues
GITHUB_REPO="${GITHUB_REPO:-Ripple-Analytics/Ripple_Analytics}"  # Repo for issues
ALERT_WEBHOOK_URL="${ALERT_WEBHOOK_URL:-}"  # Optional webhook for alerts
REPO_PATH="/repo"
SERVICES_PATH="/repo/mental_models_system/erlang-services"
STATUS_FILE="/data/updater_status.json"
GITHUB_FAILURE_COUNT=0
LAST_ALERT_TIME=0

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Write status to file for UI to read
write_status() {
    local status="$1"
    local message="$2"
    local github_ok="$3"
    
    mkdir -p /data
    cat > "$STATUS_FILE" << EOF
{
    "status": "$status",
    "message": "$message",
    "github_available": $github_ok,
    "last_check": "$(date -Iseconds)",
    "github_failure_count": $GITHUB_FAILURE_COUNT,
    "update_source": "${UPDATE_SOURCE:-github}"
}
EOF
}

# Create GitHub issue to alert about GitHub failure
create_github_alert_issue() {
    if [ -z "$GITHUB_TOKEN" ]; then
        log "WARNING: No GITHUB_TOKEN configured - cannot create alert issue"
        log "Please set GITHUB_TOKEN environment variable to enable automatic alerts"
        return 1
    fi
    
    # Only create issue once per hour to avoid spam
    CURRENT_TIME=$(date +%s)
    TIME_DIFF=$((CURRENT_TIME - LAST_ALERT_TIME))
    if [ $TIME_DIFF -lt 3600 ]; then
        log "Skipping alert - last alert was less than 1 hour ago"
        return 0
    fi
    
    log "Creating GitHub issue to alert about GitHub failure..."
    
    ISSUE_TITLE="[AUTO-ALERT] GitHub Update Failed - Using Google Drive Fallback"
    ISSUE_BODY="## Automatic Alert from Mental Models Auto-Updater

**Time:** $(date -Iseconds)
**Status:** GitHub connection failed, using Google Drive backup

### Details
- GitHub URL: $REPO_URL
- Branch: $BRANCH
- Failure Count: $GITHUB_FAILURE_COUNT
- Fallback: Google Drive backup was used successfully

### Action Required
Please investigate why GitHub is not accessible:
1. Check if the repository is available
2. Check network connectivity
3. Check if there are any GitHub outages

### For Devin
If you see this issue, please:
1. Check the GitHub repository status
2. Verify the auto-updater configuration
3. Fix any issues preventing GitHub access

---
*This issue was automatically created by the Mental Models Auto-Updater service.*"

    # Create issue via GitHub API
    RESPONSE=$(curl -s -X POST \
        -H "Authorization: token $GITHUB_TOKEN" \
        -H "Accept: application/vnd.github.v3+json" \
        "https://api.github.com/repos/$GITHUB_REPO/issues" \
        -d "{\"title\": \"$ISSUE_TITLE\", \"body\": $(echo "$ISSUE_BODY" | jq -Rs .), \"labels\": [\"auto-alert\", \"infrastructure\"]}" \
        2>/dev/null) || {
        log "Failed to create GitHub issue"
        return 1
    }
    
    ISSUE_URL=$(echo "$RESPONSE" | jq -r '.html_url // empty')
    if [ -n "$ISSUE_URL" ]; then
        log "GitHub alert issue created: $ISSUE_URL"
        LAST_ALERT_TIME=$CURRENT_TIME
    else
        log "Failed to create GitHub issue: $RESPONSE"
    fi
}

# Send webhook alert
send_webhook_alert() {
    if [ -z "$ALERT_WEBHOOK_URL" ]; then
        return 0
    fi
    
    log "Sending webhook alert..."
    
    PAYLOAD="{
        \"event\": \"github_failure\",
        \"timestamp\": \"$(date -Iseconds)\",
        \"message\": \"GitHub update failed, using Google Drive fallback\",
        \"details\": {
            \"repo_url\": \"$REPO_URL\",
            \"branch\": \"$BRANCH\",
            \"failure_count\": $GITHUB_FAILURE_COUNT
        }
    }"
    
    curl -s -X POST \
        -H "Content-Type: application/json" \
        -d "$PAYLOAD" \
        "$ALERT_WEBHOOK_URL" 2>/dev/null || log "Webhook alert failed"
}

# Alert when GitHub fails and Google Drive is used
alert_github_failure() {
    GITHUB_FAILURE_COUNT=$((GITHUB_FAILURE_COUNT + 1))
    
    log "=========================================="
    log "ALERT: GitHub update failed!"
    log "Using Google Drive backup as fallback."
    log "Failure count: $GITHUB_FAILURE_COUNT"
    log "=========================================="
    
    # Write failure status
    write_status "fallback" "GitHub failed, using Google Drive backup" "false"
    
    # Create GitHub issue to alert
    create_github_alert_issue
    
    # Send webhook if configured
    send_webhook_alert
}

# Initialize repository if not exists
init_repo() {
    if [ ! -d "$REPO_PATH/.git" ]; then
        log "Cloning repository from GitHub..."
        # Disable credential prompts for public repos
        export GIT_TERMINAL_PROMPT=0
        git clone --depth 1 --branch "$BRANCH" "$REPO_URL" "$REPO_PATH" 2>&1 || return 1
    fi
    return 0
}

# Check for updates from GitHub
check_github_updates() {
    log "Checking GitHub for updates..."
    cd "$REPO_PATH"
    
    # Disable credential prompts for public repos
    export GIT_TERMINAL_PROMPT=0
    
    # Fetch latest changes
    git fetch origin "$BRANCH" --depth 1 2>/dev/null || return 1
    
    # Check if there are new commits
    LOCAL_HASH=$(git rev-parse HEAD)
    REMOTE_HASH=$(git rev-parse "origin/$BRANCH")
    
    if [ "$LOCAL_HASH" != "$REMOTE_HASH" ]; then
        log "New updates available on GitHub!"
        return 0
    else
        log "No updates available."
        return 1
    fi
}

# Pull updates from GitHub
pull_github_updates() {
    log "Pulling updates from GitHub..."
    cd "$REPO_PATH"
    
    # Disable credential prompts for public repos
    export GIT_TERMINAL_PROMPT=0
    
    git reset --hard "origin/$BRANCH" || return 1
    git pull origin "$BRANCH" --depth 1 || return 1
    
    log "GitHub update successful!"
    return 0
}

# Download from Google Drive as fallback
download_from_gdrive() {
    if [ -z "$GDRIVE_BACKUP_URL" ]; then
        log "No Google Drive backup URL configured."
        return 1
    fi
    
    log "Attempting to download from Google Drive backup..."
    
    # Extract file ID from Google Drive URL
    FILE_ID=$(echo "$GDRIVE_BACKUP_URL" | grep -oP '(?<=id=)[^&]+|(?<=d/)[^/]+')
    
    if [ -z "$FILE_ID" ]; then
        # Try direct URL
        DOWNLOAD_URL="$GDRIVE_BACKUP_URL"
    else
        # Construct download URL
        DOWNLOAD_URL="https://drive.google.com/uc?export=download&id=$FILE_ID"
    fi
    
    # Download the backup
    TEMP_ZIP="/tmp/backup.zip"
    curl -L -o "$TEMP_ZIP" "$DOWNLOAD_URL" 2>/dev/null || return 1
    
    # Verify it's a valid zip
    if ! unzip -t "$TEMP_ZIP" >/dev/null 2>&1; then
        log "Downloaded file is not a valid zip archive."
        rm -f "$TEMP_ZIP"
        return 1
    fi
    
    # Extract to repo path
    log "Extracting Google Drive backup..."
    rm -rf "$REPO_PATH"
    mkdir -p "$REPO_PATH"
    unzip -o "$TEMP_ZIP" -d "$REPO_PATH" || return 1
    rm -f "$TEMP_ZIP"
    
    # Handle nested directory structure
    if [ -d "$REPO_PATH/Mental_Models" ]; then
        mv "$REPO_PATH/Mental_Models"/* "$REPO_PATH/" 2>/dev/null || true
        rm -rf "$REPO_PATH/Mental_Models"
    fi
    
    log "Google Drive backup restored successfully!"
    return 0
}

# Rebuild and restart services
rebuild_services() {
    log "Rebuilding services..."
    
    if [ ! -d "$SERVICES_PATH" ]; then
        log "Services path not found: $SERVICES_PATH"
        return 1
    fi
    
    cd "$SERVICES_PATH"
    
    # Rebuild containers (excluding the updater itself)
    docker-compose build --no-cache api-gateway analysis-service harvester-service storage-service chaos-engineering desktop-ui 2>/dev/null || {
        log "Warning: Could not rebuild some services"
    }
    
    # Restart services
    docker-compose up -d api-gateway analysis-service harvester-service storage-service chaos-engineering desktop-ui 2>/dev/null || {
        log "Warning: Could not restart some services"
    }
    
    log "Services rebuilt and restarted!"
    return 0
}

# Main update function
perform_update() {
    UPDATE_SOURCE="github"
    
    # Try GitHub first
    if pull_github_updates; then
        write_status "ok" "Updated from GitHub" "true"
        GITHUB_FAILURE_COUNT=0  # Reset failure count on success
        rebuild_services
        return 0
    fi
    
    log "GitHub update failed, trying Google Drive backup..."
    UPDATE_SOURCE="gdrive"
    
    # Try Google Drive as fallback
    if download_from_gdrive; then
        # ALERT: GitHub failed, using Google Drive fallback
        alert_github_failure
        rebuild_services
        return 0
    fi
    
    log "All update sources failed."
    write_status "error" "All update sources failed" "false"
    return 1
}

# Main loop
main() {
    log "=========================================="
    log "Mental Models Auto-Updater Starting"
    log "=========================================="
    log "GitHub Repo: $REPO_URL"
    log "Branch: $BRANCH"
    log "Check Interval: ${CHECK_INTERVAL}s"
    log "Google Drive Backup: ${GDRIVE_BACKUP_URL:-Not configured}"
    log "=========================================="
    
    # Initialize repository
    if ! init_repo; then
        log "Failed to initialize repository from GitHub, trying Google Drive..."
        download_from_gdrive || log "Could not initialize from any source"
    fi
    
    # Main update loop
    while true; do
        log "Checking for updates..."
        
        if check_github_updates; then
            perform_update
        fi
        
        log "Next check in ${CHECK_INTERVAL} seconds..."
        sleep "$CHECK_INTERVAL"
    done
}

# Handle signals for graceful shutdown
trap 'log "Shutting down auto-updater..."; exit 0' SIGTERM SIGINT

main
