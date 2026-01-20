#!/bin/bash
# Mental Models System - Auto Updater Service
# Bulletproof update chain with multiple fallback sources:
# 1. SSH (private repo with deploy key) - Primary
# 2. HTTPS with token (authenticated) - Fallback 1
# 3. HTTPS public (if repo is public) - Fallback 2
# 4. Google Drive backup - Fallback 3
# Creates GitHub issue to alert Devin when primary sources fail

set -e

# Configuration
REPO_SSH_URL="${GITHUB_SSH_URL:-git@github.com:Ripple-Analytics/Ripple_Analytics.git}"
REPO_HTTPS_URL="${GITHUB_REPO_URL:-https://github.com/Ripple-Analytics/Ripple_Analytics.git}"
BRANCH="${GITHUB_BRANCH:-master}"
CHECK_INTERVAL="${UPDATE_CHECK_INTERVAL:-300}"  # 5 minutes default
GDRIVE_BACKUP_URL="${GDRIVE_BACKUP_URL:-}"  # Optional Google Drive backup URL
GITHUB_TOKEN="${GITHUB_TOKEN:-}"  # GitHub token for authenticated HTTPS and creating issues
GITHUB_REPO="${GITHUB_REPO:-Ripple-Analytics/Ripple_Analytics}"  # Repo for issues
ALERT_WEBHOOK_URL="${ALERT_WEBHOOK_URL:-}"  # Optional webhook for alerts
REPO_PATH="/repo"
SERVICES_PATH="/repo/mental_models_system/erlang-services"
STATUS_FILE="/data/updater_status.json"
SSH_KEY_PATH="${SSH_KEY_PATH:-/ssh/deploy_key}"
GITHUB_FAILURE_COUNT=0
LAST_ALERT_TIME=0
CURRENT_SOURCE=""

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
    "update_source": "${CURRENT_SOURCE:-unknown}",
    "sources_tried": "${SOURCES_TRIED:-}"
}
EOF
}

# Setup SSH for GitHub
setup_ssh() {
    if [ -f "$SSH_KEY_PATH" ]; then
        log "Setting up SSH authentication..."
        mkdir -p ~/.ssh
        cp "$SSH_KEY_PATH" ~/.ssh/id_rsa
        chmod 600 ~/.ssh/id_rsa
        
        # Add GitHub to known hosts
        ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts 2>/dev/null
        
        # Configure git to use SSH
        git config --global core.sshCommand "ssh -i ~/.ssh/id_rsa -o StrictHostKeyChecking=no"
        
        log "SSH authentication configured."
        return 0
    else
        log "No SSH key found at $SSH_KEY_PATH"
        return 1
    fi
}

# Get authenticated HTTPS URL
get_authenticated_https_url() {
    if [ -n "$GITHUB_TOKEN" ]; then
        echo "https://${GITHUB_TOKEN}@github.com/${GITHUB_REPO}.git"
    else
        echo ""
    fi
}

# Create GitHub issue to alert about failures
create_github_alert_issue() {
    if [ -z "$GITHUB_TOKEN" ]; then
        log "WARNING: No GITHUB_TOKEN configured - cannot create alert issue"
        return 1
    fi
    
    # Only create issue once per hour to avoid spam
    CURRENT_TIME=$(date +%s)
    TIME_DIFF=$((CURRENT_TIME - LAST_ALERT_TIME))
    if [ $TIME_DIFF -lt 3600 ]; then
        log "Skipping alert - last alert was less than 1 hour ago"
        return 0
    fi
    
    log "Creating GitHub issue to alert about update source degradation..."
    
    ISSUE_TITLE="[AUTO-ALERT] Primary Update Sources Failed - Using Fallback"
    ISSUE_BODY="## Automatic Alert from Mental Models Auto-Updater

**Time:** $(date -Iseconds)
**Status:** Primary update sources failed, using fallback
**Current Source:** $CURRENT_SOURCE
**Sources Tried:** $SOURCES_TRIED

### Details
- SSH URL: $REPO_SSH_URL
- HTTPS URL: $REPO_HTTPS_URL
- Branch: $BRANCH
- Failure Count: $GITHUB_FAILURE_COUNT

### Action Required
Please investigate why primary sources are not accessible:
1. Check SSH deploy key configuration
2. Check GitHub token validity
3. Check if the repository is available
4. Check network connectivity

### For Devin
If you see this issue, please:
1. Verify the auto-updater configuration
2. Check deploy key and token settings
3. Fix any issues preventing primary source access

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
        \"event\": \"update_source_degradation\",
        \"timestamp\": \"$(date -Iseconds)\",
        \"message\": \"Primary update sources failed, using fallback\",
        \"details\": {
            \"current_source\": \"$CURRENT_SOURCE\",
            \"sources_tried\": \"$SOURCES_TRIED\",
            \"branch\": \"$BRANCH\",
            \"failure_count\": $GITHUB_FAILURE_COUNT
        }
    }"
    
    curl -s -X POST \
        -H "Content-Type: application/json" \
        -d "$PAYLOAD" \
        "$ALERT_WEBHOOK_URL" 2>/dev/null || log "Webhook alert failed"
}

# Alert when primary sources fail
alert_source_degradation() {
    GITHUB_FAILURE_COUNT=$((GITHUB_FAILURE_COUNT + 1))
    
    log "=========================================="
    log "ALERT: Primary update sources failed!"
    log "Using fallback source: $CURRENT_SOURCE"
    log "Sources tried: $SOURCES_TRIED"
    log "Failure count: $GITHUB_FAILURE_COUNT"
    log "=========================================="
    
    # Write failure status
    write_status "fallback" "Using fallback source: $CURRENT_SOURCE" "false"
    
    # Create GitHub issue to alert (if we have a token)
    create_github_alert_issue
    
    # Send webhook if configured
    send_webhook_alert
}

# Try to clone/init via SSH
init_via_ssh() {
    if [ ! -f "$SSH_KEY_PATH" ]; then
        return 1
    fi
    
    setup_ssh || return 1
    
    log "Attempting to clone via SSH..."
    git clone --depth 1 --branch "$BRANCH" "$REPO_SSH_URL" "$REPO_PATH" 2>/dev/null
    return $?
}

# Try to clone/init via authenticated HTTPS
init_via_https_auth() {
    if [ -z "$GITHUB_TOKEN" ]; then
        return 1
    fi
    
    AUTH_URL=$(get_authenticated_https_url)
    log "Attempting to clone via authenticated HTTPS..."
    git clone --depth 1 --branch "$BRANCH" "$AUTH_URL" "$REPO_PATH" 2>/dev/null
    return $?
}

# Try to clone/init via public HTTPS
init_via_https_public() {
    log "Attempting to clone via public HTTPS..."
    git clone --depth 1 --branch "$BRANCH" "$REPO_HTTPS_URL" "$REPO_PATH" 2>/dev/null
    return $?
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

# Initialize repository with fallback chain
init_repo() {
    if [ -d "$REPO_PATH/.git" ]; then
        log "Repository already initialized."
        return 0
    fi
    
    SOURCES_TRIED=""
    
    # Try SSH first (private repo with deploy key)
    SOURCES_TRIED="ssh"
    if init_via_ssh; then
        CURRENT_SOURCE="ssh"
        log "Repository initialized via SSH."
        return 0
    fi
    log "SSH clone failed."
    
    # Try authenticated HTTPS
    SOURCES_TRIED="$SOURCES_TRIED,https-auth"
    if init_via_https_auth; then
        CURRENT_SOURCE="https-auth"
        log "Repository initialized via authenticated HTTPS."
        return 0
    fi
    log "Authenticated HTTPS clone failed."
    
    # Try public HTTPS
    SOURCES_TRIED="$SOURCES_TRIED,https-public"
    if init_via_https_public; then
        CURRENT_SOURCE="https-public"
        log "Repository initialized via public HTTPS."
        return 0
    fi
    log "Public HTTPS clone failed."
    
    # Try Google Drive
    SOURCES_TRIED="$SOURCES_TRIED,gdrive"
    if download_from_gdrive; then
        CURRENT_SOURCE="gdrive"
        log "Repository initialized via Google Drive backup."
        alert_source_degradation
        return 0
    fi
    log "Google Drive backup failed."
    
    log "CRITICAL: All initialization sources failed!"
    write_status "error" "All initialization sources failed" "false"
    return 1
}

# Check for updates from current remote
check_updates() {
    log "Checking for updates..."
    cd "$REPO_PATH"
    
    # Determine which remote URL to use based on what worked during init
    case "$CURRENT_SOURCE" in
        ssh)
            setup_ssh 2>/dev/null
            git remote set-url origin "$REPO_SSH_URL" 2>/dev/null
            ;;
        https-auth)
            AUTH_URL=$(get_authenticated_https_url)
            git remote set-url origin "$AUTH_URL" 2>/dev/null
            ;;
        https-public|*)
            git remote set-url origin "$REPO_HTTPS_URL" 2>/dev/null
            ;;
    esac
    
    # Fetch latest changes
    if ! git fetch origin "$BRANCH" --depth 1 2>/dev/null; then
        log "Fetch failed with current source, trying fallbacks..."
        return 2  # Signal to try fallbacks
    fi
    
    # Check if there are new commits
    LOCAL_HASH=$(git rev-parse HEAD)
    REMOTE_HASH=$(git rev-parse "origin/$BRANCH")
    
    if [ "$LOCAL_HASH" != "$REMOTE_HASH" ]; then
        log "New updates available!"
        return 0
    else
        log "No updates available."
        return 1
    fi
}

# Pull updates with fallback chain
pull_updates() {
    cd "$REPO_PATH"
    SOURCES_TRIED=""
    
    # Try SSH first
    if [ -f "$SSH_KEY_PATH" ]; then
        SOURCES_TRIED="ssh"
        setup_ssh 2>/dev/null
        git remote set-url origin "$REPO_SSH_URL" 2>/dev/null
        log "Trying to pull via SSH..."
        if git fetch origin "$BRANCH" --depth 1 2>/dev/null && \
           git reset --hard "origin/$BRANCH" 2>/dev/null; then
            CURRENT_SOURCE="ssh"
            log "Update successful via SSH."
            write_status "ok" "Updated via SSH (private)" "true"
            GITHUB_FAILURE_COUNT=0
            return 0
        fi
        log "SSH pull failed."
    fi
    
    # Try authenticated HTTPS
    if [ -n "$GITHUB_TOKEN" ]; then
        SOURCES_TRIED="$SOURCES_TRIED,https-auth"
        AUTH_URL=$(get_authenticated_https_url)
        git remote set-url origin "$AUTH_URL" 2>/dev/null
        log "Trying to pull via authenticated HTTPS..."
        if git fetch origin "$BRANCH" --depth 1 2>/dev/null && \
           git reset --hard "origin/$BRANCH" 2>/dev/null; then
            CURRENT_SOURCE="https-auth"
            log "Update successful via authenticated HTTPS."
            write_status "ok" "Updated via authenticated HTTPS" "true"
            GITHUB_FAILURE_COUNT=0
            return 0
        fi
        log "Authenticated HTTPS pull failed."
    fi
    
    # Try public HTTPS
    SOURCES_TRIED="$SOURCES_TRIED,https-public"
    git remote set-url origin "$REPO_HTTPS_URL" 2>/dev/null
    log "Trying to pull via public HTTPS..."
    if git fetch origin "$BRANCH" --depth 1 2>/dev/null && \
       git reset --hard "origin/$BRANCH" 2>/dev/null; then
        CURRENT_SOURCE="https-public"
        log "Update successful via public HTTPS."
        write_status "ok" "Updated via public HTTPS" "true"
        # Alert that we fell back to public
        if [ -f "$SSH_KEY_PATH" ] || [ -n "$GITHUB_TOKEN" ]; then
            alert_source_degradation
        fi
        return 0
    fi
    log "Public HTTPS pull failed."
    
    # Try Google Drive
    SOURCES_TRIED="$SOURCES_TRIED,gdrive"
    log "Trying Google Drive backup..."
    if download_from_gdrive; then
        CURRENT_SOURCE="gdrive"
        alert_source_degradation
        return 0
    fi
    
    log "CRITICAL: All update sources failed!"
    write_status "error" "All update sources failed" "false"
    return 1
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

# Main loop
main() {
    log "=========================================="
    log "Mental Models Auto-Updater Starting"
    log "=========================================="
    log "SSH URL: $REPO_SSH_URL"
    log "HTTPS URL: $REPO_HTTPS_URL"
    log "Branch: $BRANCH"
    log "Check Interval: ${CHECK_INTERVAL}s"
    log "SSH Key: ${SSH_KEY_PATH} ($([ -f "$SSH_KEY_PATH" ] && echo 'found' || echo 'not found'))"
    log "GitHub Token: $([ -n "$GITHUB_TOKEN" ] && echo 'configured' || echo 'not configured')"
    log "Google Drive Backup: ${GDRIVE_BACKUP_URL:-Not configured}"
    log "=========================================="
    log "Update Source Priority:"
    log "  1. SSH (private repo with deploy key)"
    log "  2. HTTPS with token (authenticated)"
    log "  3. HTTPS public (if repo is public)"
    log "  4. Google Drive backup"
    log "=========================================="
    
    # Initialize repository
    if ! init_repo; then
        log "CRITICAL: Could not initialize from any source!"
        log "Waiting 60 seconds before retry..."
        sleep 60
        exec "$0"  # Restart the script
    fi
    
    # Main update loop
    while true; do
        CHECK_RESULT=$(check_updates; echo $?)
        
        case "$CHECK_RESULT" in
            0)
                # Updates available
                if pull_updates; then
                    rebuild_services
                fi
                ;;
            2)
                # Fetch failed, try full fallback pull
                log "Fetch failed, attempting full fallback chain..."
                if pull_updates; then
                    rebuild_services
                fi
                ;;
            *)
                # No updates or check failed
                ;;
        esac
        
        log "Next check in ${CHECK_INTERVAL} seconds..."
        sleep "$CHECK_INTERVAL"
    done
}

# Handle signals for graceful shutdown
trap 'log "Shutting down auto-updater..."; exit 0' SIGTERM SIGINT

main
