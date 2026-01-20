#!/bin/bash
# Mental Models System - Auto Updater Service
# Checks GitHub for updates every 5 minutes, with Google Drive as fallback

set -e

# Configuration
REPO_URL="${GITHUB_REPO_URL:-https://github.com/Ripple-Analytics/Ripple_Analytics.git}"
BRANCH="${GITHUB_BRANCH:-master}"
CHECK_INTERVAL="${UPDATE_CHECK_INTERVAL:-300}"  # 5 minutes default
GDRIVE_BACKUP_URL="${GDRIVE_BACKUP_URL:-}"  # Optional Google Drive backup URL
REPO_PATH="/repo"
SERVICES_PATH="/repo/mental_models_system/erlang-services"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Initialize repository if not exists
init_repo() {
    if [ ! -d "$REPO_PATH/.git" ]; then
        log "Cloning repository from GitHub..."
        git clone --depth 1 --branch "$BRANCH" "$REPO_URL" "$REPO_PATH" || return 1
    fi
    return 0
}

# Check for updates from GitHub
check_github_updates() {
    log "Checking GitHub for updates..."
    cd "$REPO_PATH"
    
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
    # Try GitHub first
    if pull_github_updates; then
        rebuild_services
        return 0
    fi
    
    log "GitHub update failed, trying Google Drive backup..."
    
    # Try Google Drive as fallback
    if download_from_gdrive; then
        rebuild_services
        return 0
    fi
    
    log "All update sources failed."
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
