#!/bin/bash
###############################################################################
# AUTOMATED ERLANG SYNTAX CHECKER AND FIXER
#
# This script:
# 1. Scans all .erl files for syntax errors
# 2. Attempts to auto-fix common issues (unescaped quotes, etc.)
# 3. Reports results with verbose logging
# 4. Can be run as part of the build pipeline
###############################################################################

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="${SCRIPT_DIR}/.."
LOG_FILE="${BASE_DIR}/logs/syntax_check.log"
FIXED_COUNT=0
ERROR_COUNT=0

mkdir -p "${BASE_DIR}/logs"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

log "=========================================="
log "ERLANG SYNTAX CHECKER STARTING"
log "=========================================="

# Find all Erlang source files
find "$BASE_DIR" -name "*.erl" -type f | while read -r file; do
    # Skip _build directories
    if [[ "$file" == *"_build"* ]]; then
        continue
    fi
    
    log "Checking: $file"
    
    # Run erlc syntax check
    ERRORS=$(erlc -o /tmp +debug_info "$file" 2>&1 | grep -E "error|Error" || true)
    
    if [ -n "$ERRORS" ]; then
        log "ERRORS FOUND in $file:"
        log "$ERRORS"
        
        # Attempt auto-fix for common issues
        log "Attempting auto-fix..."
        
        # Fix 1: Unescaped double quotes in strings (common in HTML/JS embedded in Erlang)
        # Look for patterns like class="..." and style="..." inside Erlang strings
        sed -i 's/class="\([^"]*\)"/class=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/style="\([^"]*\)"/style=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/id="\([^"]*\)"/id=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/href="\([^"]*\)"/href=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/src="\([^"]*\)"/src=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/type="\([^"]*\)"/type=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/name="\([^"]*\)"/name=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/value="\([^"]*\)"/value=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/onclick="\([^"]*\)"/onclick=\\"\1\\"/g' "$file" 2>/dev/null || true
        sed -i 's/onchange="\([^"]*\)"/onchange=\\"\1\\"/g' "$file" 2>/dev/null || true
        
        # Re-check after fix
        ERRORS_AFTER=$(erlc -o /tmp +debug_info "$file" 2>&1 | grep -E "error|Error" || true)
        
        if [ -z "$ERRORS_AFTER" ]; then
            log "AUTO-FIX SUCCESSFUL for $file"
            FIXED_COUNT=$((FIXED_COUNT + 1))
        else
            log "AUTO-FIX FAILED - manual intervention needed for $file"
            log "Remaining errors: $ERRORS_AFTER"
            ERROR_COUNT=$((ERROR_COUNT + 1))
        fi
    else
        log "OK: $file"
    fi
done

log "=========================================="
log "SYNTAX CHECK COMPLETE"
log "Fixed: $FIXED_COUNT files"
log "Errors remaining: $ERROR_COUNT files"
log "=========================================="

# Exit with error if any files still have errors
if [ "$ERROR_COUNT" -gt 0 ]; then
    exit 1
fi

exit 0
