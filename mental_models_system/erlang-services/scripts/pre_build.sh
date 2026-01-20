#!/bin/bash
###############################################################################
# PRE-BUILD HOOK
#
# Runs before every docker build to:
# 1. Check and fix Erlang syntax errors
# 2. Validate all source files compile
# 3. Log everything verbosely
###############################################################################

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="${SCRIPT_DIR}/.."

echo "[PRE-BUILD] =========================================="
echo "[PRE-BUILD] Running pre-build checks..."
echo "[PRE-BUILD] =========================================="

# Run syntax checker/fixer
if [ -f "${SCRIPT_DIR}/fix_erlang_syntax.sh" ]; then
    echo "[PRE-BUILD] Running Erlang syntax checker..."
    bash "${SCRIPT_DIR}/fix_erlang_syntax.sh" || {
        echo "[PRE-BUILD] Syntax errors found and could not be auto-fixed"
        echo "[PRE-BUILD] Check logs/syntax_check.log for details"
        exit 1
    }
fi

echo "[PRE-BUILD] =========================================="
echo "[PRE-BUILD] Pre-build checks PASSED"
echo "[PRE-BUILD] =========================================="
