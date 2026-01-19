#!/bin/bash
# =============================================================================
# Pre-Release Validation Script for Mental Models Desktop
# =============================================================================
# Run this BEFORE creating any release to catch bugs that would crash the app.
#
# Usage: ./scripts/validate-release.sh
#
# This script checks for:
# 1. Clojure syntax errors (via clj-kondo)
# 2. Variable shadowing bugs (Integer cannot be cast to IFn)
# 3. Regex escape issues
# 4. Static field reference errors
# 5. Common dangerous patterns
# =============================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}       MENTAL MODELS DESKTOP - PRE-RELEASE VALIDATION          ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

ERRORS=0
WARNINGS=0

# =============================================================================
# Check 1: clj-kondo installation
# =============================================================================
echo -e "${BLUE}[1/6] Checking clj-kondo installation...${NC}"

if ! command -v clj-kondo &> /dev/null; then
    echo -e "${YELLOW}⚠️  clj-kondo not installed. Installing...${NC}"
    curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
    chmod +x install-clj-kondo
    sudo ./install-clj-kondo
    rm -f install-clj-kondo
fi
echo -e "${GREEN}✓ clj-kondo installed: $(clj-kondo --version)${NC}"
echo ""

# =============================================================================
# Check 2: Syntax errors in all Clojure files
# =============================================================================
echo -e "${BLUE}[2/6] Running clj-kondo static analysis...${NC}"

# Find all directories with Clojure source files
DIRS_TO_CHECK=(
    "desktop/src"
    "Mental_Models_build/src"
    "src"
)

for dir in "${DIRS_TO_CHECK[@]}"; do
    if [ -d "$dir" ]; then
        echo "  Checking $dir..."
        
        # Run clj-kondo and capture errors
        CLJ_ERRORS=$(clj-kondo --lint "$dir" 2>&1 | grep -E "error:" || true)
        
        if [ -n "$CLJ_ERRORS" ]; then
            echo -e "${RED}❌ ERRORS in $dir:${NC}"
            echo "$CLJ_ERRORS"
            ERRORS=1
        else
            echo -e "${GREEN}  ✓ No errors in $dir${NC}"
        fi
    fi
done
echo ""

# =============================================================================
# Check 3: Variable shadowing (the bug that caused hours of debugging!)
# =============================================================================
echo -e "${BLUE}[3/6] Checking for variable shadowing bugs...${NC}"

# These are the EXACT patterns that cause "Integer cannot be cast to IFn"
# Note: 'name' is excluded because it's commonly used as a parameter name
# and doesn't typically cause issues in that context
SHADOW_PATTERNS=(
    '\[count\s'
    '\[map\s'
    '\[filter\s'
    '\[reduce\s'
    '\[first\s'
    '\[rest\s'
    '\[str\s'
    '\[get\s'
    '\[set\s'
)

for pattern in "${SHADOW_PATTERNS[@]}"; do
    MATCHES=$(grep -rn "$pattern" --include="*.clj" --include="*.cljs" --include="*.cljc" . 2>/dev/null | grep -v test | grep -v target | grep -v ".git" || true)
    
    if [ -n "$MATCHES" ]; then
        VAR_NAME=$(echo "$pattern" | sed 's/\\\[//' | sed 's/\\s//')
        echo -e "${RED}❌ Variable '$VAR_NAME' shadows clojure.core/$VAR_NAME:${NC}"
        echo "$MATCHES"
        echo -e "${YELLOW}   This causes 'Integer cannot be cast to IFn' errors!${NC}"
        echo ""
        ERRORS=1
    fi
done

if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}✓ No variable shadowing detected${NC}"
fi
echo ""

# =============================================================================
# Check 4: Regex escape issues
# =============================================================================
echo -e "${BLUE}[4/6] Checking for regex escape issues...${NC}"

# Find re-pattern with unescaped \s, \d, \w
REGEX_ISSUES=$(grep -rn 're-pattern.*str.*\\[sdwSDW]\*' --include="*.clj" . 2>/dev/null | grep -v '\\\\[sdwSDW]' | grep -v test | grep -v target || true)

if [ -n "$REGEX_ISSUES" ]; then
    echo -e "${RED}❌ Unescaped regex metacharacters in re-pattern:${NC}"
    echo "$REGEX_ISSUES"
    echo -e "${YELLOW}   Use \\\\\\\\s not \\\\s inside (re-pattern (str ...))${NC}"
    ERRORS=1
else
    echo -e "${GREEN}✓ No regex escape issues${NC}"
fi
echo ""

# =============================================================================
# Check 5: Static field reference errors
# =============================================================================
echo -e "${BLUE}[5/6] Checking for static field reference errors...${NC}"

# Find static fields called with parens
STATIC_ISSUES=$(grep -rn '(DateTimeFormatter/ISO_LOCAL_DATE_TIME)' --include="*.clj" . 2>/dev/null | grep -v test || true)

if [ -n "$STATIC_ISSUES" ]; then
    echo -e "${RED}❌ Static fields called as functions:${NC}"
    echo "$STATIC_ISSUES"
    echo -e "${YELLOW}   Use DateTimeFormatter/ISO_LOCAL_DATE_TIME not (DateTimeFormatter/ISO_LOCAL_DATE_TIME)${NC}"
    ERRORS=1
else
    echo -e "${GREEN}✓ No static field reference errors${NC}"
fi
echo ""

# =============================================================================
# Check 6: Version consistency
# =============================================================================
echo -e "${BLUE}[6/6] Checking version consistency...${NC}"

# Find all version declarations
VERSIONS=$(grep -rh ':version\s*"v[0-9]' --include="*.clj" . 2>/dev/null | grep -v test | sort -u || true)

if [ -n "$VERSIONS" ]; then
    VERSION_COUNT=$(echo "$VERSIONS" | wc -l)
    if [ "$VERSION_COUNT" -gt 1 ]; then
        echo -e "${YELLOW}⚠️  Multiple version strings found:${NC}"
        echo "$VERSIONS"
        WARNINGS=1
    else
        echo -e "${GREEN}✓ Version: $(echo "$VERSIONS" | head -1)${NC}"
    fi
else
    echo -e "${YELLOW}⚠️  No version string found${NC}"
    WARNINGS=1
fi
echo ""

# =============================================================================
# Summary
# =============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"

if [ $ERRORS -eq 1 ]; then
    echo -e "${RED}❌ VALIDATION FAILED - DO NOT RELEASE!${NC}"
    echo ""
    echo "Fix the errors above before creating a release."
    echo "These bugs WILL crash the application at runtime."
    echo ""
    exit 1
elif [ $WARNINGS -eq 1 ]; then
    echo -e "${YELLOW}⚠️  VALIDATION PASSED WITH WARNINGS${NC}"
    echo ""
    echo "Review the warnings above before releasing."
    echo ""
    exit 0
else
    echo -e "${GREEN}✅ ALL VALIDATION CHECKS PASSED!${NC}"
    echo ""
    echo "Safe to create a release."
    echo ""
    exit 0
fi
