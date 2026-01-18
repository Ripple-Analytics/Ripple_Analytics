#!/bin/bash
# Mental Models Desktop App - Mac/Linux Launcher
# Requires: Java 17+, Clojure CLI

echo ""
echo "============================================"
echo "  Mental Models Desktop Analyzer"
echo "============================================"
echo ""

# Check if Clojure is installed
if ! command -v clojure &> /dev/null; then
    echo "ERROR: Clojure CLI not found!"
    echo ""
    echo "Please install Clojure:"
    echo "  Mac:   brew install clojure/tools/clojure"
    echo "  Linux: curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh"
    echo "         chmod +x linux-install.sh && sudo bash linux-install.sh"
    echo ""
    exit 1
fi

# Check if Java is installed
if ! command -v java &> /dev/null; then
    echo "ERROR: Java not found!"
    echo ""
    echo "Please install Java 17 or later:"
    echo "  Mac:   brew install openjdk@17"
    echo "  Linux: sudo apt install openjdk-17-jdk"
    echo ""
    exit 1
fi

# Run the app
if [ -z "$1" ]; then
    echo "Usage: ./run.sh <command> [options]"
    echo ""
    echo "Commands:"
    echo "  scan <folder>   - Scan a folder for mental models"
    echo "  watch <folder>  - Watch a folder for new files"
    echo "  status          - Show analysis statistics"
    echo "  help            - Show help"
    echo ""
    echo "Example:"
    echo "  ./run.sh scan ~/Documents/Research"
    echo ""
else
    clojure -M:run "$@"
fi
