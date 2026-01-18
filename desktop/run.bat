@echo off
REM Mental Models Desktop App - Windows Launcher
REM Requires: Java 17+, Clojure CLI

echo.
echo ============================================
echo   Mental Models Desktop Analyzer
echo ============================================
echo.

REM Check if Clojure is installed
where clojure >nul 2>nul
if %ERRORLEVEL% neq 0 (
    echo ERROR: Clojure CLI not found!
    echo.
    echo Please install Clojure:
    echo   1. Download from: https://clojure.org/guides/install_clojure
    echo   2. Or use: winget install Cognitect.Clojure
    echo.
    pause
    exit /b 1
)

REM Check if Java is installed
where java >nul 2>nul
if %ERRORLEVEL% neq 0 (
    echo ERROR: Java not found!
    echo.
    echo Please install Java 17 or later:
    echo   winget install Microsoft.OpenJDK.17
    echo.
    pause
    exit /b 1
)

REM Run the app
if "%1"=="" (
    echo Usage: run.bat ^<command^> [options]
    echo.
    echo Commands:
    echo   scan ^<folder^>   - Scan a folder for mental models
    echo   watch ^<folder^>  - Watch a folder for new files
    echo   status          - Show analysis statistics
    echo   help            - Show help
    echo.
    echo Example:
    echo   run.bat scan "C:\Users\YourName\Documents\Research"
    echo.
) else (
    clojure -M:run %*
)
