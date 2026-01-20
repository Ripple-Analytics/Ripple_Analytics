@echo off
setlocal enabledelayedexpansion

echo.
echo ============================================================
echo   Mental Models System - One-Click Startup
echo   Bulletproof Update System with Multiple Backup Sources
echo ============================================================
echo.

cd /d "%~dp0"

:: Check if Docker is running
docker info >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Docker is not running. Please start Docker Desktop first.
    echo.
    pause
    exit /b 1
)

echo [OK] Docker is running
echo.

:: Check if .env file exists, create if not
if not exist ".env" (
    echo [INFO] Creating .env file...
    echo # Mental Models System Configuration > .env
    echo # Add your GitHub token for private repo access >> .env
    echo GITHUB_TOKEN= >> .env
    echo # Add your Google Drive backup URL ^(optional^) >> .env
    echo GDRIVE_BACKUP_URL= >> .env
    echo.
    echo [WARN] Edit .env file to add GITHUB_TOKEN for private repo access.
    echo.
)

:: Pull latest changes if this is a git repo
echo [1/5] Checking for updates...
cd ..\..
git fetch origin 2>nul
git reset --hard origin/release 2>nul
if errorlevel 1 (
    echo [WARN] Git update failed, using local files...
) else (
    echo [OK] Code updated from GitHub
)
cd mental_models_system\erlang-services
echo.

:: Stop old containers
echo [2/5] Stopping old containers...
docker-compose down 2>nul
echo.

:: Build all services
echo [3/5] Building all services...
echo       This may take 10-15 minutes on first run...
echo.
docker-compose build --parallel 2>nul
if errorlevel 1 (
    echo [INFO] Retrying build without parallel...
    docker-compose build
)
if errorlevel 1 (
    echo [ERROR] Build failed. Check error messages above.
    pause
    exit /b 1
)
echo [OK] All services built
echo.

:: Start all services
echo [4/5] Starting all services...
docker-compose up -d
if errorlevel 1 (
    echo [ERROR] Failed to start services.
    pause
    exit /b 1
)
echo [OK] All services started
echo.

:: Wait for services to initialize
echo [5/5] Waiting for services to initialize...
timeout /t 10 /nobreak >nul
echo.

echo ============================================================
echo   SUCCESS! All services are running.
echo ============================================================
echo.
echo   MAIN DASHBOARD:
echo     http://localhost:3000
echo.
echo   UPDATE SOURCES (Bulletproof Fallback Chain):
echo     1. GitHub Source:  http://localhost:8010/health
echo     2. GDrive Source:  http://localhost:8011/health
echo     3. Local Cache:    http://localhost:8012/health
echo.
echo   TESTING:
echo     Chaos Monkey:      http://localhost:8013/health
echo.
echo   CORE SERVICES:
echo     API Gateway:       http://localhost:8000/health
echo     Auto-Updater:      http://localhost:8006/health
echo.
echo   COMMANDS:
echo     View logs:         docker-compose logs -f
echo     Stop all:          docker-compose down
echo     Restart:           docker-compose restart
echo.
echo ============================================================
echo.

:: Open the dashboard in browser
start http://localhost:3000

echo Press any key to view live logs (Ctrl+C to exit)...
pause >nul
docker-compose logs -f
