@echo off
REM ============================================================================
REM BLUE-GREEN DEPLOYMENT BOOTSTRAP SCRIPT
REM ============================================================================
REM 
REM This script initializes the comprehensive blue-green deployment system.
REM Run this ONCE to bootstrap the new system, then it will be fully automatic.
REM
REM What this script does:
REM 1. Pulls latest code from GitHub
REM 2. Stops all existing containers gracefully
REM 3. Removes old containers and images
REM 4. Builds ALL services with --no-cache (blue AND green)
REM 5. Starts the complete blue-green system
REM 6. Verifies health of all services
REM
REM After running this script, the system will:
REM - Auto-update from GitHub every 5 minutes
REM - Use blue-green deployment for ALL services
REM - Have zero downtime during updates
REM - Be able to update even the deployment controller itself
REM
REM ============================================================================

echo.
echo ========================================
echo BLUE-GREEN DEPLOYMENT BOOTSTRAP
echo ========================================
echo.
echo This will initialize the comprehensive blue-green deployment system.
echo All services will have blue/green instances for zero-downtime updates.
echo.
echo Press Ctrl+C to cancel, or
pause

REM Navigate to the erlang-services directory
cd /d "%~dp0"

echo.
echo [1/7] Pulling latest code from GitHub...
git pull origin release
if errorlevel 1 (
    echo WARNING: Git pull failed, continuing with local code...
)

echo.
echo [2/7] Stopping existing containers gracefully...
docker-compose down --timeout 30
if errorlevel 1 (
    echo WARNING: Some containers may not have stopped cleanly
)

echo.
echo [3/7] Removing old images to ensure clean build...
docker-compose rm -f
docker image prune -f

echo.
echo [4/7] Building ALL blue-green services (this may take 5-10 minutes)...
echo Building with --no-cache to ensure fresh source code...

REM Set build args for cache busting
for /f "tokens=*" %%i in ('git rev-parse --short HEAD 2^>nul') do set COMMIT_HASH=%%i
if "%COMMIT_HASH%"=="" set COMMIT_HASH=bootstrap

for /f "tokens=*" %%i in ('powershell -command "Get-Date -Format o"') do set BUILD_TIME=%%i
if "%BUILD_TIME%"=="" set BUILD_TIME=bootstrap

echo COMMIT_HASH=%COMMIT_HASH%
echo BUILD_TIME=%BUILD_TIME%

docker-compose build --no-cache ^
    --build-arg COMMIT_HASH=%COMMIT_HASH% ^
    --build-arg BUILD_TIME=%BUILD_TIME%

if errorlevel 1 (
    echo ERROR: Build failed!
    echo Please check Docker Desktop is running and try again.
    pause
    exit /b 1
)

echo.
echo [5/7] Starting all blue-green services...
docker-compose up -d

if errorlevel 1 (
    echo ERROR: Failed to start services!
    pause
    exit /b 1
)

echo.
echo [6/7] Waiting for services to become healthy (60 seconds)...
timeout /t 60 /nobreak

echo.
echo [7/7] Checking service health...
echo.

echo Checking nginx-proxy...
docker inspect --format="{{.State.Health.Status}}" mental-models-proxy 2>nul || echo NOT RUNNING

echo Checking deployment-controller-blue...
docker inspect --format="{{.State.Health.Status}}" mental-models-deploy-ctrl-blue 2>nul || echo NOT RUNNING

echo Checking deployment-controller-green...
docker inspect --format="{{.State.Health.Status}}" mental-models-deploy-ctrl-green 2>nul || echo NOT RUNNING

echo Checking auto-updater-blue...
docker inspect --format="{{.State.Health.Status}}" mental-models-auto-updater-blue 2>nul || echo NOT RUNNING

echo Checking auto-updater-green...
docker inspect --format="{{.State.Health.Status}}" mental-models-auto-updater-green 2>nul || echo NOT RUNNING

echo Checking desktop-ui-blue...
docker inspect --format="{{.State.Health.Status}}" mental-models-ui-blue 2>nul || echo NOT RUNNING

echo Checking desktop-ui-green...
docker inspect --format="{{.State.Health.Status}}" mental-models-ui-green 2>nul || echo NOT RUNNING

echo Checking analysis-service-blue...
docker inspect --format="{{.State.Health.Status}}" mental-models-analysis-blue 2>nul || echo NOT RUNNING

echo Checking analysis-service-green...
docker inspect --format="{{.State.Health.Status}}" mental-models-analysis-green 2>nul || echo NOT RUNNING

echo.
echo ========================================
echo BOOTSTRAP COMPLETE!
echo ========================================
echo.
echo The blue-green deployment system is now active.
echo.
echo Access the UI at: http://localhost:3000
echo.
echo Services running:
echo   - Desktop UI (blue + green)
echo   - API Gateway (blue + green)
echo   - Analysis Service (blue + green)
echo   - Harvester Service (blue + green)
echo   - Storage Service (blue + green)
echo   - Chaos Engineering (blue + green)
echo   - Auto-Updater (blue + green)
echo   - Deployment Controller (blue + green)
echo   - Nginx Proxy (routes traffic)
echo.
echo From now on, all updates will be automatic with zero downtime!
echo The auto-updater checks GitHub every 5 minutes.
echo.
echo To view logs: docker-compose logs -f
echo To stop all:  docker-compose down
echo.
pause
