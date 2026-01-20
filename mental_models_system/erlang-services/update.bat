@echo off
REM Mental Models System - Auto Update Script for Windows
REM This script pulls the latest code from GitHub and rebuilds the containers

echo ========================================
echo Mental Models System - Auto Update
echo ========================================
echo.

REM Check if git is available
where git >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Git is not installed or not in PATH
    echo Please install Git from https://git-scm.com/
    pause
    exit /b 1
)

REM Check if docker is available
where docker >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Docker is not installed or not in PATH
    echo Please install Docker Desktop from https://www.docker.com/products/docker-desktop/
    pause
    exit /b 1
)

echo [1/4] Pulling latest code from GitHub...
git pull origin master
if %ERRORLEVEL% NEQ 0 (
    echo WARNING: Could not pull from GitHub. Continuing with local code...
)

echo.
echo [2/4] Stopping existing containers...
docker-compose down

echo.
echo [3/4] Rebuilding containers with latest code...
docker-compose build --no-cache

echo.
echo [4/4] Starting updated containers...
docker-compose up -d

echo.
echo ========================================
echo Update complete!
echo ========================================
echo.
echo Access the application at: http://localhost:3000
echo.
echo To view logs: docker-compose logs -f
echo To stop: docker-compose down
echo.
pause
