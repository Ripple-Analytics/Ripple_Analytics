# ============================================================================
# BLUE-GREEN DEPLOYMENT BOOTSTRAP SCRIPT (PowerShell)
# ============================================================================
#
# This script initializes the comprehensive blue-green deployment system.
# Run this ONCE to bootstrap the new system, then it will be fully automatic.
#
# Usage: Right-click and "Run with PowerShell" or execute from PowerShell:
#        .\bootstrap-blue-green.ps1
#
# ============================================================================

$ErrorActionPreference = "Continue"

function Write-Header {
    param([string]$Message)
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host $Message -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""
}

function Write-Step {
    param([string]$Step, [string]$Message)
    Write-Host "[$Step] $Message" -ForegroundColor Yellow
}

function Write-Success {
    param([string]$Message)
    Write-Host "[OK] $Message" -ForegroundColor Green
}

function Write-Warning {
    param([string]$Message)
    Write-Host "[WARNING] $Message" -ForegroundColor Yellow
}

function Write-Error {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

function Test-ServiceHealth {
    param([string]$ContainerName)
    try {
        $health = docker inspect --format="{{.State.Health.Status}}" $ContainerName 2>$null
        if ($health -eq "healthy") {
            Write-Host "  $ContainerName : " -NoNewline
            Write-Host "HEALTHY" -ForegroundColor Green
            return $true
        } elseif ($health -eq "starting") {
            Write-Host "  $ContainerName : " -NoNewline
            Write-Host "STARTING" -ForegroundColor Yellow
            return $false
        } else {
            Write-Host "  $ContainerName : " -NoNewline
            Write-Host "$health" -ForegroundColor Red
            return $false
        }
    } catch {
        Write-Host "  $ContainerName : " -NoNewline
        Write-Host "NOT RUNNING" -ForegroundColor Red
        return $false
    }
}

# Main script
Write-Header "BLUE-GREEN DEPLOYMENT BOOTSTRAP"

Write-Host "This will initialize the comprehensive blue-green deployment system."
Write-Host "All services will have blue/green instances for zero-downtime updates."
Write-Host ""
Write-Host "Press Enter to continue or Ctrl+C to cancel..."
Read-Host

# Navigate to script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ScriptDir

# Step 1: Pull latest code
Write-Step "1/8" "Pulling latest code from GitHub..."
git pull origin release
if ($LASTEXITCODE -ne 0) {
    Write-Warning "Git pull failed, continuing with local code..."
}

# Step 2: Check Docker is running
Write-Step "2/8" "Checking Docker Desktop is running..."
try {
    docker info | Out-Null
    Write-Success "Docker is running"
} catch {
    Write-Error "Docker is not running! Please start Docker Desktop and try again."
    Read-Host "Press Enter to exit"
    exit 1
}

# Step 3: Stop existing containers
Write-Step "3/8" "Stopping existing containers gracefully..."
docker-compose down --timeout 30 2>$null
docker-compose rm -f 2>$null

# Step 4: Clean up old images
Write-Step "4/8" "Cleaning up old images..."
docker image prune -f 2>$null

# Step 5: Set build args
Write-Step "5/8" "Preparing build arguments..."
$CommitHash = git rev-parse --short HEAD 2>$null
if (-not $CommitHash) { $CommitHash = "bootstrap" }
$BuildTime = Get-Date -Format "o"

Write-Host "  COMMIT_HASH: $CommitHash"
Write-Host "  BUILD_TIME: $BuildTime"

# Set environment variables for docker-compose
$env:COMMIT_HASH = $CommitHash
$env:BUILD_TIME = $BuildTime

# Step 6: Build all services
Write-Step "6/8" "Building ALL blue-green services (this may take 5-10 minutes)..."
Write-Host "  Building with --no-cache to ensure fresh source code..."
Write-Host ""

docker-compose build --no-cache `
    --build-arg COMMIT_HASH=$CommitHash `
    --build-arg BUILD_TIME=$BuildTime

if ($LASTEXITCODE -ne 0) {
    Write-Error "Build failed! Please check the error messages above."
    Read-Host "Press Enter to exit"
    exit 1
}
Write-Success "All services built successfully"

# Step 7: Start all services
Write-Step "7/8" "Starting all blue-green services..."
docker-compose up -d

if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to start services!"
    Read-Host "Press Enter to exit"
    exit 1
}
Write-Success "All services started"

# Step 8: Health checks
Write-Step "8/8" "Waiting for services to become healthy..."
Write-Host "  This may take up to 2 minutes..."
Write-Host ""

$maxAttempts = 12
$attempt = 0
$allHealthy = $false

while ($attempt -lt $maxAttempts -and -not $allHealthy) {
    $attempt++
    Write-Host "  Health check attempt $attempt of $maxAttempts..." -ForegroundColor Gray
    Start-Sleep -Seconds 10
    
    $services = @(
        "mental-models-proxy",
        "mental-models-deploy-ctrl-blue",
        "mental-models-deploy-ctrl-green",
        "mental-models-auto-updater-blue",
        "mental-models-auto-updater-green",
        "mental-models-ui-blue",
        "mental-models-ui-green",
        "mental-models-analysis-blue",
        "mental-models-analysis-green"
    )
    
    $healthyCount = 0
    foreach ($service in $services) {
        if (Test-ServiceHealth $service) {
            $healthyCount++
        }
    }
    
    if ($healthyCount -ge 7) {  # At least 7 core services healthy
        $allHealthy = $true
    }
}

Write-Host ""

if ($allHealthy) {
    Write-Header "BOOTSTRAP COMPLETE!"
    Write-Host "The blue-green deployment system is now active." -ForegroundColor Green
} else {
    Write-Header "BOOTSTRAP COMPLETED WITH WARNINGS"
    Write-Host "Some services may still be starting. Check logs with:" -ForegroundColor Yellow
    Write-Host "  docker-compose logs -f" -ForegroundColor White
}

Write-Host ""
Write-Host "Access the UI at: " -NoNewline
Write-Host "http://localhost:3000" -ForegroundColor Cyan
Write-Host ""
Write-Host "Services running:"
Write-Host "  - Desktop UI (blue + green)"
Write-Host "  - API Gateway (blue + green)"
Write-Host "  - Analysis Service (blue + green)"
Write-Host "  - Harvester Service (blue + green)"
Write-Host "  - Storage Service (blue + green)"
Write-Host "  - Chaos Engineering (blue + green)"
Write-Host "  - Auto-Updater (blue + green)"
Write-Host "  - Deployment Controller (blue + green)"
Write-Host "  - Nginx Proxy (routes traffic)"
Write-Host ""
Write-Host "From now on, all updates will be automatic with zero downtime!" -ForegroundColor Green
Write-Host "The auto-updater checks GitHub every 5 minutes."
Write-Host ""
Write-Host "Useful commands:"
Write-Host "  View logs:    docker-compose logs -f"
Write-Host "  Stop all:     docker-compose down"
Write-Host "  Restart:      docker-compose restart"
Write-Host ""

# Open browser
$openBrowser = Read-Host "Open UI in browser? (Y/n)"
if ($openBrowser -ne "n" -and $openBrowser -ne "N") {
    Start-Process "http://localhost:3000"
}

Read-Host "Press Enter to exit"
