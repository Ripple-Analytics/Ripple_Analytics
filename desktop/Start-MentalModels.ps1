# Mental Models Desktop App - PowerShell Launcher
# This script handles all dependencies automatically

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

$AppDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$JavaDir = Join-Path $AppDir "jre"
$ClojureDir = Join-Path $AppDir "clojure"
$DepsDir = Join-Path $AppDir ".cpcache"

Write-Host ""
Write-Host "  ================================================================" -ForegroundColor Cyan
Write-Host "              MENTAL MODELS DESKTOP APP" -ForegroundColor Cyan
Write-Host "                    GUI Version" -ForegroundColor Cyan
Write-Host "  ================================================================" -ForegroundColor Cyan
Write-Host ""

# Function to check Java version
function Get-JavaVersion {
    try {
        $output = & java -version 2>&1
        if ($output -match '"(\d+)') {
            return [int]$matches[1]
        }
    } catch {}
    return 0
}

# Check/Install Java
$javaExe = Join-Path $JavaDir "bin\java.exe"
if (Test-Path $javaExe) {
    $env:JAVA_HOME = $JavaDir
    $env:PATH = "$JavaDir\bin;$env:PATH"
    Write-Host "[OK] Using bundled Java" -ForegroundColor Green
} elseif ((Get-JavaVersion) -ge 17) {
    Write-Host "[OK] System Java detected" -ForegroundColor Green
} else {
    Write-Host "[INFO] Downloading portable Java 21..." -ForegroundColor Yellow
    Write-Host "[INFO] This is a one-time download (~50MB compressed)" -ForegroundColor Yellow
    
    $javaUrl = "https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2%2B13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip"
    $javaZip = Join-Path $env:TEMP "java.zip"
    
    try {
        Invoke-WebRequest -Uri $javaUrl -OutFile $javaZip
        Write-Host "[INFO] Extracting Java..." -ForegroundColor Yellow
        Expand-Archive -Path $javaZip -DestinationPath $AppDir -Force
        
        # Rename extracted folder
        $extracted = Get-ChildItem -Path $AppDir -Directory | Where-Object { $_.Name -like "jdk-*" } | Select-Object -First 1
        if ($extracted) {
            if (Test-Path $JavaDir) { Remove-Item $JavaDir -Recurse -Force }
            Rename-Item -Path $extracted.FullName -NewName "jre"
        }
        
        Remove-Item $javaZip -Force -ErrorAction SilentlyContinue
        
        $env:JAVA_HOME = $JavaDir
        $env:PATH = "$JavaDir\bin;$env:PATH"
        Write-Host "[OK] Java installed successfully" -ForegroundColor Green
    } catch {
        Write-Host "[ERROR] Failed to download Java: $_" -ForegroundColor Red
        Read-Host "Press Enter to exit"
        exit 1
    }
}

# Check/Install Clojure tools
$clojureJar = Get-ChildItem -Path $ClojureDir -Filter "clojure-tools*.jar" -Recurse -ErrorAction SilentlyContinue | Select-Object -First 1
if (-not $clojureJar) {
    Write-Host "[INFO] Downloading Clojure tools..." -ForegroundColor Yellow
    
    $clojureUrl = "https://download.clojure.org/install/clojure-tools-1.11.1.1435.zip"
    $clojureZip = Join-Path $env:TEMP "clojure.zip"
    
    try {
        New-Item -ItemType Directory -Path $ClojureDir -Force | Out-Null
        Invoke-WebRequest -Uri $clojureUrl -OutFile $clojureZip
        Expand-Archive -Path $clojureZip -DestinationPath $ClojureDir -Force
        Remove-Item $clojureZip -Force -ErrorAction SilentlyContinue
        
        $clojureJar = Get-ChildItem -Path $ClojureDir -Filter "clojure-tools*.jar" -Recurse | Select-Object -First 1
        Write-Host "[OK] Clojure tools installed" -ForegroundColor Green
    } catch {
        Write-Host "[ERROR] Failed to download Clojure: $_" -ForegroundColor Red
        Read-Host "Press Enter to exit"
        exit 1
    }
}

Write-Host ""
Write-Host "  ================================================================" -ForegroundColor Cyan
Write-Host "                   STARTING APPLICATION" -ForegroundColor Cyan
Write-Host "  ================================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "[INFO] First run downloads dependencies (~2-3 minutes)" -ForegroundColor Yellow
Write-Host "[INFO] Subsequent runs start in seconds" -ForegroundColor Yellow
Write-Host ""

# Change to app directory
Set-Location $AppDir

# Build classpath and run
$libsDir = Join-Path $ClojureDir "ClojureTools"
$cp = "$libsDir\*"

# Run using deps.edn
try {
    & java -jar (Join-Path $libsDir "clojure-tools-1.11.1.1435.jar") -Sdeps '{:deps {cljfx/cljfx {:mvn/version "1.7.24"} org.xerial/sqlite-jdbc {:mvn/version "3.44.1.0"} clj-http/clj-http {:mvn/version "3.12.3"} cheshire/cheshire {:mvn/version "5.12.0"} com.taoensso/timbre {:mvn/version "6.3.1"} org.clojure/core.async {:mvn/version "1.6.681"}}}' -M -m mental-models.desktop.gui.app
} catch {
    Write-Host ""
    Write-Host "[ERROR] Application failed to start" -ForegroundColor Red
    Write-Host "Error: $_" -ForegroundColor Red
    Write-Host ""
    Write-Host "Try running manually: clojure -M:gui" -ForegroundColor Yellow
}

Read-Host "Press Enter to exit"
