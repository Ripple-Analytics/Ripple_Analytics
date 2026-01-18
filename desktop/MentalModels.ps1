# Mental Models Desktop App - One-Click Launcher
# Double-click this file or run: powershell -ExecutionPolicy Bypass -File MentalModels.ps1

$ErrorActionPreference = "Stop"
$AppName = "Mental Models"
$AppDir = "$env:LOCALAPPDATA\MentalModels"
$JavaVersion = "21"
$JavaUrl = "https://download.oracle.com/java/21/latest/jdk-21_windows-x64_bin.zip"
$ClojureUrl = "https://download.clojure.org/install/win-install-1.11.1.1435.ps1"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  $AppName Desktop App" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Create app directory
if (-not (Test-Path $AppDir)) {
    Write-Host "Creating app directory..." -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $AppDir -Force | Out-Null
}

# Check for Java
$JavaHome = "$AppDir\jdk-$JavaVersion"
$JavaExe = "$JavaHome\bin\java.exe"

if (-not (Test-Path $JavaExe)) {
    Write-Host "Java not found. Downloading portable JDK $JavaVersion..." -ForegroundColor Yellow
    $JavaZip = "$AppDir\jdk.zip"
    
    try {
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -Uri $JavaUrl -OutFile $JavaZip -UseBasicParsing
        
        Write-Host "Extracting Java..." -ForegroundColor Yellow
        Expand-Archive -Path $JavaZip -DestinationPath $AppDir -Force
        
        # Find the extracted folder (it might have a version suffix)
        $ExtractedDir = Get-ChildItem -Path $AppDir -Directory | Where-Object { $_.Name -like "jdk-*" } | Select-Object -First 1
        if ($ExtractedDir -and $ExtractedDir.FullName -ne $JavaHome) {
            Rename-Item -Path $ExtractedDir.FullName -NewName "jdk-$JavaVersion" -Force
        }
        
        Remove-Item $JavaZip -Force
        Write-Host "Java installed successfully!" -ForegroundColor Green
    }
    catch {
        Write-Host "Failed to download Java. Please install Java $JavaVersion manually." -ForegroundColor Red
        Write-Host "Download from: https://adoptium.net/" -ForegroundColor Yellow
        Read-Host "Press Enter to exit"
        exit 1
    }
}

$env:JAVA_HOME = $JavaHome
$env:PATH = "$JavaHome\bin;$env:PATH"

Write-Host "Using Java: $JavaExe" -ForegroundColor Gray

# Check for Clojure CLI
$ClojureDir = "$AppDir\clojure"
$ClojureExe = "$ClojureDir\clj.ps1"

if (-not (Test-Path $ClojureExe)) {
    Write-Host "Installing Clojure CLI..." -ForegroundColor Yellow
    
    try {
        # Download and run the official Clojure installer
        $InstallerScript = "$AppDir\clojure-install.ps1"
        Invoke-WebRequest -Uri $ClojureUrl -OutFile $InstallerScript -UseBasicParsing
        
        # Create a custom install location
        New-Item -ItemType Directory -Path $ClojureDir -Force | Out-Null
        
        # Run installer with custom prefix
        & powershell -ExecutionPolicy Bypass -File $InstallerScript -Prefix $ClojureDir
        
        Write-Host "Clojure installed successfully!" -ForegroundColor Green
    }
    catch {
        Write-Host "Failed to install Clojure. Trying alternative method..." -ForegroundColor Yellow
        
        # Alternative: Download clojure-tools directly
        $ToolsUrl = "https://github.com/clojure/brew-install/releases/latest/download/clojure-tools.zip"
        $ToolsZip = "$AppDir\clojure-tools.zip"
        
        try {
            Invoke-WebRequest -Uri $ToolsUrl -OutFile $ToolsZip -UseBasicParsing
            Expand-Archive -Path $ToolsZip -DestinationPath $ClojureDir -Force
            Remove-Item $ToolsZip -Force
            Write-Host "Clojure tools installed!" -ForegroundColor Green
        }
        catch {
            Write-Host "Could not install Clojure automatically." -ForegroundColor Red
            Write-Host "Please install from: https://clojure.org/guides/install_clojure#_windows" -ForegroundColor Yellow
            Read-Host "Press Enter to exit"
            exit 1
        }
    }
}

$env:PATH = "$ClojureDir;$env:PATH"

# Clone or update the app
$RepoDir = "$AppDir\app"
$RepoUrl = "https://github.com/Ripple-Analytics/Ripple_Analytics.git"

if (-not (Test-Path "$RepoDir\.git")) {
    Write-Host "Downloading Mental Models app..." -ForegroundColor Yellow
    
    # Check if git is available
    $GitAvailable = Get-Command git -ErrorAction SilentlyContinue
    
    if ($GitAvailable) {
        git clone $RepoUrl $RepoDir
    }
    else {
        Write-Host "Git not found. Downloading as ZIP..." -ForegroundColor Yellow
        $ZipUrl = "https://github.com/Ripple-Analytics/Ripple_Analytics/archive/refs/heads/main.zip"
        $ZipFile = "$AppDir\app.zip"
        
        Invoke-WebRequest -Uri $ZipUrl -OutFile $ZipFile -UseBasicParsing
        Expand-Archive -Path $ZipFile -DestinationPath $AppDir -Force
        
        # Rename extracted folder
        $ExtractedDir = Get-ChildItem -Path $AppDir -Directory | Where-Object { $_.Name -like "Ripple_Analytics-*" } | Select-Object -First 1
        if ($ExtractedDir) {
            if (Test-Path $RepoDir) { Remove-Item $RepoDir -Recurse -Force }
            Rename-Item -Path $ExtractedDir.FullName -NewName "app" -Force
        }
        
        Remove-Item $ZipFile -Force
    }
    
    Write-Host "App downloaded successfully!" -ForegroundColor Green
}
else {
    Write-Host "Checking for updates..." -ForegroundColor Yellow
    
    $GitAvailable = Get-Command git -ErrorAction SilentlyContinue
    if ($GitAvailable) {
        Push-Location $RepoDir
        git pull --quiet 2>$null
        Pop-Location
    }
}

# Configuration
$ConfigFile = "$AppDir\config.edn"
if (-not (Test-Path $ConfigFile)) {
    Write-Host ""
    Write-Host "First-time setup:" -ForegroundColor Cyan
    
    # Get LM Studio URL
    $DefaultLmStudio = "http://localhost:1234"
    Write-Host "LM Studio URL [$DefaultLmStudio]: " -NoNewline -ForegroundColor Yellow
    $LmStudioUrl = Read-Host
    if ([string]::IsNullOrWhiteSpace($LmStudioUrl)) { $LmStudioUrl = $DefaultLmStudio }
    
    # Get watch folder
    $DefaultFolder = "$env:USERPROFILE\Documents"
    Write-Host "Folder to scan [$DefaultFolder]: " -NoNewline -ForegroundColor Yellow
    $WatchFolder = Read-Host
    if ([string]::IsNullOrWhiteSpace($WatchFolder)) { $WatchFolder = $DefaultFolder }
    
    # Get web app URL (optional)
    Write-Host "Web app URL (optional, press Enter to skip): " -NoNewline -ForegroundColor Yellow
    $WebAppUrl = Read-Host
    
    # Write config
    $Config = @"
{:lm-studio-url "$LmStudioUrl"
 :watch-folders ["$($WatchFolder -replace '\\', '/')"]
 :web-app-url $(if ([string]::IsNullOrWhiteSpace($WebAppUrl)) { 'nil' } else { "`"$WebAppUrl`"" })
 :auto-update true
 :update-interval-minutes 5}
"@
    
    Set-Content -Path $ConfigFile -Value $Config
    Write-Host "Configuration saved!" -ForegroundColor Green
}

# Show menu
function Show-Menu {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "  What would you like to do?" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  1. Scan a folder for mental models" -ForegroundColor White
    Write-Host "  2. Watch folder continuously (24/7 mode)" -ForegroundColor White
    Write-Host "  3. Check system status" -ForegroundColor White
    Write-Host "  4. Update configuration" -ForegroundColor White
    Write-Host "  5. Check for app updates" -ForegroundColor White
    Write-Host "  6. Exit" -ForegroundColor White
    Write-Host ""
    Write-Host "Enter choice (1-6): " -NoNewline -ForegroundColor Yellow
}

# Run Clojure command
function Run-Clojure {
    param([string]$Command, [string[]]$Args)
    
    $DesktopDir = "$RepoDir\desktop"
    if (-not (Test-Path $DesktopDir)) {
        $DesktopDir = $RepoDir
    }
    
    Push-Location $DesktopDir
    
    try {
        $ClojureCmd = Get-Command clj -ErrorAction SilentlyContinue
        if ($ClojureCmd) {
            & clj -M:run $Command @Args
        }
        else {
            & "$ClojureDir\clj.ps1" -M:run $Command @Args
        }
    }
    finally {
        Pop-Location
    }
}

# Main loop
while ($true) {
    Show-Menu
    $Choice = Read-Host
    
    switch ($Choice) {
        "1" {
            Write-Host ""
            Write-Host "Enter folder path to scan: " -NoNewline -ForegroundColor Yellow
            $ScanFolder = Read-Host
            
            if (Test-Path $ScanFolder) {
                Write-Host "Scanning $ScanFolder..." -ForegroundColor Cyan
                Run-Clojure "scan" $ScanFolder
            }
            else {
                Write-Host "Folder not found: $ScanFolder" -ForegroundColor Red
            }
        }
        "2" {
            Write-Host ""
            Write-Host "Starting continuous watch mode..." -ForegroundColor Cyan
            Write-Host "Press Ctrl+C to stop" -ForegroundColor Gray
            
            # Read watch folder from config
            $ConfigContent = Get-Content $ConfigFile -Raw
            if ($ConfigContent -match ':watch-folders \["([^"]+)"') {
                $WatchFolder = $Matches[1]
                Run-Clojure "watch" $WatchFolder
            }
            else {
                Write-Host "No watch folder configured. Run option 4 to configure." -ForegroundColor Red
            }
        }
        "3" {
            Write-Host ""
            Run-Clojure "status"
        }
        "4" {
            # Re-run configuration
            Remove-Item $ConfigFile -Force -ErrorAction SilentlyContinue
            
            Write-Host ""
            Write-Host "Configuration:" -ForegroundColor Cyan
            
            $DefaultLmStudio = "http://localhost:1234"
            Write-Host "LM Studio URL [$DefaultLmStudio]: " -NoNewline -ForegroundColor Yellow
            $LmStudioUrl = Read-Host
            if ([string]::IsNullOrWhiteSpace($LmStudioUrl)) { $LmStudioUrl = $DefaultLmStudio }
            
            $DefaultFolder = "$env:USERPROFILE\Documents"
            Write-Host "Folder to scan [$DefaultFolder]: " -NoNewline -ForegroundColor Yellow
            $WatchFolder = Read-Host
            if ([string]::IsNullOrWhiteSpace($WatchFolder)) { $WatchFolder = $DefaultFolder }
            
            Write-Host "Web app URL (optional): " -NoNewline -ForegroundColor Yellow
            $WebAppUrl = Read-Host
            
            $Config = @"
{:lm-studio-url "$LmStudioUrl"
 :watch-folders ["$($WatchFolder -replace '\\', '/')"]
 :web-app-url $(if ([string]::IsNullOrWhiteSpace($WebAppUrl)) { 'nil' } else { "`"$WebAppUrl`"" })
 :auto-update true
 :update-interval-minutes 5}
"@
            
            Set-Content -Path $ConfigFile -Value $Config
            Write-Host "Configuration updated!" -ForegroundColor Green
        }
        "5" {
            Write-Host ""
            Write-Host "Checking for updates..." -ForegroundColor Cyan
            
            $GitAvailable = Get-Command git -ErrorAction SilentlyContinue
            if ($GitAvailable -and (Test-Path "$RepoDir\.git")) {
                Push-Location $RepoDir
                $CurrentCommit = git rev-parse HEAD
                git fetch origin main --quiet
                $LatestCommit = git rev-parse origin/main
                
                if ($CurrentCommit -ne $LatestCommit) {
                    Write-Host "Update available!" -ForegroundColor Yellow
                    Write-Host "Current: $($CurrentCommit.Substring(0,7))" -ForegroundColor Gray
                    Write-Host "Latest:  $($LatestCommit.Substring(0,7))" -ForegroundColor Gray
                    Write-Host ""
                    Write-Host "Update now? (y/n): " -NoNewline -ForegroundColor Yellow
                    $Confirm = Read-Host
                    
                    if ($Confirm -eq "y") {
                        git pull origin main
                        Write-Host "Updated successfully!" -ForegroundColor Green
                    }
                }
                else {
                    Write-Host "Already up to date!" -ForegroundColor Green
                }
                
                Pop-Location
            }
            else {
                Write-Host "Re-downloading latest version..." -ForegroundColor Yellow
                Remove-Item $RepoDir -Recurse -Force -ErrorAction SilentlyContinue
                
                $ZipUrl = "https://github.com/Ripple-Analytics/Ripple_Analytics/archive/refs/heads/main.zip"
                $ZipFile = "$AppDir\app.zip"
                
                Invoke-WebRequest -Uri $ZipUrl -OutFile $ZipFile -UseBasicParsing
                Expand-Archive -Path $ZipFile -DestinationPath $AppDir -Force
                
                $ExtractedDir = Get-ChildItem -Path $AppDir -Directory | Where-Object { $_.Name -like "Ripple_Analytics-*" } | Select-Object -First 1
                if ($ExtractedDir) {
                    Rename-Item -Path $ExtractedDir.FullName -NewName "app" -Force
                }
                
                Remove-Item $ZipFile -Force
                Write-Host "Updated successfully!" -ForegroundColor Green
            }
        }
        "6" {
            Write-Host ""
            Write-Host "Goodbye!" -ForegroundColor Cyan
            exit 0
        }
        default {
            Write-Host "Invalid choice. Please enter 1-6." -ForegroundColor Red
        }
    }
}
