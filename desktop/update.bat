@echo off
setlocal enabledelayedexpansion
title Mental Models - Update
cls

echo.
echo  ================================================================
echo                 MENTAL MODELS - SELF UPDATE
echo  ================================================================
echo.

set "APP_DIR=%~dp0"
cd /d "%APP_DIR%"

:: Check if git is available
where git >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo [INFO] Git not found. Installing portable git...
    
    if not exist "%APP_DIR%git" mkdir "%APP_DIR%git"
    
    echo [INFO] Downloading Git...
    curl -L -# -o "%APP_DIR%git\PortableGit.exe" "https://github.com/git-for-windows/git/releases/download/v2.43.0.windows.1/PortableGit-2.43.0-64-bit.7z.exe"
    
    if exist "%APP_DIR%git\PortableGit.exe" (
        echo [INFO] Extracting Git...
        "%APP_DIR%git\PortableGit.exe" -o"%APP_DIR%git" -y
        set "PATH=%APP_DIR%git\bin;%PATH%"
    ) else (
        echo [ERROR] Could not download Git
        echo [INFO] Please install Git from: https://git-scm.com/download/win
        pause
        exit /b 1
    )
)

:: Check if this is a git repo
if exist "%APP_DIR%.git" (
    echo [OK] Git repository found
    goto :do_update
)

:: Not a git repo - need to initialize
echo [INFO] Setting up Git repository...

:: Initialize and add remote
git init
git remote add origin https://github.com/Ripple-Analytics/Ripple_Analytics.git

:: Configure sparse checkout to only get desktop folder
git config core.sparseCheckout true
echo desktop/ > .git/info/sparse-checkout

echo [INFO] Downloading latest version...
git fetch origin devin/1768775833-lm-studio-pipeline
git checkout -b main origin/devin/1768775833-lm-studio-pipeline

echo [OK] Repository initialized
goto :show_version

:do_update
echo.
echo [INFO] Checking for updates...

:: Save current version
if exist VERSION.txt (
    set /p OLD_VERSION=<VERSION.txt
    echo [INFO] Current version: !OLD_VERSION!
)

:: Fetch and pull latest
git fetch origin devin/1768775833-lm-studio-pipeline 2>nul
git pull origin devin/1768775833-lm-studio-pipeline 2>nul

if %ERRORLEVEL% equ 0 (
    echo [OK] Update complete!
) else (
    echo [WARN] Update had issues - trying force update...
    git reset --hard origin/devin/1768775833-lm-studio-pipeline
)

:show_version
echo.

:: Show new version
if exist VERSION.txt (
    echo  ----------------------------------------------------------------
    type VERSION.txt
    echo  ----------------------------------------------------------------
) else (
    echo [INFO] Version file not found
)

echo.
echo [OK] You can now run MentalModelsGUI.bat
echo.
pause
