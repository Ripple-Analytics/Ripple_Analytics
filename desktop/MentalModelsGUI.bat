@echo off
setlocal enabledelayedexpansion
title Mental Models Desktop
cls

echo.
echo  ================================================================
echo                 MENTAL MODELS DESKTOP APP
echo                       GUI Version
echo  ================================================================
echo.

:: Set paths
set "APP_DIR=%~dp0"
set "JAVA_DIR=%APP_DIR%jre"

:: Check for bundled Java first
if exist "%JAVA_DIR%\bin\java.exe" (
    echo [OK] Using bundled Java
    set "JAVA_HOME=%JAVA_DIR%"
    set "PATH=%JAVA_DIR%\bin;%PATH%"
    goto :run_app
)

:: Check system Java
java -version >nul 2>&1
if %ERRORLEVEL% equ 0 (
    echo [OK] System Java detected
    goto :run_app
)

:: Download portable Java
echo [INFO] Downloading portable Java 21...
echo [INFO] This is a one-time download (~50MB compressed)
echo [INFO] Please wait...
echo.

:: Create temp directory
if not exist "%APP_DIR%temp" mkdir "%APP_DIR%temp"

:: Download using curl (available on Windows 10+) or PowerShell
echo [INFO] Downloading from Adoptium...
curl -L -o "%APP_DIR%temp\java.zip" "https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2+13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip" 2>nul

if not exist "%APP_DIR%temp\java.zip" (
    echo [INFO] Trying PowerShell download...
    powershell -Command "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; $ProgressPreference = 'SilentlyContinue'; Invoke-WebRequest -Uri 'https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2+13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip' -OutFile '%APP_DIR%temp\java.zip'"
)

if not exist "%APP_DIR%temp\java.zip" (
    echo.
    echo [ERROR] Failed to download Java.
    echo.
    echo Please download Java manually:
    echo   1. Go to: https://adoptium.net/temurin/releases/
    echo   2. Download: Windows x64 JRE 21
    echo   3. Extract to: %JAVA_DIR%
    echo.
    pause
    exit /b 1
)

echo [INFO] Download complete. Extracting...

:: Extract using PowerShell
powershell -Command "Expand-Archive -Path '%APP_DIR%temp\java.zip' -DestinationPath '%APP_DIR%temp\extracted' -Force"

:: Find the extracted folder (it has a version number in the name)
for /d %%d in ("%APP_DIR%temp\extracted\jdk-*") do (
    echo [INFO] Moving Java runtime...
    if exist "%JAVA_DIR%" rmdir /s /q "%JAVA_DIR%"
    move "%%d" "%JAVA_DIR%" >nul
)

:: Cleanup
rmdir /s /q "%APP_DIR%temp" 2>nul

:: Verify installation
if exist "%JAVA_DIR%\bin\java.exe" (
    echo [OK] Java installed successfully
    set "JAVA_HOME=%JAVA_DIR%"
    set "PATH=%JAVA_DIR%\bin;%PATH%"
) else (
    echo.
    echo [ERROR] Java extraction failed.
    echo.
    echo The zip file may be corrupted. Please try:
    echo   1. Delete the 'temp' folder if it exists
    echo   2. Run this script again
    echo.
    echo Or install Java manually from: https://adoptium.net/
    echo.
    pause
    exit /b 1
)

:run_app
echo.
echo  ================================================================
echo                    STARTING APPLICATION
echo  ================================================================
echo.

:: Check if Clojure deps are cached
if exist "%APP_DIR%.cpcache" (
    echo [OK] Dependencies cached
) else (
    echo [INFO] First run - downloading dependencies...
    echo [INFO] This may take 2-3 minutes...
)
echo.

cd /d "%APP_DIR%"

:: Try to run with clojure CLI if available
where clojure >nul 2>&1
if %ERRORLEVEL% equ 0 (
    echo [INFO] Starting with Clojure CLI...
    clojure -M:gui
    goto :end
)

:: Otherwise use java directly with deps
echo [INFO] Starting application...
echo [INFO] If this fails, please install Clojure CLI from:
echo        https://clojure.org/guides/install_clojure#_windows
echo.

java -version

echo.
echo [INFO] To run the GUI, you need Clojure CLI installed.
echo [INFO] Install from: https://github.com/clojure/brew-install/releases
echo.

:end
pause
