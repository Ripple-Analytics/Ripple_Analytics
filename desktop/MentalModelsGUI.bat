@echo off
setlocal enabledelayedexpansion
title Mental Models Desktop
cls

echo.
echo  ================================================================
echo                 MENTAL MODELS DESKTOP APP
echo                       GUI Version v1.0.5
echo  ================================================================
echo.

:: Set paths
set "APP_DIR=%~dp0"
set "JAVA_DIR=%APP_DIR%jre"
set "CLJ_DIR=%APP_DIR%clojure"

:: ============================================================
:: JAVA CHECK - Need Java 17+
:: ============================================================

:: Check for bundled Java first
if exist "%JAVA_DIR%\bin\java.exe" (
    echo [OK] Using bundled Java 21
    set "JAVA_HOME=%JAVA_DIR%"
    set "PATH=%JAVA_DIR%\bin;%PATH%"
    goto :check_clojure
)

:: Check system Java version
set "JAVA_OK=0"
for /f "tokens=3" %%v in ('java -version 2^>^&1 ^| findstr /i "version"') do (
    set "JAVA_VER=%%~v"
)
if defined JAVA_VER (
    for /f "tokens=1 delims=." %%a in ("!JAVA_VER!") do (
        if %%a GEQ 17 set "JAVA_OK=1"
    )
)

if "!JAVA_OK!"=="1" (
    echo [OK] System Java !JAVA_VER! detected
    goto :check_clojure
)

:: Need to download Java
echo [INFO] Java 17+ required. Found: !JAVA_VER!
echo [INFO] Downloading portable Java 21...
echo [INFO] This is a one-time download (~50MB)
echo.

if not exist "%APP_DIR%temp" mkdir "%APP_DIR%temp"

:: Try curl first (faster, available on Windows 10+)
curl --version >nul 2>&1
if %ERRORLEVEL% equ 0 (
    echo [INFO] Downloading with curl...
    curl -L -# -o "%APP_DIR%temp\java.zip" "https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2+13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip"
) else (
    echo [INFO] Downloading with PowerShell...
    powershell -Command "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; $ProgressPreference = 'SilentlyContinue'; Invoke-WebRequest -Uri 'https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2+13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip' -OutFile '%APP_DIR%temp\java.zip'"
)

if not exist "%APP_DIR%temp\java.zip" (
    echo [ERROR] Failed to download Java. Check internet connection.
    pause
    exit /b 1
)

echo [INFO] Extracting Java...
powershell -Command "Expand-Archive -Path '%APP_DIR%temp\java.zip' -DestinationPath '%APP_DIR%temp\extracted' -Force"

for /d %%d in ("%APP_DIR%temp\extracted\jdk-*") do (
    if exist "%JAVA_DIR%" rmdir /s /q "%JAVA_DIR%"
    move "%%d" "%JAVA_DIR%" >nul
)

rmdir /s /q "%APP_DIR%temp" 2>nul

if exist "%JAVA_DIR%\bin\java.exe" (
    echo [OK] Java 21 installed
    set "JAVA_HOME=%JAVA_DIR%"
    set "PATH=%JAVA_DIR%\bin;%PATH%"
) else (
    echo [ERROR] Java installation failed
    pause
    exit /b 1
)

:: ============================================================
:: CLOJURE CHECK - Using portable install
:: ============================================================
:check_clojure

:: Check for our portable Clojure tools jar
if exist "%CLJ_DIR%\clojure-tools.jar" (
    echo [OK] Using bundled Clojure tools
    goto :run_app
)

:: Check system Clojure
where clj >nul 2>&1
if %ERRORLEVEL% equ 0 (
    echo [OK] System Clojure detected
    goto :run_app_clj
)

:: Download Clojure tools directly (no installer needed)
echo [INFO] Downloading Clojure tools...
if not exist "%CLJ_DIR%" mkdir "%CLJ_DIR%"

curl --version >nul 2>&1
if %ERRORLEVEL% equ 0 (
    curl -L -# -o "%CLJ_DIR%\clojure-tools.zip" "https://download.clojure.org/install/clojure-tools-1.11.1.1435.zip"
) else (
    powershell -Command "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; $ProgressPreference = 'SilentlyContinue'; Invoke-WebRequest -Uri 'https://download.clojure.org/install/clojure-tools-1.11.1.1435.zip' -OutFile '%CLJ_DIR%\clojure-tools.zip'"
)

if not exist "%CLJ_DIR%\clojure-tools.zip" (
    echo [ERROR] Failed to download Clojure tools
    pause
    exit /b 1
)

echo [INFO] Extracting Clojure tools...
powershell -Command "Expand-Archive -Path '%CLJ_DIR%\clojure-tools.zip' -DestinationPath '%CLJ_DIR%' -Force"

:: Find the jar file
for %%f in ("%CLJ_DIR%\ClojureTools\clojure-tools-*.jar") do (
    copy "%%f" "%CLJ_DIR%\clojure-tools.jar" >nul
)

:: Copy other needed files
if exist "%CLJ_DIR%\ClojureTools\deps.edn" copy "%CLJ_DIR%\ClojureTools\deps.edn" "%CLJ_DIR%\" >nul
if exist "%CLJ_DIR%\ClojureTools\exec.jar" copy "%CLJ_DIR%\ClojureTools\exec.jar" "%CLJ_DIR%\" >nul
if exist "%CLJ_DIR%\ClojureTools\tools.edn" copy "%CLJ_DIR%\ClojureTools\tools.edn" "%CLJ_DIR%\" >nul

del "%CLJ_DIR%\clojure-tools.zip" 2>nul

echo [OK] Clojure tools installed

:: ============================================================
:: RUN APPLICATION (using java directly with clojure-tools)
:: ============================================================
:run_app
echo.
echo  ================================================================
echo                    STARTING APPLICATION
echo  ================================================================
echo.

cd /d "%APP_DIR%"

if exist ".cpcache" (
    echo [OK] Dependencies cached - starting quickly
) else (
    echo [INFO] First run - downloading dependencies...
    echo [INFO] This takes 2-3 minutes, please wait...
)
echo.

:: Run using java with clojure-tools.jar
java -jar "%CLJ_DIR%\clojure-tools.jar" -Sdeps "{:deps {cljfx/cljfx {:mvn/version \"1.7.24\"} org.xerial/sqlite-jdbc {:mvn/version \"3.44.1.0\"} clj-http/clj-http {:mvn/version \"3.12.3\"} cheshire/cheshire {:mvn/version \"5.12.0\"} com.taoensso/timbre {:mvn/version \"6.3.1\"} org.clojure/core.async {:mvn/version \"1.6.681\"}}}" -M -m mental-models.desktop.gui.app

if %ERRORLEVEL% neq 0 (
    echo.
    echo [INFO] Trying alternative startup...
    goto :run_app_alt
)
goto :end

:: ============================================================
:: RUN APPLICATION (using system clj command)
:: ============================================================
:run_app_clj
echo.
echo  ================================================================
echo                    STARTING APPLICATION
echo  ================================================================
echo.

cd /d "%APP_DIR%"

if exist ".cpcache" (
    echo [OK] Dependencies cached
) else (
    echo [INFO] First run - downloading dependencies (2-3 min)...
)
echo.

clj -M:gui

if %ERRORLEVEL% neq 0 (
    echo.
    echo [ERROR] Application failed to start
)
goto :end

:: ============================================================
:: ALTERNATIVE: Run with explicit classpath
:: ============================================================
:run_app_alt
echo [INFO] Building classpath...

:: Create a simple runner
java -jar "%CLJ_DIR%\clojure-tools.jar" -Spath -Sdeps "{:deps {cljfx/cljfx {:mvn/version \"1.7.24\"} org.xerial/sqlite-jdbc {:mvn/version \"3.44.1.0\"} clj-http/clj-http {:mvn/version \"3.12.3\"} cheshire/cheshire {:mvn/version \"5.12.0\"} com.taoensso/timbre {:mvn/version \"6.3.1\"} org.clojure/core.async {:mvn/version \"1.6.681\"}}}" > "%APP_DIR%temp_cp.txt"

set /p CLASSPATH=<"%APP_DIR%temp_cp.txt"
del "%APP_DIR%temp_cp.txt" 2>nul

echo [INFO] Starting with explicit classpath...
java -cp "%CLASSPATH%;src" clojure.main -m mental-models.desktop.gui.app

:end
echo.
pause
