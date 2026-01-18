@echo off
setlocal enabledelayedexpansion
title Mental Models Desktop
cls

echo.
echo  ================================================================
echo                 MENTAL MODELS DESKTOP APP
echo                       GUI Version v1.0.6
echo  ================================================================
echo.

:: Set paths
set "APP_DIR=%~dp0"
set "JAVA_DIR=%APP_DIR%jre"
set "LIB_DIR=%APP_DIR%lib"
set "M2_REPO=%USERPROFILE%\.m2\repository"

:: ============================================================
:: JAVA CHECK - Need Java 17+
:: ============================================================

:: Check for bundled Java first
if exist "%JAVA_DIR%\bin\java.exe" (
    echo [OK] Using bundled Java 21
    set "JAVA_HOME=%JAVA_DIR%"
    set "JAVA_CMD=%JAVA_DIR%\bin\java.exe"
    goto :check_deps
)

:: Check system Java version
set "JAVA_OK=0"
set "JAVA_CMD=java"
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
    goto :check_deps
)

:: Need to download Java
echo [INFO] Java 17+ required. Found: !JAVA_VER!
echo [INFO] Downloading portable Java 21...
echo.

if not exist "%APP_DIR%temp" mkdir "%APP_DIR%temp"

curl -L -# -o "%APP_DIR%temp\java.zip" "https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.2+13/OpenJDK21U-jre_x64_windows_hotspot_21.0.2_13.zip"

if not exist "%APP_DIR%temp\java.zip" (
    echo [ERROR] Failed to download Java
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
    set "JAVA_CMD=%JAVA_DIR%\bin\java.exe"
) else (
    echo [ERROR] Java installation failed
    pause
    exit /b 1
)

:: ============================================================
:: DOWNLOAD DEPENDENCIES
:: ============================================================
:check_deps

if not exist "%LIB_DIR%" mkdir "%LIB_DIR%"

:: Check if we have the core jars
if exist "%LIB_DIR%\clojure.jar" (
    echo [OK] Dependencies cached
    goto :run_app
)

echo [INFO] Downloading dependencies (one-time)...

:: Download Clojure core
echo [INFO] Downloading Clojure 1.11.1...
curl -L -# -o "%LIB_DIR%\clojure.jar" "https://repo1.maven.org/maven2/org/clojure/clojure/1.11.1/clojure-1.11.1.jar"

:: Download spec.alpha
echo [INFO] Downloading spec.alpha...
curl -L -# -o "%LIB_DIR%\spec.alpha.jar" "https://repo1.maven.org/maven2/org/clojure/spec.alpha/0.3.218/spec.alpha-0.3.218.jar"

:: Download core.specs.alpha
echo [INFO] Downloading core.specs.alpha...
curl -L -# -o "%LIB_DIR%\core.specs.alpha.jar" "https://repo1.maven.org/maven2/org/clojure/core.specs.alpha/0.2.62/core.specs.alpha-0.2.62.jar"

:: Download CLJFX
echo [INFO] Downloading CLJFX...
curl -L -# -o "%LIB_DIR%\cljfx.jar" "https://repo1.maven.org/maven2/cljfx/cljfx/1.7.24/cljfx-1.7.24.jar"

:: Download JavaFX (Windows)
echo [INFO] Downloading JavaFX...
curl -L -# -o "%LIB_DIR%\javafx-base.jar" "https://repo1.maven.org/maven2/org/openjfx/javafx-base/21/javafx-base-21-win.jar"
curl -L -# -o "%LIB_DIR%\javafx-controls.jar" "https://repo1.maven.org/maven2/org/openjfx/javafx-controls/21/javafx-controls-21-win.jar"
curl -L -# -o "%LIB_DIR%\javafx-graphics.jar" "https://repo1.maven.org/maven2/org/openjfx/javafx-graphics/21/javafx-graphics-21-win.jar"

:: Download core.async
echo [INFO] Downloading core.async...
curl -L -# -o "%LIB_DIR%\core.async.jar" "https://repo1.maven.org/maven2/org/clojure/core.async/1.6.681/core.async-1.6.681.jar"

:: Download tools.analyzer
curl -L -# -o "%LIB_DIR%\tools.analyzer.jar" "https://repo1.maven.org/maven2/org/clojure/tools.analyzer/1.1.1/tools.analyzer-1.1.1.jar"
curl -L -# -o "%LIB_DIR%\tools.analyzer.jvm.jar" "https://repo1.maven.org/maven2/org/clojure/tools.analyzer.jvm/1.2.3/tools.analyzer.jvm-1.2.3.jar"

:: Download SQLite JDBC
echo [INFO] Downloading SQLite...
curl -L -# -o "%LIB_DIR%\sqlite-jdbc.jar" "https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.44.1.0/sqlite-jdbc-3.44.1.0.jar"

:: Download HTTP client
echo [INFO] Downloading HTTP client...
curl -L -# -o "%LIB_DIR%\clj-http.jar" "https://repo1.maven.org/maven2/clj-http/clj-http/3.12.3/clj-http-3.12.3.jar"
curl -L -# -o "%LIB_DIR%\httpcore.jar" "https://repo1.maven.org/maven2/org/apache/httpcomponents/httpcore/4.4.16/httpcore-4.4.16.jar"
curl -L -# -o "%LIB_DIR%\httpclient.jar" "https://repo1.maven.org/maven2/org/apache/httpcomponents/httpclient/4.5.14/httpclient-4.5.14.jar"
curl -L -# -o "%LIB_DIR%\httpmime.jar" "https://repo1.maven.org/maven2/org/apache/httpcomponents/httpmime/4.5.14/httpmime-4.5.14.jar"
curl -L -# -o "%LIB_DIR%\commons-logging.jar" "https://repo1.maven.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.jar"
curl -L -# -o "%LIB_DIR%\commons-codec.jar" "https://repo1.maven.org/maven2/commons-codec/commons-codec/1.16.0/commons-codec-1.16.0.jar"

:: Download Cheshire (JSON)
echo [INFO] Downloading JSON library...
curl -L -# -o "%LIB_DIR%\cheshire.jar" "https://repo1.maven.org/maven2/cheshire/cheshire/5.12.0/cheshire-5.12.0.jar"
curl -L -# -o "%LIB_DIR%\jackson-core.jar" "https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.15.3/jackson-core-2.15.3.jar"
curl -L -# -o "%LIB_DIR%\jackson-databind.jar" "https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.15.3/jackson-databind-2.15.3.jar"
curl -L -# -o "%LIB_DIR%\jackson-annotations.jar" "https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.15.3/jackson-annotations-2.15.3.jar"
curl -L -# -o "%LIB_DIR%\tigris.jar" "https://repo1.maven.org/maven2/tigris/tigris/0.1.2/tigris-0.1.2.jar"

:: Download Timbre (logging)
echo [INFO] Downloading logging library...
curl -L -# -o "%LIB_DIR%\timbre.jar" "https://repo1.maven.org/maven2/com/taoensso/timbre/6.3.1/timbre-6.3.1.jar"
curl -L -# -o "%LIB_DIR%\encore.jar" "https://repo1.maven.org/maven2/com/taoensso/encore/3.68.0/encore-3.68.0.jar"

echo [OK] Dependencies downloaded

:: ============================================================
:: RUN APPLICATION
:: ============================================================
:run_app
echo.
echo  ================================================================
echo                    STARTING APPLICATION
echo  ================================================================
echo.

cd /d "%APP_DIR%"

:: Build classpath
set "CP=%LIB_DIR%\clojure.jar"
set "CP=%CP%;%LIB_DIR%\spec.alpha.jar"
set "CP=%CP%;%LIB_DIR%\core.specs.alpha.jar"
set "CP=%CP%;%LIB_DIR%\cljfx.jar"
set "CP=%CP%;%LIB_DIR%\javafx-base.jar"
set "CP=%CP%;%LIB_DIR%\javafx-controls.jar"
set "CP=%CP%;%LIB_DIR%\javafx-graphics.jar"
set "CP=%CP%;%LIB_DIR%\core.async.jar"
set "CP=%CP%;%LIB_DIR%\tools.analyzer.jar"
set "CP=%CP%;%LIB_DIR%\tools.analyzer.jvm.jar"
set "CP=%CP%;%LIB_DIR%\sqlite-jdbc.jar"
set "CP=%CP%;%LIB_DIR%\clj-http.jar"
set "CP=%CP%;%LIB_DIR%\httpcore.jar"
set "CP=%CP%;%LIB_DIR%\httpclient.jar"
set "CP=%CP%;%LIB_DIR%\httpmime.jar"
set "CP=%CP%;%LIB_DIR%\commons-logging.jar"
set "CP=%CP%;%LIB_DIR%\commons-codec.jar"
set "CP=%CP%;%LIB_DIR%\cheshire.jar"
set "CP=%CP%;%LIB_DIR%\jackson-core.jar"
set "CP=%CP%;%LIB_DIR%\jackson-databind.jar"
set "CP=%CP%;%LIB_DIR%\jackson-annotations.jar"
set "CP=%CP%;%LIB_DIR%\tigris.jar"
set "CP=%CP%;%LIB_DIR%\timbre.jar"
set "CP=%CP%;%LIB_DIR%\encore.jar"
set "CP=%CP%;src"

echo [INFO] Starting Mental Models GUI...
echo.

"%JAVA_CMD%" --module-path "%LIB_DIR%" --add-modules javafx.controls,javafx.graphics -cp "%CP%" clojure.main -m mental-models.desktop.gui.app

if %ERRORLEVEL% neq 0 (
    echo.
    echo [ERROR] Application failed to start
    echo.
    echo Troubleshooting:
    echo   1. Make sure all files downloaded correctly
    echo   2. Check the lib folder has all jar files
    echo   3. Try deleting the lib folder and running again
    echo.
)

pause
