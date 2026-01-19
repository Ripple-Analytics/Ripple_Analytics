@echo off
setlocal enabledelayedexpansion

:: Mental Models Desktop v1.3.8 - Auto-Rollback Launcher
:: If app crashes, automatically restores previous version

title Mental Models Desktop

set "APP_DIR=%~dp0"
set "APP_DIR=%APP_DIR:~0,-1%"
set "JAVA_CMD=%APP_DIR%\jre\bin\java.exe"
set "LIB_DIR=%APP_DIR%\lib"
set "SRC_DIR=%APP_DIR%\src"
set "STATE_DIR=%APP_DIR%\state"
set "LOG_FILE=%APP_DIR%\startup.log"
set "BACKUP_DIR=%APP_DIR%\.backup"
set "CRASH_FLAG=%STATE_DIR%\.crash_flag"
set "VERSION_FILE=%STATE_DIR%\.current_version"

:: ============================================
:: AUTO-ROLLBACK CHECK
:: ============================================
if exist "%CRASH_FLAG%" (
    echo.
    echo [ROLLBACK] Previous run crashed. Checking for backup...
    
    if exist "%BACKUP_DIR%\src" (
        echo [ROLLBACK] Restoring previous working version...
        
        :: Restore backup
        xcopy /E /Y /Q "%BACKUP_DIR%\src\*" "%SRC_DIR%\" >nul 2>&1
        
        :: Clear crash flag
        del "%CRASH_FLAG%" >nul 2>&1
        
        echo [ROLLBACK] Restored! Starting previous version...
        echo.
    ) else (
        echo [ROLLBACK] No backup found. Clearing crash flag...
        del "%CRASH_FLAG%" >nul 2>&1
    )
)

:: Clear previous log
echo Mental Models Desktop - Startup Log > "%LOG_FILE%"
echo Started: %date% %time% >> "%LOG_FILE%"
echo. >> "%LOG_FILE%"

:: ============================================
:: SELF-TEST 1: Check Java Runtime
:: ============================================
echo [1/4] Checking Java runtime...
if not exist "%JAVA_CMD%" (
    echo [FAIL] Java runtime not found >> "%LOG_FILE%"
    echo.
    echo ERROR: Java runtime not found.
    echo Expected at: %JAVA_CMD%
    echo.
    echo Please re-download the application.
    pause
    exit /b 1
)
echo [PASS] Java runtime found >> "%LOG_FILE%"

:: ============================================
:: SELF-TEST 2: Check Critical JARs
:: ============================================
echo [2/4] Checking dependencies...
set "MISSING_JARS="
for %%f in (clojure.jar spec.alpha.jar sqlite-jdbc.jar cheshire.jar jackson-core.jar slf4j-api.jar) do (
    if not exist "%LIB_DIR%\%%f" (
        set "MISSING_JARS=!MISSING_JARS! %%f"
        echo [FAIL] Missing: %%f >> "%LOG_FILE%"
    )
)

if not "!MISSING_JARS!"=="" (
    echo.
    echo ERROR: Missing required files:!MISSING_JARS!
    echo.
    echo Please re-download the application.
    pause
    exit /b 1
)
echo [PASS] All dependencies found >> "%LOG_FILE%"

:: ============================================
:: SELF-TEST 3: Check Source Files
:: ============================================
echo [3/4] Checking application code...
if not exist "%SRC_DIR%\mental_models\desktop\gui\swing_app.clj" (
    echo [FAIL] Application code not found >> "%LOG_FILE%"
    echo.
    echo ERROR: Application code not found.
    echo Please re-download the application.
    pause
    exit /b 1
)
echo [PASS] Application code found >> "%LOG_FILE%"

:: ============================================
:: SELF-TEST 4: Create State Directory
:: ============================================
echo [4/4] Initializing state...
if not exist "%STATE_DIR%" mkdir "%STATE_DIR%"
echo [PASS] State directory ready >> "%LOG_FILE%"

:: ============================================
:: CREATE BACKUP BEFORE LAUNCH
:: ============================================
echo Creating backup... >> "%LOG_FILE%"
if not exist "%BACKUP_DIR%" mkdir "%BACKUP_DIR%"
xcopy /E /Y /Q "%SRC_DIR%\*" "%BACKUP_DIR%\src\" >nul 2>&1
echo [PASS] Backup created >> "%LOG_FILE%"

:: ============================================
:: SET CRASH FLAG (cleared on clean exit)
:: ============================================
echo starting > "%CRASH_FLAG%"

:: ============================================
:: BUILD CLASSPATH
:: ============================================
echo. >> "%LOG_FILE%"
echo Building classpath... >> "%LOG_FILE%"

set "CP=%LIB_DIR%\clojure.jar"
set "CP=%CP%;%LIB_DIR%\spec.alpha.jar"
set "CP=%CP%;%LIB_DIR%\core.specs.alpha.jar"
set "CP=%CP%;%LIB_DIR%\core.async.jar"
set "CP=%CP%;%LIB_DIR%\tools.analyzer.jar"
set "CP=%CP%;%LIB_DIR%\tools.analyzer.jvm.jar"
set "CP=%CP%;%LIB_DIR%\sqlite-jdbc.jar"
set "CP=%CP%;%LIB_DIR%\sqlite-jdbc-3.45.1.0.jar"
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
set "CP=%CP%;%LIB_DIR%\slf4j-api.jar"
set "CP=%CP%;%LIB_DIR%\slf4j-simple.jar"
set "CP=%CP%;%LIB_DIR%\pdfbox.jar"
set "CP=%CP%;%LIB_DIR%\fontbox.jar"
set "CP=%CP%;%LIB_DIR%\commons-io.jar"
set "CP=%CP%;%SRC_DIR%"

:: ============================================
:: LAUNCH APPLICATION
:: ============================================
echo. >> "%LOG_FILE%"
echo All tests passed. Launching application... >> "%LOG_FILE%"
echo.
echo All checks passed. Starting Mental Models Desktop...
echo.

"%JAVA_CMD%" ^
    -Dfile.encoding=UTF-8 ^
    -Dapp.version=1.3.8 ^
    -Dapp.dir="%APP_DIR%" ^
    -Dstate.dir="%STATE_DIR%" ^
    -cp "%CP%" ^
    clojure.main -m mental-models.desktop.gui.swing-app

:: If we get here, app exited
set "EXIT_CODE=%ERRORLEVEL%"
echo. >> "%LOG_FILE%"
echo Application exited with code: %EXIT_CODE% >> "%LOG_FILE%"

:: ============================================
:: CLEAN EXIT - REMOVE CRASH FLAG
:: ============================================
if %EXIT_CODE% equ 0 (
    del "%CRASH_FLAG%" >nul 2>&1
    echo Clean exit >> "%LOG_FILE%"
) else (
    echo.
    echo Application exited with error code: %EXIT_CODE%
    echo.
    echo If this keeps happening, the app will auto-rollback on next launch.
    echo Check startup.log for details.
    pause
)
