@echo off
setlocal enabledelayedexpansion

echo.
echo ============================================================
echo   AUTO-UPDATER WATCHDOG - Ensures updater NEVER stops
echo   Run this in VS Code terminal to monitor continuously
echo ============================================================
echo.

cd /d "%~dp0"

:: Get Machine GUID for license verification
for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\Microsoft\Cryptography" /v MachineGuid 2^>nul ^| find "MachineGuid"') do set MACHINE_GUID=%%b

:: Update .env with Machine GUID
if defined MACHINE_GUID (
    findstr /v "MACHINE_GUID" .env > .env.tmp 2>nul
    echo MACHINE_GUID=%MACHINE_GUID% >> .env.tmp
    move /y .env.tmp .env >nul 2>&1
)

:watchdog_loop
echo [%date% %time%] Checking auto-updater status...

:: Check if auto-updater container is running
docker ps --filter "name=mental-models-auto-updater" --format "{{.Status}}" > nul 2>&1
for /f "tokens=*" %%a in ('docker ps --filter "name=mental-models-auto-updater" --format "{{.Status}}"') do set UPDATER_STATUS=%%a

if not defined UPDATER_STATUS (
    echo [%date% %time%] AUTO-UPDATER NOT RUNNING - Starting it now!
    docker-compose up -d auto-updater-service
    timeout /t 5 /nobreak >nul
) else (
    echo [%date% %time%] Auto-updater is running: %UPDATER_STATUS%
)

:: Check if container is healthy by checking logs for recent heartbeat
for /f "tokens=*" %%a in ('docker logs --tail 5 mental-models-auto-updater 2^>^&1 ^| findstr "HEARTBEAT"') do set HEARTBEAT=%%a

if defined HEARTBEAT (
    echo [%date% %time%] Heartbeat detected - updater is healthy
) else (
    echo [%date% %time%] No recent heartbeat - restarting container
    docker restart mental-models-auto-updater
)

:: Clear variables for next iteration
set UPDATER_STATUS=
set HEARTBEAT=

:: Wait 60 seconds before next check
echo [%date% %time%] Next check in 60 seconds... (Press Ctrl+C to stop watchdog)
timeout /t 60 /nobreak >nul

goto watchdog_loop
