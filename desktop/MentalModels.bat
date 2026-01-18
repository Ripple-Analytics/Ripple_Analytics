@echo off
:: Mental Models Desktop App - One-Click Launcher
:: Double-click this file to start the application

cd /d "%~dp0"
powershell -ExecutionPolicy Bypass -File "%~dp0Start-MentalModels.ps1"
