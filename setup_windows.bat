@echo off
cd /d "%~dp0"
powershell -ExecutionPolicy Bypass -File "scripts\bootstrap_windows.ps1"
pause
