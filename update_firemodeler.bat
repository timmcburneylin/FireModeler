@echo off
cd /d "%~dp0"

powershell -NoProfile -ExecutionPolicy Bypass -File "scripts\update_firemodeler.ps1"
set "UPDATE_EXIT=%ERRORLEVEL%"

echo.
if "%UPDATE_EXIT%"=="0" (
  echo FireModeler update finished successfully.
) else (
  echo FireModeler was not updated. Review the message above.
)
pause
exit /b %UPDATE_EXIT%
