@echo off
setlocal EnableExtensions EnableDelayedExpansion

REM === Determine script directory and set up logging ===
set "SCRIPT_DIR=%~dp0"
set "LOG_DIR=%SCRIPT_DIR%logs"
if not exist "%LOG_DIR%" mkdir "%LOG_DIR%" >nul 2>&1

for /f "tokens=1-3 delims=/:. " %%a in ("%date% %time%") do (
  set "TS=%%a-%%b-%%c_%%d%%e%%f"
)
set "LOG_FILE=%LOG_DIR%\run_!TS!.log"

call :log "===== Maintenance run started at %date% %time% ====="

REM === Step 1: Run del-extra.bat ===
call :runStep "del-extra.bat"

REM === Step 2: Run push-chng.bat ===
call :runStep "git-multi-sync.bat"

call :log "===== Maintenance run completed at %date% %time% with EXITCODE %ERRORLEVEL% ====="
exit /b %ERRORLEVEL%

REM ---------- helpers ----------
:runStep
set "STEP=%~1"
set "TARGET=%SCRIPT_DIR%%STEP%"
if not exist "%TARGET%" (
  call :log "ERROR: File not found: %TARGET%"
  exit /b 1
)
call :log "Running %STEP% ..."
call "%TARGET%" >> "%LOG_FILE%" 2>&1
set "EC=%ERRORLEVEL%"
if not "!EC!"=="0" (
  call :log "%STEP% failed with error code !EC!"
  exit /b !EC!
) else (
  call :log "%STEP% completed successfully."
)
exit /b 0

:log
echo [%date% %time%] %~1
echo [%date% %time%] %~1>> "%LOG_FILE%"
exit /b 0
