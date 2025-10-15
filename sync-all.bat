@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: =========================================================
:: sync-all-git.bat
:: Recursively fetch/pull/commit/push every git repo under CWD,
:: then commit/push the main (root) repo last.
:: Usage: sync-all-git.bat [your commit message]
:: If no message is given, a timestamped default is used.
:: =========================================================

:: Commit message
set "MSG=%*"
if "%MSG%"=="" (
  for /f "tokens=1-3 delims=/ " %%a in ("%date%") do set "TODAY=%%a-%%b-%%c"
  set "MSG=Auto-sync: %TODAY% %time%"
)

set "ROOT=%CD%"

echo.
echo =========================================================
echo Searching for git repositories under:
echo   %ROOT%
echo =========================================================
echo.

:: --- Process sub-repos (all subdirectories that contain a .git folder) ---
for /d /r %%D in (*) do (
  if exist "%%D\.git" (
    :: Skip if this is the ROOT (we'll do root last)
    if /I not "%%D"=="%ROOT%" (
      call :PROCESS_REPO "%%D" "%MSG%"
    )
  )
)

:: --- Finally, process the main (root) repo, if it is a git repo ---
if exist "%ROOT%\.git" (
  echo.
  echo =========================================================
  echo Processing MAIN repo: %ROOT%
  echo =========================================================
  call :PROCESS_REPO "%ROOT%" "%MSG%"
) else (
  echo.
  echo (Root folder is not a git repo; skipping root commit/push.)
)

echo.
echo All done. 🚀
exit /b 0

:: =========================================================
:: Subroutine: PROCESS_REPO <path> <message>
:: =========================================================
:PROCESS_REPO
set "REPO=%~1"
set "CMSG=%~2"

echo.
echo --- Repo: %REPO%
pushd "%REPO%" >nul 2>&1

:: Verify it’s really a git work tree (in case of oddities)
git rev-parse --is-inside-work-tree >nul 2>&1
if errorlevel 1 (
  echo   (Not a valid git work tree; skipping)
  popd >nul 2>&1
  goto :eof
)

:: Show path and current branch
for /f "delims=" %%b in ('git rev-parse --abbrev-ref HEAD 2^>nul') do set "BR=%%b"
if not defined BR set "BR=main"
echo   Branch: %BR%

echo   Fetching...
git fetch --all --prune

echo   Pulling (fast-forward only)...
git pull --ff-only origin %BR% 2>nul

echo   Staging changes...
git add -A

:: Commit only if there are staged changes
git diff --cached --quiet
if errorlevel 1 (
  echo   Committing...
  git commit -m "%CMSG%"
) else (
  echo   No changes to commit.
)

:: Push (set upstream if missing)
git rev-parse --abbrev-ref --symbolic-full-name @{u} >nul 2>&1
if errorlevel 1 (
  echo   Pushing (setting upstream)...
  git push -u origin %BR%
) else (
  echo   Pushing...
  git push
)

popd >nul 2>&1
goto :eof
