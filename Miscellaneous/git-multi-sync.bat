@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: ==========================================================
:: git-multi-sync.bat
:: Scans all immediate subfolders of this script's parent
:: directory. For each Git repo:
::  - git fetch --all --prune
::  - auto-commit any local changes
::  - git pull --rebase --autostash
::  - push to upstream (create upstream if missing)
:: Notes:
::  - Runs on the CURRENT branch of each repo.
::  - Skips repos with merge/rebase conflicts.
::  - Requires Git in PATH.
:: ==========================================================

:: Jump to the parent folder of where this script lives
cd /d "%~dp0.."

:: Ensure git exists
where git >nul 2>&1
if errorlevel 1 (
  echo [ERROR] Git is not available on PATH. Install Git for Windows or add it to PATH.
  exit /b 1
)

echo.
echo ==========================================================
echo   Starting multi-repo sync in: %cd%
echo   Time: %DATE% %TIME%
echo ==========================================================
echo.

for /d %%D in (*) do (
  if exist "%%D\.git" (
    echo ----------------------------------------------------------
    echo [INFO] Repo: %%D
    pushd "%%D" >nul

    :: Show current branch (if repo is valid)
    for /f "usebackq tokens=* delims=" %%b in (`git rev-parse --abbrev-ref HEAD 2^>nul`) do set "BRANCH=%%b"
    if not defined BRANCH (
      echo [WARN]   Could not detect branch (detached HEAD or invalid repo?). Skipping.
      set "BRANCH="
      popd >nul
      goto :continueLoop
    )
    echo [INFO]   Branch: !BRANCH!

    :: Fetch latest (and prune deleted remote refs)
    git fetch --all --prune
    if errorlevel 1 (
      echo [WARN]   Fetch failed. Skipping this repo.
      set "BRANCH="
      popd >nul
      goto :continueLoop
    )

    :: Auto-commit local uncommitted changes (if any)
    for /f "usebackq tokens=* delims=" %%s in (`git status --porcelain`) do set "DIRTY=1"
    if defined DIRTY (
      echo [INFO]   Local changes detected. Auto-committing...
      git add -A
      git commit -m "chore: auto-commit %DATE% %TIME%" >nul 2>&1
      if errorlevel 1 (
        echo [WARN]   Nothing to commit or commit failed.
      ) else (
        echo [INFO]   Committed local changes.
      )
      set "DIRTY="
    ) else (
      echo [INFO]   No uncommitted changes.
    )

    :: Pull remote changes with rebase and autostash
    echo [INFO]   Pulling from upstream (rebase + autostash)...
    git pull --rebase --autostash
    if errorlevel 1 (
      echo [ERROR]  Pull failed (conflicts or other issue). Resolve manually and re-run. Skipping push.
      set "BRANCH="
      popd >nul
      goto :continueLoop
    )

    :: Ensure upstream exists; if not, set to origin/BRANCH
    set "HAS_UPSTREAM="
    for /f "usebackq tokens=* delims=" %%u in (`git rev-parse --abbrev-ref "@{u}" 2^>nul`) do set "HAS_UPSTREAM=1"
    if not defined HAS_UPSTREAM (
      echo [INFO]   No upstream set. Creating origin/!BRANCH! if needed and setting upstream...
      for /f "usebackq tokens=* delims=" %%h in (`git ls-remote --heads origin !BRANCH! ^| findstr /r /c:"."`) do set "REMOTE_HEAD=1"
      if not defined REMOTE_HEAD (
        echo [INFO]   Remote branch doesn't exist yet; will create on push.
      )
      git push -u origin !BRANCH!
      if errorlevel 1 (
        echo [ERROR]  Push failed when setting upstream.
        set "BRANCH="
        popd >nul
        goto :continueLoop
      )
    ) else (
      :: Upstream exists; do a regular push
      echo [INFO]   Pushing to upstream...
      git push
      if errorlevel 1 (
        echo [ERROR]  Push failed.
        set "BRANCH="
        popd >nul
        goto :continueLoop
      )
    )

    echo [OK]     Sync complete for %%D (!BRANCH!).
    set "BRANCH="
    popd >nul
  )
  :continueLoop
)

echo.
echo ==========================================================
echo   All done. Check messages above for any repos requiring
echo   manual attention (conflicts, auth, etc.).
echo ==========================================================
exit /b 0
