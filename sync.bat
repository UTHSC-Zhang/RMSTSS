@echo off
setlocal enabledelayedexpansion

REM === Paths relative to RMSTSS root ===
set APP_DIR=RMSTSS-App
set PACKAGE_DIR=RMSTSS-Package

set APP_R=%APP_DIR%\R
set PACKAGE_R=%PACKAGE_DIR%\R

set SRC_APP_R_FILE=%APP_DIR%\app.R
set DST_APP_R_FILE=%PACKAGE_DIR%\inst\shiny_app\app.R

set COPY_R_DONE=0
set COPY_APP_R_DONE=0
set COMMIT_DONE=0
set PUSH_DONE=0

echo ==========================================
echo 🔁 Starting sync from App to Package...
echo ==========================================

REM === Step 0: git pull latest from remote ===
echo ⬇️ Pulling latest changes in RMSTSS-Package...
pushd "%PACKAGE_DIR%" >nul
git pull
popd >nul

REM === Step 1: Copy R/ folder ===
if not exist "%APP_R%" (
    echo ❌ ERROR: Source folder not found: %APP_R%
) else (
    if not exist "%PACKAGE_R%" (
        echo 📁 Creating destination folder: %PACKAGE_R%
        mkdir "%PACKAGE_R%"
    )
    echo 🔄 Copying R folder from App → Package (non-destructive)
    xcopy /E /Y /D "%APP_R%\*" "%PACKAGE_R%\" >nul
    if %errorlevel% equ 0 (
        echo ✅ R folder copied successfully.
        set COPY_R_DONE=1
    ) else (
        echo ⚠️ Warning: Problem copying R folder.
    )
)

REM === Step 2: Copy app.R to inst/shiny_app/ ===
if not exist "%SRC_APP_R_FILE%" (
    echo ❌ ERROR: Source app.R not found: %SRC_APP_R_FILE%
) else (
    if not exist "%PACKAGE_DIR%\inst\shiny_app" (
        echo 📁 Creating destination folder: %PACKAGE_DIR%\inst\shiny_app
        mkdir "%PACKAGE_DIR%\inst\shiny_app"
    )
    echo 🔄 Copying app.R to Package\inst\shiny_app\
    copy /Y "%SRC_APP_R_FILE%" "%DST_APP_R_FILE%" >nul
    if exist "%DST_APP_R_FILE%" (
        echo ✅ app.R copied successfully.
        set COPY_APP_R_DONE=1
    ) else (
        echo ⚠️ Warning: app.R copy failed.
    )
)

REM === Step 3: Git commit and push if any changes ===
pushd "%PACKAGE_DIR%" >nul
for /f "delims=" %%a in ('git status --porcelain R inst\shiny_app\app.R') do (
    echo 📝 Committing changes in RMSTSS-Package...
    git add R inst\shiny_app\app.R
    git commit -m "Auto-sync: Updated R folder and app.R from App on %date% %time%"
    set COMMIT_DONE=1
    goto commit_done
)
echo ✅ No changes to commit.
:commit_done

if !COMMIT_DONE! equ 1 (
    echo ⬆️ Pushing committed changes to remote...
    git push
    set PUSH_DONE=1
)
popd >nul

REM === Summary ===
echo.
echo ==========================================
echo ✅ SYNC SUMMARY
echo ==========================================
if !COPY_R_DONE! equ 1 (
    echo - R folder sync:    SUCCESS
) else (
    echo - R folder sync:    ❌ FAILED
)

if !COPY_APP_R_DONE! equ 1 (
    echo - app.R copy:       SUCCESS
) else (
    echo - app.R copy:       ❌ FAILED
)

if !COMMIT_DONE! equ 1 (
    echo - Git commit:       DONE
) else (
    echo - Git commit:       Skipped (no changes)
)

if !PUSH_DONE! equ 1 (
    echo - Git push:         DONE
) else (
    echo - Git push:         Skipped
)
echo ==========================================
pause
endlocal
