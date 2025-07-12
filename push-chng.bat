@echo off
setlocal EnableDelayedExpansion

REM Store the root directory
set ROOT_DIR=%CD%
echo Searching for Git repositories under %ROOT_DIR%...

REM Recursive function to check and push each Git repo
for /r %%d in (.) do (
    if exist "%%d\.git" (
        pushd "%%d"
        echo --------------------------------------
        echo Processing Git repo at %%d

        REM Check if in merge state
        if exist ".git\MERGE_HEAD" (
            echo Merge conflict detected in %%d
            echo Please resolve conflicts manually.
            goto skip
        )

        REM Check if repo is clean
        git diff --quiet && git diff --cached --quiet
        if not !errorlevel! == 0 (
            echo Changes detected in %%d

            REM Add all changes
            git add -A

            REM Try to commit
            git commit -m "Auto-commit from sync_all_git_repos.bat"

            REM Push changes
            git push
        ) else (
            echo No local changes. Pulling latest changes...
            git pull --no-edit

            REM Check again in case there were uncommitted changes after pull
            git diff --quiet && git diff --cached --quiet
            if not !errorlevel! == 0 (
                git add -A
                git commit -m "Auto-commit after pull from sync_all_git_repos.bat"
                git push
            )
        )

        :skip
        popd
    )
)

REM Now go back to the root directory and commit/push it
cd /d %ROOT_DIR%
echo --------------------------------------
echo Now pushing changes from root directory: %ROOT_DIR%

if exist ".git\MERGE_HEAD" (
    echo Merge conflict detected at root. Please resolve it manually.
    goto end
)

git diff --quiet && git diff --cached --quiet
if not %errorlevel% == 0 (
    git add -A
    git commit -m "Auto-commit from root by sync_all_git_repos.bat"
    git push
) else (
    git pull --no-edit
    git diff --quiet && git diff --cached --quiet
    if not %errorlevel% == 0 (
        git add -A
        git commit -m "Auto-commit after pull from root"
        git push
    )
)

:end
echo --------------------------------------
echo ✅ Git sync complete for all subdirectories and root.
pause
