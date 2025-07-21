@echo off
setlocal enabledelayedexpansion

echo Searching for files containing "kratos" in the name...

:: Loop through all files in current directory and subdirectories
for /r %%f in (*kratos*) do (
    echo Deleting: "%%f"
    del /f /q "%%f"
)

echo Done.
pause
