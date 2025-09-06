@echo off
setlocal enabledelayedexpansion

echo Searching for files containing "kratos" in the name...

:: Loop through all files in current directory and subdirectories
cd /d "%~dp0.."

for /r %%f in (*kratos*,*Atreus*,*atreus*,*Kratos*) do (
    echo Deleting: "%%f"
    del /f /q "%%f"
)

echo Done.
pause
