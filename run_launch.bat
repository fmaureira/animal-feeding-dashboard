@echo off
setlocal

REM Try to find Rscript in PATH first
where Rscript >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo ✅ Found Rscript in system PATH
    Rscript launch.R
    goto :eof
)

REM If not in PATH, search common install directories
set "R_BASE=C:\Program Files\R"
set "RSCRIPT_PATH="

for /f "delims=" %%v in ('dir /b /ad "%R_BASE%"') do (
    if exist "%R_BASE%\%%v\bin\Rscript.exe" (
        set "RSCRIPT_PATH=%R_BASE%\%%v\bin\Rscript.exe"
        goto :found
    )
)

:found
if not defined RSCRIPT_PATH (
    echo ❌ No Rscript.exe found in PATH or under "%R_BASE%"
    exit /b 1
)

echo ✅ Found Rscript at: %RSCRIPT_PATH%
"%RSCRIPT_PATH%" launch.R
pause