@echo off
setlocal enabledelayedexpansion

REM VeLang Installation Script for Windows
REM Requires PowerShell and Git

set "VELANG_VERSION=0.1.0"
set "INSTALL_DIR=%USERPROFILE%\.velang"
set "TEMP_DIR=%TEMP%\velang_install"

echo.
echo ╭─────────────────────────────────────╮
echo │           VeLang Installer          │
echo │          Version %VELANG_VERSION%              │
echo ╰─────────────────────────────────────╯
echo.

echo [INFO] Checking dependencies...

REM Check for Git
git --version >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Git is not installed or not in PATH
    echo Please install Git from: https://git-scm.com/download/win
    pause
    exit /b 1
)

REM Check for Rust/Cargo
cargo --version >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Rust/Cargo is not installed
    echo Please install Rust from: https://rustup.rs/
    pause
    exit /b 1
)

REM Check for Visual Studio Build Tools or MSVC
cl >nul 2>&1
if errorlevel 1 (
    echo [WARNING] Visual Studio Build Tools not detected
    echo VeLang requires MSVC compiler for C code generation
    echo Please install Visual Studio Build Tools from:
    echo https://visualstudio.microsoft.com/visual-cpp-build-tools/
    echo.
    set /p continue="Continue anyway? (y/N): "
    if /i not "!continue!"=="y" exit /b 1
)

echo [SUCCESS] Dependencies check completed

echo [INFO] Creating temporary directory...
if exist "%TEMP_DIR%" rmdir /s /q "%TEMP_DIR%"
mkdir "%TEMP_DIR%"

echo [INFO] Downloading VeLang source code...
cd /d "%TEMP_DIR%"
git clone https://github.com/velang-org/ve.git >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Failed to clone VeLang repository
    pause
    exit /b 1
)
echo [SUCCESS] Source code downloaded successfully

cd ve

echo [INFO] Building VeLang...
cargo build --release --quiet >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Failed to build VeLang
    pause
    exit /b 1
)

echo [SUCCESS] VeLang built successfully

echo [INFO] Installing VeLang...
if not exist "%INSTALL_DIR%" mkdir "%INSTALL_DIR%"
copy "target\release\ve.exe" "%INSTALL_DIR%\ve.exe" >nul

REM Copy standard library
if exist "lib" (
    echo [INFO] Installing standard library...
    xcopy "lib" "%INSTALL_DIR%\lib" /E /I /Q >nul
    echo [SUCCESS] Standard library installed
) else (
    echo [WARNING] Standard library directory not found - some imports may fail
)

REM Add to PATH (requires PowerShell)
echo [INFO] Adding VeLang to system PATH...
powershell -Command "$env:Path = [Environment]::GetEnvironmentVariable('Path','User'); if ($env:Path -notlike '*%INSTALL_DIR%*') { [Environment]::SetEnvironmentVariable('Path', $env:Path + ';%INSTALL_DIR%', 'User') }"

echo [SUCCESS] VeLang installed to %INSTALL_DIR%

echo [INFO] Cleaning up...
cd /d "%TEMP%"
rmdir /s /q "%TEMP_DIR%"

echo [INFO] Verifying installation...
"%INSTALL_DIR%\ve.exe" --version >nul 2>&1
if errorlevel 1 (
    echo [WARNING] VeLang binary exists but may not be working correctly
) else (
    echo [SUCCESS] VeLang is working correctly
)

echo.
echo [SUCCESS] VeLang installation completed!
echo.
echo To get started:
echo   ve --help                 # Show help
echo   ve init my_project        # Create a new project  
echo   ve example.ve             # Compile and run a file
echo.
echo Join our Discord: https://dsc.gg/velang
echo.
echo [INFO] Please restart your command prompt to use 've' command
echo.
pause
