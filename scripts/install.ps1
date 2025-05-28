# VeLang Installation Script for Windows PowerShell
# Requires Git and Rust/Cargo

param(
    [string]$Branch = $env:VELANG_BRANCH
)

# Configuration
$VELANG_VERSION = "0.1.0"
$INSTALL_DIR = Join-Path $env:USERPROFILE ".velang"
$TEMP_DIR = Join-Path $env:TEMP "velang_install"

# Function to display formatted messages
function Write-Message {
    param(
        [string]$Type,
        [string]$Message
    )
    
    switch ($Type) {
        "INFO" { Write-Host "[INFO] $Message" -ForegroundColor Cyan }
        "SUCCESS" { Write-Host "[SUCCESS] $Message" -ForegroundColor Green }
        "WARNING" { Write-Host "[WARNING] $Message" -ForegroundColor Yellow }
        "ERROR" { Write-Host "[ERROR] $Message" -ForegroundColor Red }
    }
}

# Function to check if command exists
function Test-Command {
    param([string]$Command)
    
    try {
        Get-Command $Command -ErrorAction Stop | Out-Null
        return $true
    }
    catch {
        return $false
    }
}

# Function to exit with pause
function Exit-WithPause {
    Write-Host "Press any key to exit..."
    $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    exit 1
}


Write-Message 'INFO' 'Checking dependencies...'

# Check for Git
if (-not (Test-Command "git")) {
    Write-Message "ERROR" "Git is not installed or not in PATH"
    Write-Host "Please install Git from: https://git-scm.com/download/win"
    Exit-WithPause
}

# Check for Rust/Cargo
if (-not (Test-Command "cargo")) {
    Write-Message "ERROR" "Rust/Cargo is not installed"
    Write-Host "Please install Rust from: https://rustup.rs/"
    Exit-WithPause
}

# Check for Visual Studio Build Tools or MSVC
if (-not (Test-Command "cl")) {
    Write-Message "WARNING" "Visual Studio Build Tools not detected"
    Write-Host "VeLang requires MSVC compiler for C code generation"
    Write-Host "Please install Visual Studio Build Tools from:"
    Write-Host "https://visualstudio.microsoft.com/visual-cpp-build-tools/"
    Write-Host ""
    $continue = Read-Host 'Continue anyway? (y/N)'
    if ($continue.ToLower() -ne "y") {
        exit 1
    }
}

Write-Message "SUCCESS" "Dependencies check completed"

# Create temporary directory
Write-Message "INFO" "Creating temporary directory..."
if (Test-Path $TEMP_DIR) {
    Remove-Item $TEMP_DIR -Recurse -Force
}
New-Item -ItemType Directory -Path $TEMP_DIR -Force | Out-Null

# Download VeLang source code
Write-Message "INFO" "Downloading VeLang source code..."
Set-Location $TEMP_DIR

# Determine which branch to use - priority order:
# 1. Parameter/VELANG_BRANCH environment variable
# 2. Auto-detection from script context
# 3. Default to main
if (-not $Branch) {
    $Branch = "main"
    # Check if we're being run from feature/installer context
    if ($PSCommandPath -like "*feature/installer*") {
        $Branch = "feature/installer"
        Write-Message "INFO" "Auto-detected development branch: $Branch"
    }
} else {
    Write-Message "INFO" "Using branch from environment: $Branch"
}

try {
    git clone -b $Branch https://github.com/velang-org/ve.git --quiet 2>$null
    if ($LASTEXITCODE -ne 0) {
        throw "Git clone failed"
    }
}
catch {
    Write-Message "ERROR" "Failed to clone VeLang repository from branch '$Branch'"
    Exit-WithPause
}

Write-Message "SUCCESS" "Source code downloaded successfully"

Set-Location "ve"

# Build VeLang
Write-Message "INFO" "Building VeLang..."
try {
    cargo build --release --quiet 2>$null
    if ($LASTEXITCODE -ne 0) {
        throw "Cargo build failed"
    }
}
catch {
    Write-Message "ERROR" "Failed to build VeLang"
    Exit-WithPause
}

Write-Message "SUCCESS" "VeLang built successfully"

# Install VeLang
Write-Message "INFO" "Installing VeLang..."
if (-not (Test-Path $INSTALL_DIR)) {
    New-Item -ItemType Directory -Path $INSTALL_DIR -Force | Out-Null
}

$sourceExe = Join-Path "target" "release" "ve.exe"
$targetExe = Join-Path $INSTALL_DIR "ve.exe"
Copy-Item $sourceExe $targetExe -Force

# Copy standard library
if (Test-Path "lib") {
    Write-Message "INFO" "Installing standard library..."
    $libTarget = Join-Path $INSTALL_DIR "lib"
    Copy-Item "lib" $libTarget -Recurse -Force
    Write-Message "SUCCESS" "Standard library installed"
} else {
    Write-Message "WARNING" "Standard library directory not found - some imports may fail"
}

# Add to PATH
Write-Message "INFO" "Adding VeLang to system PATH..."

try {
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if ($userPath -notlike "*$($INSTALL_DIR)*") {
        if ($userPath) {
            $newPath = $userPath + ';' + $INSTALL_DIR
        } else {
            $newPath = $INSTALL_DIR
        }
        [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
        Write-Message 'SUCCESS' 'Added VeLang directory to user PATH'
    } else {
        Write-Message 'INFO' 'VeLang directory already in PATH, skipping'
    }
}
catch {
    Write-Message 'WARNING' 'Failed to add VeLang to PATH. You may need to add it manually.'
}

Write-Message 'SUCCESS' 'VeLang installed to $INSTALL_DIR'

# Cleanup
Write-Message 'INFO' 'Cleaning up...'
Set-Location $env:TEMP
Remove-Item $TEMP_DIR -Recurse -Force

# Verify installation
Write-Message 'INFO' 'Verifying installation...'
try {
    $veExe = Join-Path $INSTALL_DIR 've.exe'
    & $veExe --version 2>$null | Out-Null
    if ($LASTEXITCODE -ne 0) {
        throw 'VeLang verification failed'
    }
    Write-Message 'SUCCESS' 'VeLang is working correctly'
}
catch {
    Write-Message 'WARNING' 'VeLang binary exists but may not be working correctly'
}

# Final success message
Write-Host ''
Write-Message 'SUCCESS' 'VeLang installation completed!'
Write-Host ''
Write-Host 'To get started:'
Write-Host '  ve --help                 # Show help'
Write-Host '  ve init my_project        # Create a new project'
Write-Host '  ve example.ve             # Compile and run a file'
Write-Host ''
Write-Host 'Join our Discord: https://dsc.gg/velang'
Write-Host ''
Write-Message 'INFO' 'Please restart your command prompt to use ''ve'' command'
Write-Host ''

# Pause for interactive sessions
if ($Host.Name -eq 'ConsoleHost') {
    Write-Host 'Press any key to exit...'
    $null = $Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')
}