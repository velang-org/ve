# VeLang Installation Script for Windows (PowerShell)
# Requires PowerShell 5.0+ and Git

param(
    [switch]$Force,
    [string]$InstallPath = "$env:USERPROFILE\.velang"
)

$ErrorActionPreference = "Stop"
$VelangVersion = "0.1.0"

# Colors for console output
$Colors = @{
    Info = "Cyan"
    Success = "Green" 
    Warning = "Yellow"
    Error = "Red"
}

function Write-ColoredOutput {
    param(
        [string]$Message,
        [string]$Type = "Info"
    )
    
    $prefix = switch ($Type) {
        "Info" { "[INFO]" }
        "Success" { "[SUCCESS]" }
        "Warning" { "[WARNING]" }
        "Error" { "[ERROR]" }
    }
    
    Write-Host "$prefix $Message" -ForegroundColor $Colors[$Type]
}

function Show-Banner {
    Write-Host ""
    Write-Host "╭─────────────────────────────────────╮" -ForegroundColor Blue
    Write-Host "│           VeLang Installer          │" -ForegroundColor Blue  
    Write-Host "│          Version $VelangVersion              │" -ForegroundColor Blue
    Write-Host "╰─────────────────────────────────────╯" -ForegroundColor Blue
    Write-Host ""
}

function Test-Dependencies {
    Write-ColoredOutput "Checking dependencies..." "Info"
    
    # Check PowerShell version
    if ($PSVersionTable.PSVersion.Major -lt 5) {
        Write-ColoredOutput "PowerShell 5.0+ is required" "Error"
        exit 1
    }
    
    # Check for Git
    try {
        $null = Get-Command git -ErrorAction Stop
        $gitVersion = git --version
        Write-ColoredOutput "Found Git: $gitVersion" "Info"
    }
    catch {
        Write-ColoredOutput "Git is not installed or not in PATH" "Error"
        Write-Host "Please install Git from: https://git-scm.com/download/win"
        exit 1
    }
    
    # Check for Rust/Cargo
    try {
        $null = Get-Command cargo -ErrorAction Stop
        $cargoVersion = cargo --version
        Write-ColoredOutput "Found Cargo: $cargoVersion" "Info"
    }
    catch {
        Write-ColoredOutput "Rust/Cargo is not installed" "Error"
        Write-Host "Please install Rust from: https://rustup.rs/"
        exit 1
    }
    
    # Check for MSVC compiler
    try {
        $null = Get-Command cl -ErrorAction Stop
        Write-ColoredOutput "Found MSVC compiler" "Info"
    }
    catch {
        Write-ColoredOutput "Visual Studio Build Tools not detected" "Warning"
        Write-Host "VeLang requires MSVC compiler for C code generation"
        Write-Host "Please install Visual Studio Build Tools from:"
        Write-Host "https://visualstudio.microsoft.com/visual-cpp-build-tools/"
        
        if (-not $Force) {
            $continue = Read-Host "Continue anyway? (y/N)"
            if ($continue -ne "y" -and $continue -ne "Y") {
                exit 1
            }
        }
    }
    
    Write-ColoredOutput "Dependencies check completed" "Success"
}

function Install-VeLang {
    Write-ColoredOutput "Creating temporary directory..." "Info"
    $tempDir = Join-Path $env:TEMP "velang_install_$(Get-Random)"
    New-Item -ItemType Directory -Path $tempDir -Force | Out-Null
    
    try {
        Write-ColoredOutput "Downloading VeLang source code..." "Info"
        Set-Location $tempDir
        
        git clone https://github.com/velang-org/ve.git *>$null
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to clone VeLang repository"
        }
        Write-ColoredOutput "Source code downloaded successfully" "Success"
        
        Set-Location "ve"
        
        Write-ColoredOutput "Building VeLang..." "Info"
        cargo build --release --quiet *>$null
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to build VeLang"
        }
        
        Write-ColoredOutput "VeLang built successfully" "Success"
        
        Write-ColoredOutput "Installing VeLang..." "Info"
        if (-not (Test-Path $InstallPath)) {
            New-Item -ItemType Directory -Path $InstallPath -Force | Out-Null
        }
        
        $binaryPath = Join-Path $InstallPath "ve.exe"
        Copy-Item "target\release\ve.exe" $binaryPath -Force
        
        # Copy standard library
        if (Test-Path "lib") {
            Write-ColoredOutput "Installing standard library..." "Info"
            $libPath = Join-Path $InstallPath "lib"
            Copy-Item "lib" $libPath -Recurse -Force
            Write-ColoredOutput "Standard library installed" "Success"
        } else {
            Write-ColoredOutput "Standard library directory not found - some imports may fail" "Warning"
        }
        
        Write-ColoredOutput "VeLang installed to $InstallPath" "Success"
        
        # Add to PATH
        Write-ColoredOutput "Adding VeLang to system PATH..." "Info"
        $currentPath = [Environment]::GetEnvironmentVariable("Path", "User")
        if ($currentPath -notlike "*$InstallPath*") {
            $newPath = $currentPath + ";$InstallPath"
            [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
            Write-ColoredOutput "Added $InstallPath to user PATH" "Success"
        } else {
            Write-ColoredOutput "$InstallPath already in PATH" "Info"
        }
        
        # Verify installation
        Write-ColoredOutput "Verifying installation..." "Info"
        try {
            $version = & $binaryPath --version 2>$null
            Write-ColoredOutput "VeLang is working: $version" "Success"
        }
        catch {
            Write-ColoredOutput "VeLang binary exists but may not be working correctly" "Warning"
        }
    }
    finally {
        # Cleanup
        Write-ColoredOutput "Cleaning up..." "Info"
        Set-Location $env:TEMP
        Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

function Show-CompletionMessage {
    Write-Host ""
    Write-ColoredOutput "VeLang installation completed!" "Success"
    Write-Host ""
    Write-ColoredOutput "To get started:" "Info"
    Write-Host "  ve --help                 # Show help"
    Write-Host "  ve init my_project        # Create a new project"
    Write-Host "  ve example.ve             # Compile and run a file"
    Write-Host ""
    Write-ColoredOutput "Join our Discord: https://dsc.gg/velang" "Info"
    Write-Host ""
    Write-ColoredOutput "Please restart your terminal to use the 've' command" "Warning"
}

# Main execution
try {
    Show-Banner
    Test-Dependencies
    Install-VeLang
    Show-CompletionMessage
}
catch {
    Write-ColoredOutput "Installation failed: $_" "Error"
    exit 1
}
