# VeLang Installation Script for Windows (PowerShell)
# Requires PowerShell 5.0+ and Git

param(
    [switch]$Force,
    [switch]$Verbose,
    [string]$InstallPath = "$env:USERPROFILE\.velang",
    [string]$Branch = "main"
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
        throw "PowerShell 5.0+ is required"
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
        throw "Git is not installed"
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
        throw "Rust/Cargo is not installed"
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
                throw "MSVC compiler required"
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
        Write-ColoredOutput "Working directory: $tempDir" "Info"
        Set-Location $tempDir
        
        # Determine which branch to use - priority order:
        # 1. Explicitly provided -Branch parameter
        # 2. VELANG_BRANCH environment variable  
        # 3. Auto-detection from script URL
        # 4. Default to main
        $targetBranch = $Branch
        
        if ($env:VELANG_BRANCH -and $Branch -eq "main") {
            $targetBranch = $env:VELANG_BRANCH
            Write-ColoredOutput "Using branch from environment: $targetBranch" "Info"
        } elseif ($Branch -ne "main") {
            Write-ColoredOutput "Using specified branch: $targetBranch" "Info"
        } else {
            try {
                $callingScript = (Get-PSCallStack)[1].Command
                if ($callingScript -like "*feature/installer*") {
                    $targetBranch = "feature/installer"
                    Write-ColoredOutput "Auto-detected development branch: $targetBranch" "Info"
                }
            } catch {
                # Use main as fallback
            }
        }
        
        Write-ColoredOutput "Target branch: $targetBranch" "Info"
        
        # Check internet connectivity and repository access
        Write-ColoredOutput "Verifying repository access..." "Info"
        try {
            $testOutput = & git ls-remote --heads https://github.com/velang-org/ve.git $targetBranch 2>&1
            if ($LASTEXITCODE -ne 0 -or -not $testOutput) {
                Write-ColoredOutput "Branch '$targetBranch' may not exist. Available branches:" "Warning"
                $allBranches = & git ls-remote --heads https://github.com/velang-org/ve.git 2>&1
                if ($LASTEXITCODE -eq 0) {
                    $allBranches | ForEach-Object {
                        if ($_ -match "refs/heads/(.+)$") {
                            Write-Host "  - $($matches[1])"
                        }
                    }
                }
                throw "Branch '$targetBranch' not found"
            }
            Write-ColoredOutput "Repository access verified" "Success"
        } catch {
            Write-ColoredOutput "Could not verify repository access" "Warning"
            Write-ColoredOutput "Proceeding with clone attempt..." "Info"
        }
        
        # First attempt with error suppression
        if ($Verbose) {
            Write-ColoredOutput "Running in verbose mode - showing all output" "Info"
            git clone -b $targetBranch https://github.com/velang-org/ve.git
        } else {
            $cloneOutput = & git clone -b $targetBranch https://github.com/velang-org/ve.git 2>&1
        }
        
        if ($LASTEXITCODE -ne 0) {
            if (-not $Verbose) {
                # Show the actual error and try again
                Write-ColoredOutput "Clone failed on first attempt" "Warning"
                Write-ColoredOutput "Git error output: $cloneOutput" "Warning"
                Write-ColoredOutput "Retrying clone with full output..." "Info"
                
                # Second attempt with full output visible
                git clone -b $targetBranch https://github.com/velang-org/ve.git
            }
            
            if ($LASTEXITCODE -ne 0) {
                Write-ColoredOutput "Failed to clone VeLang repository from branch '$targetBranch'" "Error"
                Write-ColoredOutput "This could be due to:" "Info"
                Write-Host "  - Internet connectivity issues"
                Write-Host "  - Branch '$targetBranch' does not exist"
                Write-Host "  - GitHub access restrictions"
                Write-Host "  - Git configuration issues"
                throw "Repository clone failed"
            }
        }
        
        # Verify that the directory was created
        if (-not (Test-Path "ve")) {
            throw "Repository clone completed but 've' directory not found"
        }
        
        Write-ColoredOutput "Source code downloaded successfully" "Success"
        
        Set-Location "ve"
        
        Write-ColoredOutput "Building VeLang..." "Info"
        $buildOutput = & cargo build --release --quiet 2>&1
        if ($LASTEXITCODE -ne 0) {
            Write-ColoredOutput "Build failed. Output:" "Error"
            Write-Host $buildOutput
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
            # Temporarily add to current session PATH for verification
            $env:PATH = "$InstallPath;$env:PATH"
            $versionOutput = & $binaryPath --version 2>&1
            if ($LASTEXITCODE -eq 0) {
                Write-ColoredOutput "VeLang is working: $versionOutput" "Success"
            } else {
                Write-ColoredOutput "VeLang binary created but version check failed: $versionOutput" "Warning"
            }
        }
        catch {
            Write-ColoredOutput "VeLang binary exists but may not be working correctly: $_" "Warning"
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
    Write-Host ""
    Write-ColoredOutput "If you continue to have issues, please visit:" "Info"
    Write-Host "  GitHub Issues: https://github.com/velang-org/ve/issues"
    Write-Host "  Discord: https://dsc.gg/velang"
    Write-Host ""
    
    # Check if we're being run via Invoke-Expression
    $callingScript = (Get-PSCallStack)[1].Command
    if ($callingScript -eq "<ScriptBlock>" -or $callingScript -like "*Invoke-Expression*") {
        # If run via iex, pause instead of exiting
        Write-Host "Press any key to continue..." -ForegroundColor Yellow
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    } else {
        exit 1
    }
}
