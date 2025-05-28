# VeLang Installation Scripts

This directory contains installation and setup scripts**Note**: The installation scripts will automatically handle the setup process.for VeLang programming language.

## Available Scripts

### Installation Scripts
- **`install.sh`** - Installation script for Unix systems (Linux/macOS)
- **`install.ps1`** - Advanced installation script for Windows (PowerShell)
- **`install-dev.sh`** - Development installation script

### Utility Scripts
- **`uninstall.sh`** - Uninstallation script for Unix systems

## Quick Installation

### Linux/macOS (Bash)
```bash
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.sh | bash
```

### Windows (PowerShell)
```powershell
iex (iwr -useb https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.ps1).Content
```

## Advanced Installation Options

### Debug Mode

If installation fails, you can run with verbose output to see detailed error messages:

#### Linux/macOS (Debug Mode)
```bash
# Download and run with debug output
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.sh | VERBOSE=1 bash
```

#### Windows PowerShell (Debug Mode)
```powershell
# Download and run with verbose output
$script = (iwr -useb https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.ps1).Content
Invoke-Expression "$script -Verbose"
```

## Prerequisites

Before installing VeLang, ensure you have the following dependencies:

### Required
- **Git** - For cloning the repository
- **Rust/Cargo** - For building VeLang from source
  - Install from [rustup.rs](https://rustup.rs/)

### Platform-specific Requirements

#### Windows
- **Visual Studio Build Tools** or **MSVC** - For C code generation
  - Download from [Visual Studio Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/)

#### Linux/macOS
- **C Compiler** - GCC or Clang for code generation
  - Usually pre-installed or available via package manager

## Local Installation

If you've cloned the repository locally:

### Linux/macOS
```bash
cd scripts
chmod +x install.sh
./install.sh
```

### Windows (PowerShell)
```bash
cd scripts
.\install.ps1
```

#### PowerShell Advanced Options
```powershell
# With verbose output for debugging
.\install.ps1 -Verbose
```

**Note:** The installation scripts will automatically detect development branches when run locally.

## Verification

After installation, verify that VeLang is working correctly:

```bash
# Check VeLang version
ve --version

# Show help
ve --help

# Create a new project
ve init my_project

# Compile and run a VeLang file
ve example.ve
```

## Troubleshooting

### Common Issues

#### "Command not found: ve"
- **Solution**: Restart your terminal/command prompt to reload PATH
- **Alternative**: Source your shell profile manually:
  ```bash
  # Linux/macOS
  source ~/.bashrc  # or ~/.zshrc
  ```

#### Build Errors on Windows
- **Issue**: Missing Visual Studio Build Tools
- **Solution**: Install [Visual Studio Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/)

#### Permission Errors
- **Linux/macOS**: Run with appropriate permissions
- **Windows**: Run PowerShell as Administrator if needed

### Getting Help

- **Discord**: [https://dsc.gg/velang](https://dsc.gg/velang)
- **GitHub Issues**: [https://github.com/velang-org/ve/issues](https://github.com/velang-org/ve/issues)
- **Documentation**: Check the main repository README

## Uninstallation

### Linux/macOS
```bash
# Run the uninstall script
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/scripts/uninstall.sh | bash

# Or manually remove
rm -rf ~/.velang
# Remove from PATH in your shell profile
```

### Windows
```powershell
# Manual removal
Remove-Item -Recurse -Force "$env:USERPROFILE\.velang"
# Remove from PATH via System Properties > Environment Variables
```

---