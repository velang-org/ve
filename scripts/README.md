# VeLang Installation Scripts

This directory contains installation and setup scripts for VeLang.

## Files

### Installation Scripts
- **`install.sh`** - Installation script for Unix systems (Linux/macOS)
- **`install.bat`** - Installation script for Windows (Command Prompt)
- **`install.ps1`** - Advanced installation script for Windows (PowerShell)

### Utility Scripts
- **`uninstall.sh`** - Uninstallation script for Unix systems

### Documentation
- **`INSTALL.md`** - Comprehensive installation guide

## Quick Start

### Unix (Linux/macOS)
```bash
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.sh | bash
```

### Windows (PowerShell)
```powershell
iex (iwr -useb https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.ps1).Content
```

### Windows (Command Prompt)
```cmd
curl -o install.bat https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.bat && install.bat
```

## Local Installation

If you've cloned the repository:

```bash
# Unix/macOS
cd scripts
chmod +x install.sh
./install.sh

# Windows (PowerShell)
cd scripts
.\install.ps1

# Windows (Command Prompt)
cd scripts
install.bat
```

## Features

All installation scripts:
- ✅ Automatically check system dependencies
- ✅ Download and build VeLang from source
- ✅ Install binaries to appropriate locations
- ✅ Add VeLang to system PATH
- ✅ Verify installation success
- ✅ Provide helpful error messages and troubleshooting

For detailed installation instructions, see [INSTALL.md](INSTALL.md).
