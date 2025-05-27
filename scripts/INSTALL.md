# VeLang Installation Guide

This guide provides multiple ways to install VeLang on different operating systems.

## Quick Installation

### Unix (Linux/macOS)

The easiest way to install VeLang on Unix systems:

```bash
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/install.sh | bash
```

Or download and run manually:
```bash
wget https://raw.githubusercontent.com/velang-org/ve/main/install.sh
chmod +x install.sh
./install.sh
```

### Windows

**PowerShell (Recommended):**
```powershell
iex (iwr -useb https://raw.githubusercontent.com/velang-org/ve/main/install.ps1).Content
```

**Command Prompt:**
```cmd
curl -o install.bat https://raw.githubusercontent.com/velang-org/ve/main/install.bat && install.bat
```

## Manual Installation

### Prerequisites

**All Platforms:**
- Rust 1.70 or later
- Git
- Internet connection

**Unix-specific:**
- Clang 17+ (for C code generation)
- Build essentials (gcc, make, etc.)

**Windows-specific:**
- Visual Studio Build Tools 2019 or later
- MSVC compiler

### Installing Prerequisites

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install clang git build-essential
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

**macOS:**
```bash
# Install Xcode Command Line Tools
xcode-select --install

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

**Windows:**
1. Install Visual Studio Build Tools from https://visualstudio.microsoft.com/visual-cpp-build-tools/
2. Install Rust from https://rustup.rs/
3. Install Git from https://git-scm.com/download/win

### Building from Source

```bash
# Clone the repository
git clone https://github.com/velang-org/ve.git
cd ve

# Build and install
cargo install --path .

# Or use make (Unix only)
make install
```

## Verification

After installation, verify that VeLang is working:

```bash
ve --version
ve --help
```

Create a simple test file:
```bash
echo 'fn main() { print("Hello, VeLang!") }' > hello.ve
ve hello.ve
```

Test standard library imports:
```bash
echo 'import "std/io"; fn main() { io::println("Hello from std!") }' > test_std.ve
ve test_std.ve
```

## Troubleshooting

### Common Issues

**"ve: command not found"**
- Make sure the installation directory is in your PATH
- Restart your terminal/shell
- Check that the binary was installed correctly

**Build failures on Windows**
- Ensure Visual Studio Build Tools are installed
- Run the command from a "Developer Command Prompt"
- Check that MSVC compiler is available

**Clang not found (Unix)**
- Install clang: `sudo apt install clang` (Ubuntu/Debian)
- Or: `brew install llvm` (macOS)

**Permission denied**
- Make sure you have write permissions to the installation directory
- Try running with appropriate privileges

### Getting Help

- Join our Discord: https://dsc.gg/velang
- Open an issue: https://github.com/velang-org/ve/issues
- Check the documentation: https://github.com/velang-org/ve/wiki

## Uninstallation

To remove VeLang:

**If installed via script:**
```bash
# Unix
rm -rf ~/.velang
# Remove from PATH in your shell config file

# Windows
rmdir /s %USERPROFILE%\.velang
# Remove from PATH via System Properties
```

**If installed via cargo:**
```bash
cargo uninstall ve
```
