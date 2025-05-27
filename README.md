![Ve Language](https://img.shields.io/static/v1?label=&message=Velang&color=2b7489&logo=asciidoc&logoColor=white)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## ⚠️ Experimental Status
Velang is in **EARLY DEVELOPMENT** – syntax and features will change radically. Not suitable for any production use. Use at your own risk!

## Discord 
Join our [Discord](https://dsc.gg/velang) server to chat with the community and get help with the language.

## Installation

### Quick Install (Recommended)

**Unix (Linux/macOS):**
```bash
curl -sSf https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.sh | bash
```

**Windows (PowerShell):**
```powershell
iex (iwr -useb https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.ps1).Content
```

**Windows (Command Prompt):**
```cmd
curl -o install.bat https://raw.githubusercontent.com/velang-org/ve/main/scripts/install.bat && install.bat
```

### Manual Installation

Requirements:
- Rust 1.70+
- Clang 17+ (Unix) / Visual Studio Build Tools (Windows)
- Git

```bash
git clone https://github.com/velang-org/ve
cd ve
cargo install --path .
```

For detailed installation instructions and troubleshooting, see [`scripts/INSTALL.md`](scripts/INSTALL.md).

## Usage
```bash
# Compile and run a file
ve example.ve

# Initialize a new project
ve init my_project

# Run a project (from the current directory)
ve run

# Run benchmarks - two approaches
ve benchmark example.ve --iterations 5 --verbose
# or
ve example.ve --iterations 10 --verbose
```

## Examples
- [Basics](./examples/basics/README.md)
- [Modularity](./examples/modularity/README.md)

## Contributing
Contributions are welcome! Please read the [contributing guidelines](CONTRIBUTING.md) first.