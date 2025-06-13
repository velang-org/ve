
<div align="center" style="display: grid; place-items: center; gap: 1rem;">
     <img src="https://cdn.discordapp.com/app-assets/1382254020038627338/1382254808395812995.png" alt="Veil Logo" width="200">
</div>



## Installation

### Quick Install (Recommended)

**Unix (Linux/macOS):**
```bash
curl -sSf https://raw.githubusercontent.com/veil-lang/veil/main/scripts/install.sh | bash
```

**Windows (PowerShell):**
```powershell
iex (iwr -useb https://raw.githubusercontent.com/veil-lang/veil/main/scripts/install.ps1).Content
```

### Manual Installation

Requirements:
- Rust 1.70+
- Clang 17+ (Unix) / Visual Studio Build Tools (Windows)
- Git

```bash
git clone https://github.com/veil-lang/veil
cd ve
cargo install --path .
```

For detailed installation instructions and troubleshooting, see [`scripts/README.md`](scripts/README.md).

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