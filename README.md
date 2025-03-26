![Ve Language](https://img.shields.io/static/v1?label=&message=Velang&color=2b7489&logo=asciidoc&logoColor=white)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## ⚠️ Experimental Status
Velang is in **EARLY DEVELOPMENT** – syntax and features will change radically. Not suitable for any production use. Use at your own risk!

## Current Features
- Types:
  - Primitives: i32, bool, string, void
  - Arrays
  - Structures
  - Functions
  - Modules
- Control flow:
  - if/else conditions
  - while/for loops
  - Early function returns
- Operators:
  - Arithmetic: + - * / % **
  - Comparisons: == != > < >= <=
  - Logical: && || !
- Modularity:
  - Import other Verve files
  - Export functions for use in other files
- Development Tools:
  - Benchmarking capabilities

## Discord 
Join our [Discord](https://dsc.gg/velang) server to chat with the community and get help with the language.

## Installation
Requirements:
- Rust 1.70+
- Clang 17+
- Windows: Visual Studio Build Tools

Install:
```bash
git clone https://github.com/velang-org/ve
cd velang
cargo install --path .
```

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