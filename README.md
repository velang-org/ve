![Verve Language](https://img.shields.io/static/v1?label=&message=Verve&color=2b7489&logo=asciidoc&logoColor=white)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## ⚠️ Experimental Status
Verve is in **EARLY DEVELOPMENT** – syntax and features will change radically. Not suitable for any production use. Use at your own risk!

## Current Features
- Compiles to human-readable C
- Basic type system:
  - Primitives: i32, bool, string, void
  - Type inference for variables
  - Scoped variables/functions
- Control flow:
  - if/else conditions
  - while/for loops
  - Early function returns
- Operators:
  - Arithmetic: + - * /
  - Comparisons: == != > <
  - Logical: && ||
- print() function for debugging
- Modularity:
  - Import other Verve files
  - Export functions for use in other files

## Installation
Requirements:
- Rust 1.70+
- Clang 17+
- Windows: Visual Studio Build Tools

Install:
```bash
git clone https://github.com/verve-lang/verve
cd verve
cargo install --path .
```
## Examples
- [Basics](./examples/basics/README.md)
## Contributing
Contributions are welcome! Please read the [contributing guidelines](CONTRIBUTING.md) first.