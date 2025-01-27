# Verve Language

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Verve is an experimental systems programming language compiling to C, focusing on simplicity and interoperability.

## Current Features

- **C compilation backend** with automatic code generation
- **Basic type system** supporting:
  - Primitives: `i32`, `bool`, `string`, `void`
  - Pointers (experimental)
- **Function definitions** with parameters and returns
- **Variables** with type inference
- **Print statements** with format specifiers
- **Arithmetic operations**: `+`, `-`, `*`, `/`
- **Comparison operators**: `==`, `>`
- **Control flow**: Basic function calls, `while` loops, `for` loops

## Installation

### Requirements
- Rust 1.70+
- Clang 17+
- Windows: Visual Studio Build Tools (for C compilation)

```bash
cargo install --git https://github.com/verve-lang/verve
```
