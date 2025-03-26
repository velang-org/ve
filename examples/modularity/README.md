# Modularity in Verve Language

This directory contains examples demonstrating how to organize code into modules in the Verve language.

## Examples Structure

### 1. Math Module
**Directory**: [01_math/](./01_math/)

Demonstrates a simple mathematical module with functions for adding and multiplying numbers.

Files:
- [math.ve](./01_math/math.ve) - Module containing mathematical functions
- [main.ve](./01_math/main.ve) - Main program that uses the math module

To run this example:
```bash
cargo run --bin Ve examples/modularity/01_math/main.ve
```

## Key Features
- **Module imports** - Using the `import` directive to access code from other files
- **Function exports** - Making functions available for use in other modules using the `export` keyword
- **Module organization** - Structuring code into logical modules to improve maintainability
