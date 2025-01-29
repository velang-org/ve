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
- print() function for debugging

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
## Example Usage
```rust
// examples/basic/control_flow.vrv
fn main() {
  let number = 7;
  if (number > 5) {
    print("Number is greater than 5");
  } else {
    print("Number is less than or equal to 5");
  }
  for (let i = 0; i < 5; i = i + 1) {
    print(i);
  }
  while (number > 0) {
    print(number);
    number = number - 1;
  }
}
```
## Contributing
Contributions are welcome! Please read the [contributing guidelines](CONTRIBUTING.md) first.