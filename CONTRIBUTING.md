# Contributing to Verve

We welcome contributions! As Verve is in **early experimental phase**, your input will directly shape the language's future.

## 🛑 Before You Start
1. **Check existing issues** for duplicates
2. **Discuss major changes** via [GitHub Discussions](https://github.com/verve-lang/verve/discussions) before coding
3. **Expect breaking changes** – current architecture is unstable

## 🛠️ Development Setup

### Requirements
- Rust 1.70+ ([install](https://www.rust-lang.org/tools/install))
- Clang 17+ ([docs](https://releases.llvm.org/))
- For Windows: Visual Studio Build Tools

### Quick Start
```bash
git clone https://github.com/verve-lang/verve
cd verve
cargo build
```

## 📜 Contribution Areas

### High Priority
- Compiler error messages
- Type system improvements
- C code generation optimizations

### Experimental
- Standard library concepts
- Memory management prototypes
- Syntax proposals

## 💻 Code Practices

### Rust Code
- Format with `cargo fmt`
- Lint with `cargo clippy`
- Document public APIs

### Verve Examples
```rust
// Use 4-space indentation
fn main() {
    let x = 10;
    if (x < 5) {
        print("Consistent style matters");
    }
}
```

## 🧪 Testing 

### Unit Tests
```bash
cargo test --lib  # Core functionality
```

### Integration Tests
```bash
cargo test --test integration  # End-to-end compilation
```

Add new test cases in:
- `tests/` for unit tests
- `tests/integration/` for end-to-end tests

## 📝 Pull Requests

### Checklist
- [ ] Tests pass (`cargo test --all`) 
- [ ] Documentation updated
- [ ] Examples added/updated
- [ ] No debug prints

### PR Template
```markdown
## Change Type
- [ ] Bug Fix
- [ ] Feature
- [ ] Documentation

## Description
< Concise summary >

## Related Issues
Closes #<issue-number>

## Verification Steps
1. How to test this?
2. Sample Verve code (if applicable)
```

## 📄 License
By contributing, you agree to license your work under the project's [MIT License](LICENSE).