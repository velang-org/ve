# VeLang Makefile

.PHONY: help install build test clean release install-deps

# Default target
help:
	@echo "VeLang Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  install      - Install VeLang using cargo"
	@echo "  build        - Build VeLang in debug mode"
	@echo "  release      - Build VeLang in release mode"
	@echo "  test         - Run all tests"
	@echo "  clean        - Clean build artifacts"
	@echo "  install-deps - Install system dependencies (Unix only)"
	@echo "  help         - Show this help message"

# Install VeLang
install:
	cargo install --path .

# Build in debug mode
build:
	cargo build

# Build in release mode
release:
	cargo build --release

# Run tests
test:
	cargo test

# Clean build artifacts
clean:
	cargo clean

# Install system dependencies (Unix only)
install-deps:
	@echo "Installing system dependencies..."
	@if command -v apt-get >/dev/null 2>&1; then \
		echo "Detected Debian/Ubuntu"; \
		sudo apt-get update && sudo apt-get install -y clang git; \
	elif command -v yum >/dev/null 2>&1; then \
		echo "Detected RHEL/CentOS"; \
		sudo yum install -y clang git; \
	elif command -v dnf >/dev/null 2>&1; then \
		echo "Detected Fedora"; \
		sudo dnf install -y clang git; \
	elif command -v pacman >/dev/null 2>&1; then \
		echo "Detected Arch Linux"; \
		sudo pacman -S --noconfirm clang git; \
	elif command -v brew >/dev/null 2>&1; then \
		echo "Detected macOS with Homebrew"; \
		brew install llvm git; \
	else \
		echo "Package manager not detected. Please install clang and git manually."; \
	fi
