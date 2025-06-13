#!/bin/bash
# VeLang Installation Script for Unix (Linux/macOS)
# Requires Git and Rust/Cargo

# Configuration
INSTALL_DIR="$HOME/.veil"
TEMP_DIR=$(mktemp -d)
BRANCH="${VEIL_BRANCH:-main}"

# Text formatting
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Function to display formatted messages
function print_msg() {
    case "$1" in
        "info") echo -e "${CYAN}[INFO]${NC} $2" ;;
        "success") echo -e "${GREEN}[SUCCESS]${NC} $2" ;;
        "warning") echo -e "${YELLOW}[WARNING]${NC} $2" ;;
        "error") echo -e "${RED}[ERROR]${NC} $2" ;;
    esac
}

# Function to check dependencies
function check_dependency() {
    if ! command -v "$1" &> /dev/null; then
        print_msg "error" "$1 is not installed"
        echo "Please install $1 and try again"
        exit 1
    fi
}

# Check dependencies
print_msg "info" "Checking dependencies..."
check_dependency git
check_dependency cargo

# Check for C compiler
if ! command -v gcc &> /dev/null && ! command -v clang &> /dev/null; then
    print_msg "warning" "C compiler (gcc/clang) not detected"
    echo "VeLang requires a C compiler for code generation"
    read -rp "Continue anyway? (y/N) " continue
    if [[ ! "$continue" =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Clone repository
print_msg "info" "Cloning repository (branch: $BRANCH)..."
if ! git clone -b "$BRANCH" --quiet https://github.com/veil-lang/veil.git "$TEMP_DIR" 2>/dev/null; then
    print_msg "error" "Failed to clone repository from branch '$BRANCH'"
    exit 1
fi

# Build VeLang
print_msg "info" "Building Veil..."
(
    cd "$TEMP_DIR" || exit 1
    if ! cargo build --release --quiet 2>/dev/null; then
        print_msg "error" "Build failed"
        exit 1
    fi
)

# Install files
print_msg "info" "Installing to $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR"
cp "$TEMP_DIR/target/release/ve" "$INSTALL_DIR/ve"

# Copy standard library
if [[ -d "$TEMP_DIR/lib" ]]; then
    cp -r "$TEMP_DIR/lib" "$INSTALL_DIR"
else
    print_msg "warning" "Standard library not found - some imports may fail"
fi

# Add to PATH
print_msg "info" "Updating PATH..."
shell_profile=""
case "$SHELL" in
    */zsh) shell_profile="$HOME/.zshrc" ;;
    */bash) shell_profile="$HOME/.bashrc" ;;
    */fish) shell_profile="$HOME/.config/fish/config.fish" ;;
    *) shell_profile="$HOME/.profile" ;;
esac

if ! grep -q "$INSTALL_DIR" "$shell_profile" 2>/dev/null; then
    echo "export PATH=\"\$PATH:$INSTALL_DIR\"" >> "$shell_profile"
    print_msg "success" "Added to $shell_profile"
else
    print_msg "info" "PATH already configured in $shell_profile"
fi

# Cleanup
print_msg "info" "Cleaning up..."
rm -rf "$TEMP_DIR"

# Verify installation
print_msg "info" "Verifying installation..."
if "$INSTALL_DIR/ve" --version &>/dev/null; then
    print_msg "success" "Veil installed successfully!"
else
    print_msg "warning" "Installation completed but verification failed"
fi

# Final instructions
echo -e "\nTo get started:"
echo "  source $shell_profile  # Reload your shell configuration"
echo "  ve --help              # Show help"
echo "  ve init my_project     # Create new project"
echo "  ve example.ve          # Compile and run a file"