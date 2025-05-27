#!/bin/bash

# VeLang Installation Script for Unix (Linux/macOS)
set -e

VELANG_VERSION="0.1.0"
INSTALL_DIR="$HOME/.velang"
BIN_DIR="$HOME/.local/bin"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_banner() {
    echo -e "${BLUE}"
    echo "╭─────────────────────────────────────╮"
    echo "│           VeLang Installer          │"
    echo "│          Version ${VELANG_VERSION}              │"
    echo "╰─────────────────────────────────────╯"
    echo -e "${NC}"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to handle script exit when run via curl/wget
safe_exit() {
    local exit_code=${1:-1}
    if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
        # Script is being run directly
        exit $exit_code
    else
        # Script is being sourced (likely via curl | bash)
        return $exit_code
    fi
}

check_dependencies() {
    print_info "Checking dependencies..."
    
    # Check for Rust
    if ! command -v rustc &> /dev/null; then
        print_error "Rust is not installed. Please install Rust first:"
        echo "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        safe_exit 1
    fi
    
    # Check for Cargo
    if ! command -v cargo &> /dev/null; then
        print_error "Cargo is not installed. Please install Rust with Cargo."
        safe_exit 1
    fi
    
    # Check for Clang
    if ! command -v clang &> /dev/null; then
        print_warning "Clang is not installed. VeLang requires Clang 17+ for compilation."
        echo "Please install clang:"
        echo "  Ubuntu/Debian: sudo apt-get install clang"
        echo "  macOS: xcode-select --install"
        echo "  Arch Linux: sudo pacman -S clang"
        read -p "Continue anyway? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            safe_exit 1
        fi
    fi
    
    # Check Rust version
    RUST_VERSION=$(rustc --version | cut -d' ' -f2)
    print_info "Found Rust version: $RUST_VERSION"
    
    # Check Clang version if available
    if command -v clang &> /dev/null; then
        CLANG_VERSION=$(clang --version | head -n1 | grep -o '[0-9]\+\.[0-9]\+' | head -n1)
        print_info "Found Clang version: $CLANG_VERSION"
    fi
    
    print_success "Dependencies check completed"
}

download_and_build() {
    print_info "Downloading VeLang source code..."
    
    # Create temporary directory
    TEMP_DIR=$(mktemp -d)
    cd "$TEMP_DIR"
    
    # Determine which branch to use - priority order:
    # 1. VELANG_BRANCH environment variable
    # 2. Auto-detection from script URL  
    # 3. Default to main
    BRANCH="main"
    
    if [ -n "$VELANG_BRANCH" ]; then
        BRANCH="$VELANG_BRANCH"
        print_info "Using branch from environment: $BRANCH"
    elif [[ "${BASH_SOURCE[0]}" == *"feature/installer"* ]] || [[ "$0" == *"feature/installer"* ]]; then
        BRANCH="feature/installer"
        print_info "Auto-detected development branch: $BRANCH"
    fi
    
    # Clone the repository
    if git clone -b "$BRANCH" https://github.com/velang-org/ve.git > /dev/null 2>&1; then
        print_success "Source code downloaded successfully"
    else
        print_error "Failed to clone VeLang repository from branch '$BRANCH'"
        safe_exit 1
    fi
    
    cd ve
    
    print_info "Building VeLang..."
    if cargo build --release --quiet > /dev/null 2>&1; then
        print_success "VeLang built successfully"
    else
        print_error "Failed to build VeLang"
        safe_exit 1
    fi
    
    # Create installation directory
    mkdir -p "$INSTALL_DIR"
    mkdir -p "$BIN_DIR"
    
    # Copy binary
    cp target/release/ve "$INSTALL_DIR/ve"
    
    # Copy standard library
    if [ -d "lib" ]; then
        print_info "Installing standard library..."
        cp -r lib "$INSTALL_DIR/"
        print_success "Standard library installed"
    else
        print_warning "Standard library directory not found - some imports may fail"
    fi
    
    # Create symlink in bin directory
    ln -sf "$INSTALL_DIR/ve" "$BIN_DIR/ve"
    
    print_success "VeLang installed to $INSTALL_DIR"
    
    # Cleanup
    cd /
    rm -rf "$TEMP_DIR"
}

update_path() {
    # Check if ~/.local/bin is in PATH
    if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
        print_warning "$BIN_DIR is not in your PATH"
        
        # Detect shell and update appropriate config file
        SHELL_NAME=$(basename "$SHELL")
        case "$SHELL_NAME" in
            bash)
                CONFIG_FILE="$HOME/.bashrc"
                ;;
            zsh)
                CONFIG_FILE="$HOME/.zshrc"
                ;;
            fish)
                CONFIG_FILE="$HOME/.config/fish/config.fish"
                ;;
            *)
                CONFIG_FILE="$HOME/.profile"
                ;;
        esac
        
        echo "export PATH=\"$BIN_DIR:\$PATH\"" >> "$CONFIG_FILE"
        print_info "Added $BIN_DIR to PATH in $CONFIG_FILE"
        print_warning "Please restart your shell or run: source $CONFIG_FILE"
    fi
}

verify_installation() {
    print_info "Verifying installation..."
    
    if [ -f "$INSTALL_DIR/ve" ]; then
        print_success "VeLang binary found at $INSTALL_DIR/ve"
        
        # Test the binary
        if "$INSTALL_DIR/ve" --version &> /dev/null; then
            VERSION_OUTPUT=$("$INSTALL_DIR/ve" --version)
            print_success "VeLang is working: $VERSION_OUTPUT"
        else
            print_warning "VeLang binary exists but may not be working correctly"
        fi
    else
        print_error "VeLang binary not found"
        safe_exit 1
    fi
}

main() {
    print_banner
    
    check_dependencies
    download_and_build
    update_path
    verify_installation
    
    echo
    print_success "VeLang installation completed!"
    echo
    print_info "To get started:"
    echo "  ve --help                 # Show help"
    echo "  ve init my_project        # Create a new project"
    echo "  ve example.ve             # Compile and run a file"
    echo
    print_info "Join our Discord: https://dsc.gg/velang"
}

# Check if running with bash
if [ -z "$BASH_VERSION" ]; then
    print_error "This script requires Bash to run"
    safe_exit 1
fi

# Run main function
main "$@"
