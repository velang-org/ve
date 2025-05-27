#!/bin/bash

# Quick local installer for VeLang development
# This script allows developers to quickly install VeLang from the current directory

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Check for verbose mode
VERBOSE=${VERBOSE:-0}
if [ "$1" = "--verbose" ] || [ "$1" = "-v" ]; then
    VERBOSE=1
fi

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_error() {
    echo -e "\033[0;31m[ERROR]\033[0m $1"
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

print_banner() {
    echo -e "${BLUE}"
    echo "╭─────────────────────────────────────╮"
    echo "│        VeLang Local Installer       │"
    echo "│         Development Build           │"
    echo "╰─────────────────────────────────────╯"
    echo -e "${NC}"
}

main() {
    print_banner
    
    print_info "Installing VeLang from local development directory..."
    cd "$PROJECT_ROOT"
    
    print_info "Building VeLang..."
    if [ "$VERBOSE" = "1" ]; then
        print_info "Running in verbose mode - showing all output"
        if cargo build --release; then
            print_success "Build completed!"
        else
            print_error "Build failed!"
            safe_exit 1
        fi
    else
        if cargo build --release --quiet > /dev/null 2>&1; then
            print_success "Build completed!"
        else
            print_error "Build failed!"
            safe_exit 1
        fi
    fi
    
    print_info "Installing VeLang..."
    if [ "$VERBOSE" = "1" ]; then
        if cargo install --path . --force; then
            print_success "Installation completed!"
        else
            print_error "Installation failed!"
            safe_exit 1
        fi
    else
        if cargo install --path . --force --quiet > /dev/null 2>&1; then
            print_success "Installation completed!"
        else
            print_error "Installation failed!"
            safe_exit 1
        fi
    fi
    
    print_success "VeLang installed successfully!"
    
    # Test installation
    if command -v ve &> /dev/null; then
        VERSION=$(ve --version 2>/dev/null || echo "version check failed")
        print_success "VeLang is working: $VERSION"
    else
        print_info "VeLang binary installed, you may need to restart your shell"
    fi
}

main "$@"
