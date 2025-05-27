#!/bin/bash

# Quick local installer for VeLang development
# This script allows developers to quickly install VeLang from the current directory

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

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
    if cargo build --release --quiet > /dev/null 2>&1; then
        print_success "Build completed!"
    else
        print_error "Build failed!"
        exit 1
    fi
    
    print_info "Installing VeLang..."
    if cargo install --path . --force --quiet > /dev/null 2>&1; then
        print_success "Installation completed!"
    else
        print_error "Installation failed!"
        exit 1
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
