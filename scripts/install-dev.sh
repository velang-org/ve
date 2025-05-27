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
    cargo build --release
    
    print_info "Installing VeLang..."
    cargo install --path . --force
    
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
