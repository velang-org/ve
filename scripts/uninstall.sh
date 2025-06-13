#!/bin/bash

# Veil Uninstallation Script for Unix (Linux/macOS)

INSTALL_DIR="$HOME/.veil"
BIN_DIR="$HOME/.local/bin"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

print_banner() {
    echo -e "${BLUE}"
    echo "╭─────────────────────────────────────╮"
    echo "│         Veil Uninstaller            │"
    echo "╰─────────────────────────────────────╯"
    echo -e "${NC}"
}

main() {
    print_banner
    
    print_info "This will remove VeLang from your system"
    echo
    read -p "Are you sure you want to continue? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_info "Uninstallation cancelled"
        exit 0
    fi
    
    # Remove installation directory
    if [ -d "$INSTALL_DIR" ]; then
        print_info "Removing $INSTALL_DIR..."
        rm -rf "$INSTALL_DIR"
        print_success "Removed VeLang installation directory (including standard library)"
    else
        print_info "VeLang installation directory not found"
    fi
    
    # Remove symlink
    if [ -L "$BIN_DIR/ve" ]; then
        print_info "Removing symlink from $BIN_DIR..."
        rm "$BIN_DIR/ve"
        print_success "Removed VeLang symlink"
    elif [ -f "$BIN_DIR/ve" ]; then
        print_info "Removing binary from $BIN_DIR..."
        rm "$BIN_DIR/ve"
        print_success "Removed VeLang binary"
    else
        print_info "VeLang binary not found in $BIN_DIR"
    fi
    
    print_success "VeLang has been uninstalled"
    print_warning "Note: You may need to remove VeLang from your PATH manually"
    print_info "Check your shell configuration files (.bashrc, .zshrc, etc.)"
}

main "$@"
