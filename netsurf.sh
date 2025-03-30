#!/bin/bash

set -e

# Define vars
TARGET=i686-elf
PREFIX="$HOME/opt/cross"
PATH="$PREFIX/bin:$PATH"
BUILD_DIR="$HOME/netsurf_build"
INSTALL_DIR="$BUILD_DIR/install"
JOBS=$(nproc)

echo "[+] Updating system and installing dependencies..."
sudo apt update
sudo apt install -y \
    build-essential bison flex libgmp3-dev libmpc-dev libmpfr-dev texinfo \
    git curl python3 python3-pip wget gperf cmake libtool automake pkg-config

# Install cross-compiler if not already present
if ! command -v $TARGET-gcc &> /dev/null; then
    echo "[+] Cross-compiler not found. Building i686-elf-gcc..."

    cd "$HOME"
    mkdir -p toolchain-src
    cd toolchain-src

    # Binutils
    wget https://ftp.gnu.org/gnu/binutils/binutils-2.40.tar.gz
    tar -xzf binutils-2.40.tar.gz
    mkdir binutils-build
    cd binutils-build
    ../binutils-2.40/configure --target=$TARGET --prefix=$PREFIX --with-sysroot --disable-nls --disable-werror
    make -j$JOBS
    make install
    cd ..

    # GCC
    wget https://ftp.gnu.org/gnu/gcc/gcc-13.2.0/gcc-13.2.0.tar.gz
    tar -xzf gcc-13.2.0.tar.gz
    cd gcc-13.2.0
    ./contrib/download_prerequisites
    cd ..
    mkdir gcc-build
    cd gcc-build
    ../gcc-13.2.0/configure --target=$TARGET --prefix=$PREFIX --disable-nls --enable-languages=c,c++ --without-headers
    make all-gcc -j$JOBS
    make all-target-libgcc -j$JOBS
    make install-gcc
    make install-target-libgcc
    echo "[+] Cross-compiler installed at $PREFIX"
fi

export PATH="$PREFIX/bin:$PATH"

echo "[+] Creating build directory..."
mkdir -p "$BUILD_DIR"
mkdir -p "$INSTALL_DIR"
cd "$BUILD_DIR"

# Clone required repos
echo "[+] Cloning NetSurf and dependencies..."
REPOS=(
    buildsystem
    netsurf
    libparserutils
    libwapcaplet
    libhubbub
    libcss
    libdom
    libnsfb
)

for repo in "${REPOS[@]}"; do
    git clone "https://git.netsurf-browser.org/${repo}.git"
done

# Export build flags
export CFLAGS="-ffreestanding -nostdlib -O2"
export LDFLAGS=""

# Build dependencies
build_lib() {
    LIBNAME=$1
    cd "$BUILD_DIR/$LIBNAME"
    echo "[+] Building $LIBNAME..."
    make PREFIX="$INSTALL_DIR" TARGET="$TARGET" install
}

build_lib libparserutils
build_lib libwapcaplet
build_lib libhubbub
build_lib libcss
build_lib libdom
build_lib libnsfb

# Build NetSurf framebuffer frontend
cd "$BUILD_DIR/netsurf"
echo "[+] Building NetSurf with framebuffer frontend..."
make TARGET=framebuffer \
    PREFIX="$INSTALL_DIR" \
    CC="$TARGET-gcc" \
    CFLAGS="$CFLAGS -I$INSTALL_DIR/include" \
    LDFLAGS="$LDFLAGS -L$INSTALL_DIR/lib" \
    install

echo ""
echo "=== NetSurf has been built for your custom environment! ==="
echo "Install location: $INSTALL_DIR"
echo "Next: Replace framebuffer rendering/input with your kernel's interfaces."