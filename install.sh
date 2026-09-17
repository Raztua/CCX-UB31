#!/bin/bash
# ==============================================================================
#  CalculiX CCX 2.23 (UB31 Extension) — Build & Installation Script
#  Note: Building is identical to a standard CalculiX CCX 2.23 distribution.
# ==============================================================================
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="${SCRIPT_DIR}/src"

echo "======================================================================"
echo "  CalculiX CCX 2.23 with UB31 Extension — Build Script"
echo "  Building is identical to standard CalculiX CCX 2.23."
echo "======================================================================"

if [ ! -d "$SRC_DIR" ]; then
    echo "ERROR: Source directory '$SRC_DIR' not found."
    exit 1
fi

cd "$SRC_DIR"

# Determine parallel jobs
NPROC=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

echo "--> Compiling in ${SRC_DIR} using make -j${NPROC}..."
make -j"${NPROC}"

if [ -f "${SRC_DIR}/ccx_2.23" ]; then
    echo ""
    echo "======================================================================"
    echo " SUCCESS: Built ccx_2.23 binary at: ${SRC_DIR}/ccx_2.23"
    echo "======================================================================"
else
    echo "ERROR: Compilation completed but '${SRC_DIR}/ccx_2.23' was not found."
    exit 1
fi
