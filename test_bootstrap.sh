#!/bin/bash
# Bootstrap test: verify that the OCaml-transpiled CakeML compiler can
# compile itself from its own s-expression AST.
#
# Steps:
#   1. Ensure cake64 (OCaml-based CakeML compiler) is built
#   2. Feed the s-expression through cake64 --sexp=true to produce assembly (gen1)
#   3. Assemble with gcc + basis_ffi.c to get a native binary (cake64_native)
#   4. Feed the same s-expression through cake64_native to produce assembly (gen2)
#   5. Compare: the two assembly outputs should be identical
#
# Usage: ./test_bootstrap.sh [path-to-sexpr]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SEXPR="${1:-$SCRIPT_DIR/../cake-sexpr-64}"
WORKDIR=$(mktemp -d)
CAKE64="$SCRIPT_DIR/generated/cake64"

# CakeML native runtime needs generous heap/stack for self-compilation
export CML_HEAP_SIZE="${CML_HEAP_SIZE:-4096}"
export CML_STACK_SIZE="${CML_STACK_SIZE:-4096}"

CAKE_FLAGS="--sexp=true --skip_type_inference=true"

cleanup() { rm -rf "$WORKDIR"; }
trap cleanup EXIT

echo "=== Bootstrap test ==="
echo "sexpr:   $SEXPR"
echo "workdir: $WORKDIR"
echo

# Step 1: Ensure cake64 (OCaml-transpiled compiler) exists
if [ ! -x "$CAKE64" ]; then
    echo "Error: $CAKE64 not found. Run 'make transpile' and compile first."
    exit 1
fi

# Step 2: Compile the sexpr using the OCaml-transpiled compiler
# (needs unlimited stack for OCaml's deeply recursive generated code)
echo "[1/4] Compiling sexpr with OCaml-transpiled cake64..."
bash -c "ulimit -s unlimited; \"$CAKE64\" $CAKE_FLAGS < \"$SEXPR\" > \"$WORKDIR/cake_gen1.S\" 2>\"$WORKDIR/gen1.log\""
echo "       Generated $(wc -c < "$WORKDIR/cake_gen1.S") bytes of assembly"

# Step 3: Assemble into a native binary
echo "[2/4] Assembling native binary (cake64_native)..."
gcc -o "$WORKDIR/cake64_native" "$WORKDIR/cake_gen1.S" "$SCRIPT_DIR/basis_ffi.c" -lm
echo "       Binary: $(stat -c%s "$WORKDIR/cake64_native") bytes"

# Step 4: Use the native binary to compile the sexpr again
echo "[3/4] Compiling sexpr with native cake64_native..."
"$WORKDIR/cake64_native" $CAKE_FLAGS < "$SEXPR" > "$WORKDIR/cake_gen2.S" 2>"$WORKDIR/gen2.log"
echo "       Generated $(wc -c < "$WORKDIR/cake_gen2.S") bytes of assembly"

# Step 5: Compare
echo "[4/4] Comparing assembly outputs..."
if diff -q "$WORKDIR/cake_gen1.S" "$WORKDIR/cake_gen2.S" > /dev/null 2>&1; then
    echo
    echo "SUCCESS: Bootstrap test passed -- both compilers produce identical assembly."
else
    echo
    echo "FAILURE: Assembly outputs differ."
    diff "$WORKDIR/cake_gen1.S" "$WORKDIR/cake_gen2.S" | head -20
    exit 1
fi
