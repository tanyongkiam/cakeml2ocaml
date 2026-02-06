#!/bin/bash
# build_hooked.sh — Build a hooked CakeML compiler with injected passes
#
# This script:
#   1. Compiles hook_ref.ml (mutable hooks, compiled before cake64)
#   2. Patches cake64.ml to call all 8 IL hooks in backend_compile
#   3. Compiles the patched cake64.ml (requires ulimit -s unlimited)
#   4. Compiles il_types, conv modules, user pass, hook setup, and entry point
#   5. Links everything into cake64_hooked
#
# Prerequisites: ocamlfind, zarith, unix, cakeml_runtime.cmx
#
# Usage:
#   ./build_hooked.sh [pass_files...] <hook_setup.ml> [output_binary]
#
# The last .ml argument before the optional output is treated as the hook setup.
# All other .ml arguments are user pass files to compile.
#
# Example:
#   ./build_hooked.sh test_word_pass.ml hook_setup.ml
#   ./build_hooked.sh my_pass.ml my_setup.ml my_compiler

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

OC="ocamlfind ocamlopt -package zarith,unix"
OFLAGS="-O2 -g"
IFLAGS="-I ../runtime -I il_types -I ."

# Parse arguments: all .ml files are sources, last non-.ml arg is output name
PASS_FILES=()
SETUP_FILE=""
OUTPUT="cake64_hooked"

for arg in "$@"; do
    if [[ "$arg" == *.ml ]]; then
        if [ -n "$SETUP_FILE" ]; then
            PASS_FILES+=("$SETUP_FILE")
        fi
        SETUP_FILE="$arg"
    else
        OUTPUT="$arg"
    fi
done

if [ -z "$SETUP_FILE" ]; then
    echo "Usage: $0 [pass_files...] <hook_setup.ml> [output_binary]"
    echo
    echo "Hook points (all active, unused hooks default to identity):"
    echo "  flat  — after source_to_flat  (flatLang_dec list)"
    echo "  clos  — after flat_to_clos    (closLang_exp list)"
    echo "  bvl   — after clos_to_bvl     ((name * (arity * bvl_exp)) list)"
    echo "  bvi   — after bvl_to_bvi      ((name * (arity * bvi_exp)) list)"
    echo "  data  — after bvi_to_data     ((name * (arity * dataLang_prog)) list)"
    echo "  word  — after data_to_word    ((name * (arity * wordLang_prog)) list)"
    echo "  stack — after word_to_stack   ((name * stackLang_prog) list)"
    echo "  lab   — after stack_to_lab    (labLang_sec list)"
    exit 1
fi

echo "=== Building hooked CakeML compiler ==="
echo "  Pass files:  ${PASS_FILES[*]:-none}"
echo "  Hook setup:  $SETUP_FILE"
echo "  Output:      $OUTPUT"
echo

# Step 1: Compile hook_ref (must come before cake64)
echo "[1/7] Compiling hook_ref.ml..."
$OC -c $OFLAGS hook_ref.ml

# Step 2: Build il_types
echo "[2/7] Building il_types..."
make -s -f Makefile il_types 2>&1

# Step 3: Patch and compile cake64.ml
echo "[3/7] Patching and compiling cake64.ml (this may take a while)..."
cp cake64.ml cake64.ml.bak

# Patch all 8 hooks using Python (exact string matching, no sed regex issues)
python3 patch_hooks.py cake64.ml

# Verify patches applied
HOOKS_FOUND=$(grep -o 'Hook_ref\.' cake64.ml | wc -l)
if [ "$HOOKS_FOUND" -eq 8 ]; then
    echo "  All 8 hooks verified: OK"
else
    echo "  ERROR: Expected 8 hooks, found $HOOKS_FOUND"
    mv cake64.ml.bak cake64.ml
    exit 1
fi

# Compile patched cake64.ml (needs unlimited stack)
bash -c "ulimit -s unlimited; $OC -c $OFLAGS -I ../runtime cake64.ml"

# Restore original cake64.ml
mv cake64.ml.bak cake64.ml
echo "  Restored original cake64.ml"

# Step 4: Compile conv modules
echo "[4/7] Building conv modules..."
make -s -f Makefile conv 2>&1

# Step 5: Compile user pass files
if [ ${#PASS_FILES[@]} -gt 0 ]; then
    echo "[5/7] Compiling user pass files..."
    for f in "${PASS_FILES[@]}"; do
        echo "  $f"
        $OC -c $OFLAGS $IFLAGS "$f"
    done
else
    echo "[5/7] No user pass files (skipped)"
fi

# Step 6: Compile hook setup and main entry
echo "[6/7] Compiling hook_setup and main_entry..."
$OC -c $OFLAGS $IFLAGS "$SETUP_FILE"
$OC -c $OFLAGS $IFLAGS main_entry.ml

# Step 7: Link everything
echo "[7/7] Linking $OUTPUT..."

# Build list of pass .cmx files
PASS_CMXS=""
for f in "${PASS_FILES[@]}"; do
    PASS_CMXS="$PASS_CMXS $(basename "${f%.ml}").cmx"
done

SETUP_CMX="$(basename "${SETUP_FILE%.ml}").cmx"

bash -c "ulimit -s unlimited; $OC -linkpkg $OFLAGS $IFLAGS \
    hook_ref.cmx \
    ../runtime/cakeml_runtime.cmx \
    il_types/common.cmx il_types/flat_lang.cmx il_types/clos_lang.cmx \
    il_types/bvl_lang.cmx il_types/bvi_lang.cmx il_types/data_lang.cmx \
    il_types/word_lang.cmx il_types/stack_lang.cmx il_types/lab_lang.cmx \
    cake64.cmx \
    il_types/common_conv.cmx il_types/clos_lang_conv.cmx \
    il_types/flat_lang_conv.cmx il_types/bvl_lang_conv.cmx \
    il_types/bvi_lang_conv.cmx il_types/data_lang_conv.cmx \
    il_types/word_lang_conv.cmx il_types/stack_lang_conv.cmx \
    il_types/lab_lang_conv.cmx \
    $PASS_CMXS \
    $SETUP_CMX \
    main_entry.cmx \
    -o $OUTPUT"

echo
echo "Done! Binary: $OUTPUT"
echo "Test with: echo 'val x = 1;' | ulimit -s unlimited && ./$OUTPUT"
