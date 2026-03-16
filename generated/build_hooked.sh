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

# Will be extended with pass file directories
PASS_IFLAGS=""

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

# Save original cake64.cmi so conv modules stay compatible
if [ -f cake64.cmi ]; then
    cp cake64.cmi cake64.cmi.orig
fi

# Patch all 8 hooks using Python (exact string matching, no sed regex issues)
python3 patch_hooks.py cake64.ml

# Verify patches applied
HOOKS_FOUND=$(grep -o 'Hook_ref\.' cake64.ml | wc -l)
if [ "$HOOKS_FOUND" -eq 8 ]; then
    echo "  All 8 hooks verified: OK"
else
    echo "  ERROR: Expected 8 hooks, found $HOOKS_FOUND"
    mv cake64.ml.bak cake64.ml
    if [ -f cake64.cmi.orig ]; then mv cake64.cmi.orig cake64.cmi; fi
    exit 1
fi

# Compile patched cake64.ml (needs unlimited stack)
bash -c "ulimit -s unlimited; $OC -c $OFLAGS -I ../runtime cake64.ml"

# Restore original cake64.ml
mv cake64.ml.bak cake64.ml
echo "  Restored original cake64.ml"

# Step 4: Build conv modules
echo "[4/7] Building conv modules..."
# Clean up stale .cmi.orig
if [ -f cake64.cmi.orig ]; then rm cake64.cmi.orig; fi
# Rebuild only the conv modules needed for lab-level passes
# (common_conv and lab_lang_conv; the others may reference removed types)
CONV_MODS="common_conv lab_lang_conv"
for mod in $CONV_MODS; do
    echo "  il_types/${mod}.ml"
    $OC -c $OFLAGS -I il_types -I . il_types/${mod}.ml
done
# Build other conv modules only if they compile (best-effort)
for mod in clos_lang_conv flat_lang_conv bvl_lang_conv bvi_lang_conv \
           data_lang_conv word_lang_conv stack_lang_conv; do
    if $OC -c $OFLAGS -I il_types -I . il_types/${mod}.ml 2>/dev/null; then
        echo "  il_types/${mod}.ml (ok)"
    else
        echo "  il_types/${mod}.ml (skipped — type mismatch)"
    fi
done

# Step 5: Compile user pass files
if [ ${#PASS_FILES[@]} -gt 0 ]; then
    echo "[5/7] Compiling user pass files..."
    # Pre-collect all unique pass directories for include paths
    for f in "${PASS_FILES[@]}"; do
        pdir="$(dirname "$f")"
        if [ "$pdir" != "." ] && [[ ! "$PASS_IFLAGS" == *"-I $pdir"* ]]; then
            PASS_IFLAGS="$PASS_IFLAGS -I $pdir"
        fi
    done
    for f in "${PASS_FILES[@]}"; do
        echo "  $f"
        $OC -c $OFLAGS $IFLAGS $PASS_IFLAGS "$f"
    done
else
    echo "[5/7] No user pass files (skipped)"
fi

# Step 6: Compile hook setup and main entry
echo "[6/7] Compiling hook_setup and main_entry..."
# Add setup file directory to include paths
sdir="$(dirname "$SETUP_FILE")"
if [ "$sdir" != "." ] && [[ ! "$PASS_IFLAGS" == *"-I $sdir"* ]]; then
    PASS_IFLAGS="$PASS_IFLAGS -I $sdir"
fi
$OC -c $OFLAGS $IFLAGS $PASS_IFLAGS "$SETUP_FILE"
$OC -c $OFLAGS $IFLAGS $PASS_IFLAGS main_entry.ml

# Step 7: Link everything
echo "[7/7] Linking $OUTPUT..."

# Build list of pass .cmx files (preserve directory)
PASS_CMXS=""
for f in "${PASS_FILES[@]}"; do
    PASS_CMXS="$PASS_CMXS ${f%.ml}.cmx"
done

SETUP_CMX="${SETUP_FILE%.ml}.cmx"

# Collect conv .cmx files that exist (some may have been skipped)
CONV_CMXS=""
for mod in common_conv clos_lang_conv flat_lang_conv bvl_lang_conv \
           bvi_lang_conv data_lang_conv word_lang_conv stack_lang_conv \
           lab_lang_conv; do
    if [ -f "il_types/${mod}.cmx" ]; then
        CONV_CMXS="$CONV_CMXS il_types/${mod}.cmx"
    fi
done

bash -c "ulimit -s unlimited; $OC -linkpkg $OFLAGS $IFLAGS $PASS_IFLAGS \
    hook_ref.cmx \
    ../runtime/cakeml_runtime.cmx \
    il_types/common.cmx il_types/flat_lang.cmx il_types/clos_lang.cmx \
    il_types/bvl_lang.cmx il_types/bvi_lang.cmx il_types/data_lang.cmx \
    il_types/word_lang.cmx il_types/stack_lang.cmx il_types/lab_lang.cmx \
    cake64.cmx \
    $CONV_CMXS \
    $PASS_CMXS \
    $SETUP_CMX \
    main_entry.cmx \
    -o $OUTPUT"

echo
echo "Done! Binary: $OUTPUT"
echo "Test with: bash -c 'ulimit -s unlimited; echo \"val x = 1;\" | ./$OUTPUT'"
