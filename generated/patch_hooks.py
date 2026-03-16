#!/usr/bin/env python3
"""Patch cake64.ml to insert hook calls at all 8 IL boundaries in backend_compile.

Each "finished: <marker>" FFI call in backend_compile is followed by the next
compilation step. We insert a hook call (variable shadowing) between the FFI
marker and the next step.

Uses exact string matching (no regex) and replaces only the FIRST occurrence
of each marker (which is always in backend_compile; the second is in
compiler_for_eval which we don't hook).
"""

import sys

MARKER_TEMPLATE = '"finished: {marker}" ((Cakeml_runtime.aw8alloc Z.zero 0))) in '

# (marker_name, hooked_variable, hook_ref_name)
HOOKS = [
    ("source_to_flat", "v50", "flat_hook"),
    ("flat_to_clos",   "v47", "clos_hook"),
    ("clos_to_bvl",    "v42", "bvl_hook"),
    ("bvl_to_bvi",     "v35", "bvi_hook"),
    ("bvi_to_data",    "v22", "data_hook"),
    ("data_to_word",   "v18", "word_hook"),
    ("word_to_stack",  "v7",  "stack_hook"),
    ("stack_to_lab",   "v4",  "lab_hook"),
]

def patch(text):
    for marker, var, hook in HOOKS:
        old = MARKER_TEMPLATE.format(marker=marker)
        new = (old +
               f'let {var} = (Obj.obj (!Hook_ref.{hook} (Obj.repr {var}))) in ')
        count = text.count(old)
        if count == 0:
            print(f"ERROR: marker '{marker}' not found", file=sys.stderr)
            sys.exit(1)
        # Replace only first occurrence (backend_compile)
        text = text.replace(old, new, 1)
        print(f"  Hooked {var} at {marker} ({hook})")

    # Remove standalone main entry point
    text = text.replace('\nlet () = (main (()))\n', '\n')
    print("  Removed main entry point")

    return text

if __name__ == "__main__":
    infile = sys.argv[1] if len(sys.argv) > 1 else "cake64.ml"
    with open(infile) as f:
        text = f.read()
    text = patch(text)
    with open(infile, 'w') as f:
        f.write(text)
    print(f"  Patched {infile} with 8 hooks")
