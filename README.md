# CakeML S-Expression to OCaml Transpiler

Translates the CakeML compiler -- distributed as s-expression AST files
(`cake-sexpr-64`, `cake-sexpr-32`) -- into compilable OCaml source code.
The result is a working CakeML compiler written in OCaml: it reads CakeML
source from stdin and emits x86-64 (or x86-32) assembly to stdout.

## Quick start

Prerequisites: OCaml 4.14+, dune, Zarith, GCC.

```sh
# Build the transpiler
make build

# Generate OCaml source for the 64-bit CakeML compiler
make transpile          # produces generated/cake64.ml

# Generate a hooked compiler that emits Intel-syntax x86_64 assembly
./generated/build_hooked.sh passes/x64_intel_gen.ml cake64_x64_intel_dump

# Compile the generated OCaml into a native binary
# (requires large stack due to deeply nested generated code)
ulimit -s unlimited
OCAMLRUNPARAM="l=4G" \
  ocamlfind ocamlopt -package zarith,unix -linkpkg \
    -I runtime -I generated \
    runtime/cakeml_runtime.ml generated/cake64.ml \
    -o generated/cake64

# Use the resulting CakeML compiler
echo 'val _ = TextIO.print "Hello World\n"' \
  | ./generated/cake64 2>/dev/null > hello.S
gcc -o hello hello.S basis_ffi.c -lm
./hello
# => Hello World
```

The 32-bit compiler can be produced the same way with `make transpile32` and
`generated/cake32.ml`.


## Repository layout

```
ocaml_transpiler/
  bin/
    main.ml                  Entry point
    dune
  lib/
    sexp_parser.ml           S-expression lexer/parser
    cakeml_ast.ml            CakeML AST type definitions
    ast_parse.ml             S-expression -> CakeML AST conversion
    ocaml_emit.ml            CakeML AST -> OCaml source code emitter
    astScript.sml            Reference: CakeML AST definition in HOL4 (read-only)
    dune
  runtime/
    cakeml_runtime.ml        OCaml implementations of CakeML primitives
    dune
  test/
    hello.cml                Test: "Hello, World!"
    fib.cml                  Test: Fibonacci (fib 10 = 55)
    arith.cml                Test: arithmetic with printed output
    patmatch.cml             Test: datatypes and pattern matching
    higherorder.cml          Test: higher-order functions
    exceptions.cml           Test: raise/handle with payloads
    refs.cml                 Test: mutable references and closures
    strings.cml              Test: string operations
    mutualrec.cml            Test: mutual recursion
    sorting.cml              Test: quicksort
    option.cml               Test: option type and lookup
  generated/                 (gitignored) transpiler output goes here
  basis_ffi.c               C runtime for assembling CakeML compiler output
  test_bootstrap.sh         Bootstrap self-compilation test
  Makefile
  dune-project
```


## How it works

The transpiler is a four-stage pipeline.  Each stage is a separate module
in `lib/`.

### Stage 1: S-expression parsing (`sexp_parser.ml`)

Reads the `cake-sexpr-*` file into a generic tree:

```ocaml
type sexp = Atom of string | List of sexp list
```

The parser handles CakeML's **two-level string escape** convention, which
matches the encoding defined in `fromSexpTheory`:

- **Level 1 (PEG tokeniser):**  Inside a quoted string, only `\\` and `\"`
  are escape sequences (producing `\` and `"` respectively).  Any other
  `\X` is kept literally.
- **Level 2 (`decode_control`):**  After tokenisation, every string is
  post-processed: `\XX` where `XX` are uppercase hex digits and the
  resulting byte is non-printable (code < 32 or > 126) is replaced by that
  byte; `\\` becomes a single `\`; everything else is kept as-is.

Getting this right is essential: the s-expression files contain raw binary
data (e.g. null bytes for FFI file-descriptor encodings) embedded in
strings via this scheme.

### Stage 2: CakeML AST types (`cakeml_ast.ml`)

OCaml type definitions mirroring every CakeML AST node:

| Category       | Constructors |
|----------------|--------------|
| **Declarations** | `Dtype`, `Dlet`, `DletSimple`, `Dletrec`, `Dexn`, `Dtabbrev`, `Dmod`, `Dlocal`, `Denv` |
| **Expressions**  | `Fun`, `Mat`, `App`, `Var`, `Con`, `Lit`, `If`, `Let`, `Log`, `Raise`, `Handle`, `Letrec` |
| **Patterns**     | `Pvar`, `Pcon`, `Plit`, `Pany`, `Pref`, `Ptannot` |
| **Types**        | `Atvar`, `Atapp`, `Attup`, `Atfun` |
| **Operators**    | 60+ variants: arithmetic, comparison, word8/word64, string, array, byte array, vector, ref, float, FFI, etc. |
| **Literals**     | `IntLit` (arbitrary precision), `StrLit`, `CharLit`, `Word8Lit`, `Word64Lit`, `Float64Lit` |

### Stage 3: AST conversion (`ast_parse.ml`)

Converts generic `sexp` values into typed `cakeml_ast` by pattern-matching
on the s-expression structure.  Notable details:

- **Inline expressions in branches/bindings:**  CakeML s-expressions
  sometimes inline expression atoms at the same level as the enclosing
  form (e.g. `(pat Var (Short "x"))` instead of `(pat (Var (Short "x")))`).
  The `reconstitute_exp` function reconstructs expressions from these flat
  atom lists.
- **Locations:** `(unk unk)` location annotations are silently skipped.
- **`NONE`/`(SOME x)`/`nil`:** Structural markers, not CakeML values.

### Stage 4: OCaml code generation (`ocaml_emit.ml`)

Translates the typed AST into valid OCaml source.  Key translation rules:

| CakeML construct | OCaml output |
|------------------|-------------|
| `Con NONE (a, b)` | `(a, b)` (tuple) |
| `Con NONE ()` | `()` (unit) |
| `Con (Short "::") [h; t]` | `h :: t` |
| `Con (Short "True/False") []` | `true` / `false` |
| `Con (Short "None/Some") ...` | `None` / `Some ...` |
| `App Opapp [f; x]` | `f x` |
| `App op args` | `Cakeml_runtime.op args` |
| `Lit (IntLit n)` | `Z.of_string "n"` |
| `Lit (Word64Lit n)` | `nL` (with signed conversion for values >= 2^63) |
| `Lit (Float64Lit n)` | `Int64.float_of_bits nL` |
| `Dmod "M" decls` | `module M = struct ... end` |
| `Dlocal priv pub` | `module Dlocal_private_N_ = struct ... end; open ...` |
| `Dletrec bindings` | `let rec f1 a1 = ... and f2 a2 = ...` |

**Name handling:**

- **Keyword escaping:** OCaml reserved words are suffixed with `'`
  (e.g. `type` -> `type'`).
- **Special characters:** Spaces, operators, etc. are replaced with
  descriptive names (e.g. `+` -> `_plus_`, leading space -> `_` prefix).
- **Constructor/module names:** Forced to start with an uppercase letter.

**Type name resolution:**

The CakeML translator (via `use_full_type_names`) produces flattened
type names of the form `theoryName_typeName` (e.g.
`balanced_map_balanced_map`).  Since types are defined with just the short
suffix (`balanced_map`), the emitter resolves these at emit time by trying
all underscore-split suffixes against the set of known type definitions.

**Phantom type argument trimming:**

CakeML uses phantom type-level natural numbers (built from `bit0`/`bit1`)
for word sizes.  When a type application has more arguments than the
defined type's arity, excess (phantom) arguments are silently dropped.

**Pattern guard extraction:**

OCaml does not allow `Z.t` or `int64` literals directly in patterns.
When a match branch contains such literals, they are extracted into
`when` guards:  `| Plit (IntLit "42") -> ...` becomes
`| _lit_guard_0_ when _lit_guard_0_ = Z.of_string "42" -> ...`.


## Runtime library (`cakeml_runtime.ml`)

Provides OCaml implementations of all CakeML primitive operations.

### Integers

Arbitrary-precision integers via Zarith (`Z.t`).  Division and modulo use
CakeML's truncation-toward-zero semantics (not Euclidean).

### Word types

- **Word8:** OCaml `int` masked to 0-255.  All arithmetic masks the result.
- **Word64:** OCaml `int64` with natural wrapping.  Comparisons are unsigned
  (implemented by flipping the sign bit before `Int64.compare`).
  `w64_from_int` truncates arbitrary-precision integers to 64 bits via
  modular reduction, handling values that exceed `Int64.max_int`.

### Strings, arrays, byte arrays, vectors

Direct mappings to OCaml `string`, `'a array`, `bytes`, and `'a array`
(immutable usage) respectively.  Bounds-checked with `Subscript_exn`.

### Floating point

IEEE 754 double-precision via OCaml `float`.  Includes `fma`.
Bit-level conversion via `Int64.float_of_bits` / `Int64.bits_of_float`.

### FFI

The FFI layer implements CakeML's foreign function interface, matching
the protocol defined in `basis_ffi.c`:

| FFI name | Function |
|----------|----------|
| `""` (empty) | Debug message to stderr; `"nonzero_exit"` exits with code 1 |
| `write` | Write to fd: `c` = 8-byte BE fd, `a` = `[n:2, off:2, data...]`, returns `[err:1, nwritten:2]` |
| `read` | Read from fd: `c` = 8-byte BE fd, `a` = `[n:2, ...]`, returns `[err:1, nread:2, data...]` |
| `open_in` | Open file read-only: `c` = null-terminated filename, `a` = `[err:1, fd:8]` |
| `open_out` | Open file read-write (create/truncate): same format as `open_in` |
| `close` | Close fd: `c` = 8-byte BE fd, `a` = `[err:1]` |
| `exit` | Exit with code `a[0]` |
| `get_arg_count` | Write argc to `a[0:2]` (2-byte LE) |
| `get_arg_length` | Read arg index from `a[0:2]` (LE), write length to `a[0:2]` (LE) |
| `get_arg` | Read arg index from `a[0:2]` (LE), blit arg string into `a` |
| `double_*` | Float parsing/formatting/math operations |
| `poll_sigint` | No-op |

File descriptors are stored in an `(int, Unix.file_descr) Hashtbl.t` with
stdin/stdout/stderr pre-registered as 0/1/2.  The CakeML runtime
communicates fds as 8-byte big-endian integers in the `c` (config) string
parameter, and uses 2-byte big-endian integers for counts and offsets in
the `a` (mutable array) parameter.


## basis_ffi.c

C runtime required when assembling the x86 assembly output of the CakeML
compiler.  Provides `main`, `cml_main`, and the `ffi` dispatch function
that the compiled assembly calls for I/O.  Link it with GCC:

```sh
gcc -o program program.S basis_ffi.c -lm
```


## Test programs

The `test/` directory contains CakeML programs for validating the generated
compiler:

| File | Description | Expected output |
|------|-------------|-----------------|
| `hello.cml` | Print a string | `Hello, World!` |
| `fib.cml` | Compute fib(10) | `55` |
| `arith.cml` | Arithmetic and string conversion | `2 + 3 = 5` / `5 * 7 = 35` |
| `patmatch.cml` | User-defined datatypes and pattern matching | Areas of shapes |
| `higherorder.cml` | map, filter, foldl with lambdas | Transformed lists |
| `exceptions.cml` | raise/handle with payloads | Exception handling results |
| `refs.cml` | Mutable references and closures | Counter sequences |
| `strings.cml` | String explode/implode, palindrome check | String operations |
| `mutualrec.cml` | Mutual recursion (fun/and) | Even/odd classification |
| `sorting.cml` | Quicksort with partition | Sorted list |
| `option.cml` | Option type, association list lookup | Option results |

To run a test:

```sh
cat test/hello.cml | ./generated/cake64 2>/dev/null > /tmp/hello.S
gcc -o /tmp/hello /tmp/hello.S basis_ffi.c -lm
/tmp/hello
```


## Compiler hook system

The generated compiler supports injecting custom passes at 8 IL boundaries
via mutable hooks.  This is used to intercept and transform intermediate
representations without modifying `cake64.ml` directly.

### Prerequisites

```sh
# 1. Build the runtime
cd runtime && make && cd ..

# 2. Generate cake64.ml (if not already present)
make transpile

# 3. Requires: ocamlfind, zarith, unix
```

### Building a hooked compiler

```sh
cd generated

# Build with a hook setup file (and optional pass files):
./build_hooked.sh [pass_files...] <hook_setup.ml> [output_binary]

# Example: build the Intel-syntax textual assembly generator
./build_hooked.sh ../lib/x64_ast.ml ../lib/x64_intel_emit.ml passes/x64_intel_gen.ml cake64_intel

# Example: build the labLang assembly dumper
./build_hooked.sh passes/asm_dump_setup.ml cake64_asm_dump
```

The build script:
1. Compiles `hook_ref.ml` (mutable hooks, before cake64)
2. Patches `cake64.ml` to call hooks at IL boundaries
3. Compiles the patched `cake64.ml` (requires `ulimit -s unlimited`)
4. Compiles IL types, conversion modules, pass files, and hook setup
5. Links everything into the output binary

### Running a hooked compiler

```sh
echo 'val x = 1;' | bash -c 'ulimit -s unlimited; ./cake64_asm_dump'
```

### Hook points

| Hook | IL boundary | Variable | Type |
|------|-------------|----------|------|
| `flat_hook` | after source_to_flat | v50 | `flatLang_dec list` |
| `clos_hook` | after flat_to_clos | v47 | `closLang_exp list` |
| `bvl_hook` | after clos_to_bvl | v42 | `(name * (arity * bvl_exp)) list` |
| `bvi_hook` | after bvl_to_bvi | v35 | `(name * (arity * bvi_exp)) list` |
| `data_hook` | after bvi_to_data | v22 | `(name * (arity * dataLang_prog)) list` |
| `word_hook` | after data_to_word | v18 | `(name * (arity * wordLang_prog)) list` |
| `stack_hook` | after word_to_stack | v7 | `(name * stackLang_prog) list` |
| `lab_hook` | after stack_to_lab | v4 | `labLang_sec list` |

### Writing a hook setup

A hook setup module registers hooks at module init time (before `main()` runs):

```ocaml
let () =
  Hook_ref.lab_hook := (fun prog_obj ->
    let secs = Obj.obj prog_obj in
    let clean = List.map Lab_lang_conv.sec_of_gen secs in
    (* ... transform clean ... *)
    let gen = List.map Lab_lang_conv.sec_to_gen clean in
    Obj.repr gen)
```

Clean IL type definitions are in `il_types/` (e.g. `lab_lang.ml`, `common.ml`).
Conversion functions are in `il_types/*_conv.ml`.

### x64 Assembly Generation and Optimization

To facilitate writing x64 peephole optimizations, the transpiler provides an intermediate `X64_ast` representation for x86-64 assembly. This replaces direct string-based assembly emission with structured OCaml data types that can be pattern matched and transformed.

To use the x64 AST:
1. Ensure `../lib/x64_ast.ml` and `../lib/x64_intel_emit.ml` (or `../lib/x64_emit.ml` for AT&T syntax) are included in your pass files.
2. Intercept the `lab_hook` as shown in `generated/passes/x64_intel_gen.ml`.
3. Transform the generated `Lab_lang.sec list` into a list of `X64_ast.block` elements (which contain lists of `X64_ast.instr` instructions).
4. Apply your optimization passes by pattern matching over lists of instructions:

```ocaml
open X64_ast

let rec optimize_instrs = function
  | [] -> []
  (* Example peephole optimization: remove redundant move *)
  | Mov (dst1, src1) :: Mov (dst2, src2) :: rest 
      when dst1 = src2 && src1 = dst2 -> 
      Mov (dst1, src1) :: optimize_instrs rest
  | instr :: rest -> instr :: optimize_instrs rest

let optimize_block block =
  { block with instrs = optimize_instrs block.instrs }

let optimize_prog prog =
  List.map optimize_block prog
```

5. Emit the final AST to a channel using `X64_intel_emit.emit_prog stdout optimized_prog`.

Build the compiler with your optimization passes:

```sh
./build_hooked.sh ../lib/x64_ast.ml ../lib/x64_intel_emit.ml my_optimization_pass.ml passes/x64_intel_gen.ml cake64_optimizing
```


## Bootstrap test

`test_bootstrap.sh` verifies that the OCaml-transpiled compiler can
compile itself from its own s-expression AST, and that the resulting
native compiler produces identical output:

```
sexpr ─── cake64 (OCaml) ──→ gen1.S ─── gcc ──→ cake64_native
                                                       │
sexpr ─── cake64_native ───→ gen2.S                    │
                                                       │
assertion: gen1.S == gen2.S (byte-identical) ───────────┘
```

```sh
./test_bootstrap.sh                     # uses ../cake-sexpr-64 by default
./test_bootstrap.sh /path/to/sexpr      # or specify a different sexpr file
```

The script requires:
- `generated/cake64` to be built (the OCaml-transpiled compiler)
- `ulimit -s unlimited` for the OCaml binary (set automatically by the script)
- `CML_HEAP_SIZE=4096 CML_STACK_SIZE=4096` for the native binary
  (4GB heap + 4GB stack; set automatically, override via environment)
- `--sexp=true --skip_type_inference=true` flags (the sexpr contains
  `Denv` declarations that the type checker does not support)


## Build requirements

- **OCaml** 4.14+ with `ocamlfind` and `ocamlopt`
- **dune** 3.0+
- **Zarith** (`opam install zarith`)
- **GCC** (for assembling CakeML compiler output)
- **Input files:** `cake-sexpr-64` and/or `cake-sexpr-32` in the parent
  directory (the CakeML compiler ASTs in s-expression format)

The generated OCaml source is ~30,000 lines and requires a large stack to
compile:

```sh
ulimit -s unlimited
OCAMLRUNPARAM="l=4G" ocamlfind ocamlopt ...
```


## Design decisions

1. **Zarith for integers.**  CakeML uses arbitrary-precision integers;
   literals like `18446744073709551616` (2^64) appear in the compiler.

2. **Faithful translation.**  Variable names, control flow, and program
   structure are preserved exactly as they appear in the s-expression AST.
   The transpiler does not optimise or restructure.

3. **Dlocal -> module + open.**  CakeML's `Dlocal` (private/public
   declarations) is translated by wrapping private declarations in a
   generated module (`Dlocal_private_N_`) and immediately opening it.

4. **Built-in mappings.**  CakeML's `True`/`False` -> OCaml `true`/`false`;
   `::` and `[]` -> OCaml list constructors; `None`/`Some` -> OCaml option;
   `Bind`/`Div`/`Chr`/`Subscript` -> corresponding OCaml exceptions.

5. **Two-level string escaping.**  Matches CakeML's `fromSexpTheory`
   exactly: first a PEG-level unescape (`\\`->`\`, `\"`->`"`), then
   `decode_control` for hex-encoded non-printable bytes.


## Reference files

- `lib/astScript.sml` -- CakeML's AST definition in HOL4 (from
  `cake-master/compiler/ast/astScript.sml`), included as a reference for
  the AST types.  Not used by the build.
