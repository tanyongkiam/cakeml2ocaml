open X64_ast

(*
 * Peephole optimization pass over the x64 AST.
 * This pass applies x64-specific architectural optimizations.
 *)

let rec optimize_instrs instrs =
  match instrs with
  | [] -> []

  (* 1. Increment and Decrement Substitution *)
  | Add (Reg r, Imm 1L) :: rest ->
      Inc (Reg r) :: optimize_instrs rest
  | Sub (Reg r, Imm 1L) :: rest ->
      Dec (Reg r) :: optimize_instrs rest
  | Add (Reg r, Imm -1L) :: rest ->
      Dec (Reg r) :: optimize_instrs rest
  | Sub (Reg r, Imm -1L) :: rest ->
      Inc (Reg r) :: optimize_instrs rest

  (* 2. XOR Zeroing *)
  | Mov (Reg r, Imm 0L) :: rest ->
      Xor (Reg r, Reg r) :: optimize_instrs rest

  (* 3. CMP to TEST *)
  | Cmp (Reg r, Imm 0L) :: rest ->
      Test (Reg r, Reg r) :: optimize_instrs rest

  (* 4. Add/Sub Imm Sign Canonicalization *)
  | Add (Reg r, Imm n) :: rest when n < 0L ->
      Sub (Reg r, Imm (Int64.neg n)) :: optimize_instrs rest
  | Sub (Reg r, Imm n) :: rest when n < 0L ->
      Add (Reg r, Imm (Int64.neg n)) :: optimize_instrs rest

  | instr :: rest ->
      instr :: optimize_instrs rest

let rec optimize_instrs_fixed_point instrs =
  let optimized = optimize_instrs instrs in
  if optimized = instrs then
    optimized
  else
    optimize_instrs_fixed_point optimized

let optimize_block block =
  { block with instrs = optimize_instrs_fixed_point block.instrs }

let optimize_prog prog =
  List.map optimize_block prog
