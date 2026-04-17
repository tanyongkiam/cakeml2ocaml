open X64_ast

(*
 * Peephole optimization pass over the x64 AST.
 *)

let same_reg r1 r2 = (r1 = r2)

let same_mem (m1 : mem) (m2 : mem) =
  m1.base  = m2.base  &&
  m1.index = m2.index &&
  m1.scale = m2.scale &&
  Int64.equal m1.disp m2.disp &&
  m1.is_rip = m2.is_rip

let pass_counts = Hashtbl.create 16

let record_pass name =
  let count = match Hashtbl.find_opt pass_counts name with
    | Some c -> c
    | None -> 0
  in
  Hashtbl.replace pass_counts name (count + 1)

let print_pass_counts () =
  Hashtbl.iter (fun name count ->
    Printf.eprintf "%s: %d\n" name count
  ) pass_counts;
  flush stderr

(* Returns Some k when n = 2^k, k >= 1. *)
let log2_pow2 n =
  if Int64.compare n 2L < 0 then None
  else
    let rec go k v =
      if Int64.equal v n then Some k
      else if Int64.compare v n > 0 then None
      else go (k + 1) (Int64.mul v 2L)
    in
    go 1 2L

let rec optimize_instrs = function
  | [] -> []

  (* Pass A1: Arithmetic (not+add → sub+not) *)
  | Not (Reg rA1)
    :: Add (Reg rA2, Reg rB)
    :: rest
    when same_reg rA1 rA2 ->
      record_pass "Pass A1";
      Sub (Reg rA1, Reg rB)
      :: Not (Reg rA1)
      :: optimize_instrs rest

  (* Pass A2: Arithmetic (mov-const+sub+dec → mov-(const-1)+sub) *)
  | Mov (Reg rA1, Imm n)
    :: Sub (Reg rA2, Reg rC)
    :: Dec (Reg rA3)
    :: rest
    when same_reg rA1 rA2 && same_reg rA2 rA3 ->
      record_pass "Pass A2";
      Mov (Reg rA1, Imm (Int64.sub n 1L))
      :: Sub (Reg rA1, Reg rC)
      :: optimize_instrs rest

  | Mov (Reg32 rA1, Imm n)
    :: Sub (Reg32 rA2, Reg rC)
    :: Dec (Reg rA3)
    :: rest
    when same_reg rA1 rA2 && same_reg rA2 rA3 ->
      record_pass "Pass A2";
      Mov (Reg32 rA1, Imm (Int64.sub n 1L))
      :: Sub (Reg32 rA1, Reg rC)
      :: optimize_instrs rest

  (* Pass A3: Arithmetic (div by power-of-two → shr) *)
  | Mov (Reg rC1, Imm n) :: Div (Reg rC2) :: rest
    when same_reg rC1 rC2 ->
      (match log2_pow2 n with
       | Some k ->
           record_pass "Pass A3";
           Shr (Reg Rax, Imm (Int64.of_int k)) :: optimize_instrs rest
       | None ->
           Mov (Reg rC1, Imm n) :: optimize_instrs (Div (Reg rC2) :: rest))

  | Mov (Reg32 rC1, Imm n) :: Div (Reg32 rC2) :: rest
    when same_reg rC1 rC2 ->
      (match log2_pow2 n with
       | Some k ->
           record_pass "Pass A3";
           Shr (Reg Rax, Imm (Int64.of_int k)) :: optimize_instrs rest
       | None ->
           Mov (Reg32 rC1, Imm n) :: optimize_instrs (Div (Reg32 rC2) :: rest))

  (* Pass A4: Arithmetic (imul $2^k, rA → sal $k, rA) *)
  | Imul (Reg rA,   Imm n) :: rest ->
      (match log2_pow2 n with
       | Some k -> record_pass "Pass A4"; Shl (Reg rA,   Imm (Int64.of_int k)) :: optimize_instrs rest
       | None   -> Imul (Reg rA,   Imm n)               :: optimize_instrs rest)
  | Imul (Reg32 rA, Imm n) :: rest ->
      (match log2_pow2 n with
       | Some k -> record_pass "Pass A4"; Shl (Reg32 rA, Imm (Int64.of_int k)) :: optimize_instrs rest
       | None   -> Imul (Reg32 rA, Imm n)                :: optimize_instrs rest)

  (* Pass A5: Arithmetic (add/sub 1 or -1 → inc/dec) *)
  | Add (Reg   r, Imm  1L) :: rest -> record_pass "Pass A5"; Inc (Reg   r) :: optimize_instrs rest
  | Add (Reg32 r, Imm  1L) :: rest -> record_pass "Pass A5"; Inc (Reg32 r) :: optimize_instrs rest
  | Sub (Reg   r, Imm  1L) :: rest -> record_pass "Pass A5"; Dec (Reg   r) :: optimize_instrs rest
  | Sub (Reg32 r, Imm  1L) :: rest -> record_pass "Pass A5"; Dec (Reg32 r) :: optimize_instrs rest
  | Sub (Reg   r, Imm (-1L)) :: rest -> record_pass "Pass A5"; Inc (Reg   r) :: optimize_instrs rest
  | Sub (Reg32 r, Imm (-1L)) :: rest -> record_pass "Pass A5"; Inc (Reg32 r) :: optimize_instrs rest
  | Add (Reg   r, Imm (-1L)) :: rest -> record_pass "Pass A5"; Dec (Reg   r) :: optimize_instrs rest
  | Add (Reg32 r, Imm (-1L)) :: rest -> record_pass "Pass A5"; Dec (Reg32 r) :: optimize_instrs rest

  (* Pass A6: Arithmetic (add/sub negative → sub/add positive) *)
  | Add (Reg   r, Imm n) :: rest when Int64.compare n 0L < 0 ->
      record_pass "Pass A6"; Sub (Reg   r, Imm (Int64.neg n)) :: optimize_instrs rest
  | Add (Reg32 r, Imm n) :: rest when Int64.compare n 0L < 0 ->
      record_pass "Pass A6"; Sub (Reg32 r, Imm (Int64.neg n)) :: optimize_instrs rest
  | Sub (Reg   r, Imm n) :: rest when Int64.compare n 0L < 0 ->
      record_pass "Pass A6"; Add (Reg   r, Imm (Int64.neg n)) :: optimize_instrs rest
  | Sub (Reg32 r, Imm n) :: rest when Int64.compare n 0L < 0 ->
      record_pass "Pass A6"; Add (Reg32 r, Imm (Int64.neg n)) :: optimize_instrs rest

  (* Pass B1: Redundancy (store-then-reload from same slot) *)
  | Mov (Mem m1, Reg rA)
    :: Mov (Reg rB, Mem m2)
    :: rest
    when same_mem m1 m2 && not (same_reg rA rB) ->
      record_pass "Pass B1";
      Mov (Mem m1, Reg rA)
      :: Mov (Reg rB, Reg rA)
      :: optimize_instrs rest

  (* Pass B2: Redundancy (inc+dec → nop) *)
  | Inc (Reg rA1) :: Dec (Reg rA2) :: rest
    when same_reg rA1 rA2 ->
      record_pass "Pass B2";
      Nop :: optimize_instrs rest

  | Inc (Reg32 rA1) :: Dec (Reg32 rA2) :: rest
    when same_reg rA1 rA2 ->
      record_pass "Pass B2";
      Nop :: optimize_instrs rest

  (* Pass B3: Redundancy (inc+inc+dec → inc) *)
  | Inc (Reg rA1) :: Inc (Reg rA2) :: Dec (Reg rA3) :: rest
    when same_reg rA1 rA2 && same_reg rA2 rA3 ->
      record_pass "Pass B3";
      Inc (Reg rA1) :: optimize_instrs rest

  | Inc (Reg32 rA1) :: Inc (Reg32 rA2) :: Dec (Reg32 rA3) :: rest
    when same_reg rA1 rA2 && same_reg rA2 rA3 ->
      record_pass "Pass B3";
      Inc (Reg32 rA1) :: optimize_instrs rest

  (* Pass B4: Redundancy (mov rA, rA → nop) *)
  | Mov (Reg rA1,   Reg rA2)   :: rest when same_reg rA1 rA2 ->
      record_pass "Pass B4"; Nop :: optimize_instrs rest
  | Mov (Reg32 rA1, Reg32 rA2) :: rest when same_reg rA1 rA2 ->
      record_pass "Pass B4"; Nop :: optimize_instrs rest

  (* Pass B5: Redundancy (dead store to reg) *)
  (* | Mov (Reg rB1, _)
    :: Mov (Reg rB2, src2)
    :: rest
    when same_reg rB1 rB2 ->
      record_pass "Pass B5";
      Mov (Reg rB2, src2) :: optimize_instrs rest *)

  (* Pass C1: Control Flow (jmp to immediately following label → nop) *)
  | Jmp (Lbl lbl1) :: InlineLbl lbl2 :: rest
    when lbl1 = lbl2 ->
      record_pass "Pass C1";
      Nop :: InlineLbl lbl2 :: optimize_instrs rest

  (* Pass D1: LEA/Reg reorder (mov+lea+test reorder) *)
  | Mov (Reg rB1, Reg rA1)
    :: Lea (Reg rA2,
            Mem { base=Some rB2; index=None; scale=S1;
                  disp=(-1L); is_rip=false; size=_ })
    :: Test (Reg rB3, Reg rB4)
    :: rest
    when same_reg rA1 rA2
      && same_reg rB1 rB2 && same_reg rB1 rB3 && same_reg rB3 rB4
      && not (same_reg rA1 rB1) ->
      record_pass "Pass D1";
      Test (Reg rA1, Reg rA1)
      :: Lea (Reg rA2,
              Mem { base=Some rB2; index=None; scale=S1;
                    disp=(-1L); is_rip=false; size=None })
      :: optimize_instrs rest

  (* Pass D2: LEA simplification (lea base+index, no disp → add) *)
  | Lea (Reg rDst,
         Mem { base=Some rBase; index=Some rIdx; scale=S1;
               disp=0L; is_rip=false; size=_ })
    :: rest
    when same_reg rDst rBase ->
      record_pass "Pass D2";
      Add (Reg rDst, Reg rIdx) :: optimize_instrs rest

  | Lea (Reg rDst,
         Mem { base=Some rBase; index=Some rIdx; scale=S1;
               disp=0L; is_rip=false; size=_ })
    :: rest
    when same_reg rDst rIdx ->
      record_pass "Pass D2";
      Add (Reg rDst, Reg rBase) :: optimize_instrs rest

  (* Pass E1: Zeroing (mov $0 → xor) *)
  | Mov (Reg rA,   Imm 0L) :: rest ->
      record_pass "Pass E1"; Xor (Reg32 rA, Reg32 rA) :: optimize_instrs rest
  | Mov (Reg32 rA, Imm 0L) :: rest ->
      record_pass "Pass E1"; Xor (Reg32 rA, Reg32 rA) :: optimize_instrs rest

  (* Pass E2: Zeroing (xor-then-store → mov $0) *)
  (* | Xor (Reg32 rA1, Reg32 rA2)
    :: Mov (Mem m, Reg rA3)
    :: rest
    when same_reg rA1 rA2 && same_reg rA1 rA3 ->
      record_pass "Pass E2";
      Nop
      :: Mov (Mem m, Imm 0L)
      :: optimize_instrs rest

  | Xor (Reg rA1, Reg rA2)
    :: Mov (Mem m, Reg rA3)
    :: rest
    when same_reg rA1 rA2 && same_reg rA1 rA3 ->
      record_pass "Pass E2";
      Nop
      :: Mov (Mem m, Imm 0L)
      :: optimize_instrs rest *)

  (* Pass E3: Test (cmp $0 → test) *)
  | Cmp (Reg   r, Imm 0L) :: rest -> record_pass "Pass E3"; Test (Reg   r, Reg   r) :: optimize_instrs rest
  | Cmp (Reg32 r, Imm 0L) :: rest -> record_pass "Pass E3"; Test (Reg32 r, Reg32 r) :: optimize_instrs rest

  | instr :: rest -> instr :: optimize_instrs rest

let rec optimize_instrs_fixed_point instrs =
  let optimized = optimize_instrs instrs in
  if optimized = instrs then optimized
  else optimize_instrs_fixed_point optimized

let optimize_block block =
  { block with instrs = optimize_instrs_fixed_point block.instrs }

let optimize_prog prog =
  let optimized_prog = List.map optimize_block prog in
  print_pass_counts ();
  optimized_prog
