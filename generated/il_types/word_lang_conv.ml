(* Conversion functions between Word_lang clean types and
   M_to_word64Prog generated types in cake64.ml *)

module G = Cake64
open Common_conv

(* -- wordLang_exp: Shift_2/Load_1/Var_3/Const_2 <-> Shift/Load/Var/Const -- *)

let rec exp_of_gen : G.M_to_word64Prog.wordLang_exp -> Word_lang.exp = function
  | G.M_to_word64Prog.Shift_2 (s, e, n) -> Word_lang.Shift (ast_shift_of_gen s, exp_of_gen e, n)
  | G.M_to_word64Prog.Op (b, es) -> Word_lang.Op (asm_binop_of_gen b, List.map exp_of_gen es)
  | G.M_to_word64Prog.Load_1 e -> Word_lang.Load (exp_of_gen e)
  | G.M_to_word64Prog.Lookup sn -> Word_lang.Lookup (store_name_of_gen sn)
  | G.M_to_word64Prog.Var_3 n -> Word_lang.Var n
  | G.M_to_word64Prog.Const_2 w -> Word_lang.Const w

let rec exp_to_gen : Word_lang.exp -> G.M_to_word64Prog.wordLang_exp = function
  | Word_lang.Shift (s, e, n) -> G.M_to_word64Prog.Shift_2 (ast_shift_to_gen s, exp_to_gen e, n)
  | Word_lang.Op (b, es) -> G.M_to_word64Prog.Op (asm_binop_to_gen b, List.map exp_to_gen es)
  | Word_lang.Load e -> G.M_to_word64Prog.Load_1 (exp_to_gen e)
  | Word_lang.Lookup sn -> G.M_to_word64Prog.Lookup (store_name_to_gen sn)
  | Word_lang.Var n -> G.M_to_word64Prog.Var_3 n
  | Word_lang.Const w -> G.M_to_word64Prog.Const_2 w

(* -- cutsets conversion (unit sptree_spt pairs) -- *)

let cutsets_of_gen ((a, b) : unit G.M_to_flatProg.sptree_spt * unit G.M_to_flatProg.sptree_spt) : Word_lang.cutsets =
  (sptree_spt_of_gen a, sptree_spt_of_gen b)

let cutsets_to_gen ((a, b) : Word_lang.cutsets) : unit G.M_to_flatProg.sptree_spt * unit G.M_to_flatProg.sptree_spt =
  (sptree_spt_to_gen a, sptree_spt_to_gen b)

(* -- wordLang_prog: Skip_1/Seq_2/If_1/Raise_1/Set_1/Store_1/Ffi_1 <-> clean -- *)

let rec prog_of_gen : G.M_to_word64Prog.wordLang_prog -> Word_lang.prog = function
  | G.M_to_word64Prog.Shareinst (m, r, e) ->
    Word_lang.Shareinst (asm_memop_of_gen m, r, exp_of_gen e)
  | G.M_to_word64Prog.Ffi_1 (s, a, b, c, d, cs) ->
    Word_lang.Ffi (s, a, b, c, d, cutsets_of_gen cs)
  | G.M_to_word64Prog.Databufferwrite (a, b) -> Word_lang.Databufferwrite (a, b)
  | G.M_to_word64Prog.Codebufferwrite (a, b) -> Word_lang.Codebufferwrite (a, b)
  | G.M_to_word64Prog.Install (a, b, c, d, cs) ->
    Word_lang.Install (a, b, c, d, cutsets_of_gen cs)
  | G.M_to_word64Prog.Locvalue (a, b) -> Word_lang.Locvalue (a, b)
  | G.M_to_word64Prog.Opcurrheap (op, a, b) ->
    Word_lang.Opcurrheap (asm_binop_of_gen op, a, b)
  | G.M_to_word64Prog.Tick -> Word_lang.Tick
  | G.M_to_word64Prog.Return (r, rs) -> Word_lang.Return (r, rs)
  | G.M_to_word64Prog.Raise_1 r -> Word_lang.Raise r
  | G.M_to_word64Prog.Storeconsts (a, b, c, d, ws) -> Word_lang.Storeconsts (a, b, c, d, ws)
  | G.M_to_word64Prog.Alloc (r, cs) -> Word_lang.Alloc (r, cutsets_of_gen cs)
  | G.M_to_word64Prog.If_1 (c, r, ri, t, f) ->
    Word_lang.If (asm_cmp_of_gen c, r, asm_reg_imm_of_gen ri, prog_of_gen t, prog_of_gen f)
  | G.M_to_word64Prog.Seq_2 (a, b) -> Word_lang.Seq (prog_of_gen a, prog_of_gen b)
  | G.M_to_word64Prog.Call (ret, dest, args, handler) ->
    Word_lang.Call
      ( (match ret with
         | None -> None
         | Some (rs, (cs, (p, (l1, l2)))) ->
           Some (rs, (cutsets_of_gen cs, (prog_of_gen p, (l1, l2))))),
        dest, args,
        (match handler with
         | None -> None
         | Some (r, (p, (l1, l2))) -> Some (r, (prog_of_gen p, (l1, l2)))) )
  | G.M_to_word64Prog.Mustterminate p -> Word_lang.Mustterminate (prog_of_gen p)
  | G.M_to_word64Prog.Store_1 (e, r) -> Word_lang.Store (exp_of_gen e, r)
  | G.M_to_word64Prog.Set_1 (sn, e) -> Word_lang.Set (store_name_of_gen sn, exp_of_gen e)
  | G.M_to_word64Prog.Get (r, sn) -> Word_lang.Get (r, store_name_of_gen sn)
  | G.M_to_word64Prog.Assign (r, e) -> Word_lang.Assign (r, exp_of_gen e)
  | G.M_to_word64Prog.Inst i -> Word_lang.Inst (asm_inst_of_gen i)
  | G.M_to_word64Prog.Move (r, mvs) -> Word_lang.Move (r, mvs)
  | G.M_to_word64Prog.Skip_1 -> Word_lang.Skip

let rec prog_to_gen : Word_lang.prog -> G.M_to_word64Prog.wordLang_prog = function
  | Word_lang.Shareinst (m, r, e) ->
    G.M_to_word64Prog.Shareinst (asm_memop_to_gen m, r, exp_to_gen e)
  | Word_lang.Ffi (s, a, b, c, d, cs) ->
    G.M_to_word64Prog.Ffi_1 (s, a, b, c, d, cutsets_to_gen cs)
  | Word_lang.Databufferwrite (a, b) -> G.M_to_word64Prog.Databufferwrite (a, b)
  | Word_lang.Codebufferwrite (a, b) -> G.M_to_word64Prog.Codebufferwrite (a, b)
  | Word_lang.Install (a, b, c, d, cs) ->
    G.M_to_word64Prog.Install (a, b, c, d, cutsets_to_gen cs)
  | Word_lang.Locvalue (a, b) -> G.M_to_word64Prog.Locvalue (a, b)
  | Word_lang.Opcurrheap (op, a, b) ->
    G.M_to_word64Prog.Opcurrheap (asm_binop_to_gen op, a, b)
  | Word_lang.Tick -> G.M_to_word64Prog.Tick
  | Word_lang.Return (r, rs) -> G.M_to_word64Prog.Return (r, rs)
  | Word_lang.Raise r -> G.M_to_word64Prog.Raise_1 r
  | Word_lang.Storeconsts (a, b, c, d, ws) -> G.M_to_word64Prog.Storeconsts (a, b, c, d, ws)
  | Word_lang.Alloc (r, cs) -> G.M_to_word64Prog.Alloc (r, cutsets_to_gen cs)
  | Word_lang.If (c, r, ri, t, f) ->
    G.M_to_word64Prog.If_1 (asm_cmp_to_gen c, r, asm_reg_imm_to_gen ri, prog_to_gen t, prog_to_gen f)
  | Word_lang.Seq (a, b) -> G.M_to_word64Prog.Seq_2 (prog_to_gen a, prog_to_gen b)
  | Word_lang.Call (ret, dest, args, handler) ->
    G.M_to_word64Prog.Call
      ( (match ret with
         | None -> None
         | Some (rs, (cs, (p, (l1, l2)))) ->
           Some (rs, (cutsets_to_gen cs, (prog_to_gen p, (l1, l2))))),
        dest, args,
        (match handler with
         | None -> None
         | Some (r, (p, (l1, l2))) -> Some (r, (prog_to_gen p, (l1, l2)))) )
  | Word_lang.Mustterminate p -> G.M_to_word64Prog.Mustterminate (prog_to_gen p)
  | Word_lang.Store (e, r) -> G.M_to_word64Prog.Store_1 (exp_to_gen e, r)
  | Word_lang.Set (sn, e) -> G.M_to_word64Prog.Set_1 (store_name_to_gen sn, exp_to_gen e)
  | Word_lang.Get (r, sn) -> G.M_to_word64Prog.Get (r, store_name_to_gen sn)
  | Word_lang.Assign (r, e) -> G.M_to_word64Prog.Assign (r, exp_to_gen e)
  | Word_lang.Inst i -> G.M_to_word64Prog.Inst (asm_inst_to_gen i)
  | Word_lang.Move (r, mvs) -> G.M_to_word64Prog.Move (r, mvs)
  | Word_lang.Skip -> G.M_to_word64Prog.Skip_1
