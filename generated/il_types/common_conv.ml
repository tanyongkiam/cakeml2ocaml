(* Conversion functions between Common clean types and generated cake64.ml types.
   Types that are identical (no renaming needed) can use direct casting via
   Obj.magic since they have the same runtime representation.
   Only types with renamed constructors need actual conversion functions. *)

module G = Cake64

(* -- ast_test: Equal_1 <-> Equal -- *)

let ast_test_of_gen : G.ast_test -> Common.ast_test = function
  | G.Altcompare ob -> Common.Altcompare (Obj.magic ob)
  | G.Compare ob -> Common.Compare (Obj.magic ob)
  | G.Equal_1 -> Common.Equal

let ast_test_to_gen : Common.ast_test -> G.ast_test = function
  | Common.Altcompare ob -> G.Altcompare (Obj.magic ob)
  | Common.Compare ob -> G.Compare (Obj.magic ob)
  | Common.Equal -> G.Equal_1

(* -- ast_temp_arith: *_1/*_2 <-> clean -- *)

let ast_temp_arith_of_gen : G.ast_temp_arith -> Common.ast_temp_arith = function
  | G.Fma -> Common.Fma
  | G.Sqrt -> Common.Sqrt
  | G.Abs_2 -> Common.Abs
  | G.Not -> Common.Not
  | G.Or_1 -> Common.Or
  | G.Xor_1 -> Common.Xor
  | G.And_1 -> Common.And
  | G.Neg -> Common.Neg
  | G.Mod -> Common.Mod
  | G.Div_1 -> Common.Div
  | G.Mul -> Common.Mul
  | G.Sub_1 -> Common.Sub
  | G.Add_1 -> Common.Add

let ast_temp_arith_to_gen : Common.ast_temp_arith -> G.ast_temp_arith = function
  | Common.Fma -> G.Fma
  | Common.Sqrt -> G.Sqrt
  | Common.Abs -> G.Abs_2
  | Common.Not -> G.Not
  | Common.Or -> G.Or_1
  | Common.Xor -> G.Xor_1
  | Common.And -> G.And_1
  | Common.Neg -> G.Neg
  | Common.Mod -> G.Mod
  | Common.Div -> G.Div_1
  | Common.Mul -> G.Mul
  | Common.Sub -> G.Sub_1
  | Common.Add -> G.Add_1

(* -- Types with identical constructors: use Obj.magic for zero-cost conversion -- *)
(* ast_lit, ast_shift, ast_opb, ast_opn, ast_opw, ast_word_size,
   ast_fp_uop, ast_fp_bop, ast_fp_top, ast_fp_cmp,
   ast_thunk_mode, ast_thunk_op, ast_prim_type *)

let ast_lit_of_gen : G.ast_lit -> Common.ast_lit = Obj.magic
let ast_lit_to_gen : Common.ast_lit -> G.ast_lit = Obj.magic

let ast_shift_of_gen : G.ast_shift -> Common.ast_shift = Obj.magic
let ast_shift_to_gen : Common.ast_shift -> G.ast_shift = Obj.magic

let ast_opb_of_gen : G.ast_opb -> Common.ast_opb = Obj.magic
let ast_opb_to_gen : Common.ast_opb -> G.ast_opb = Obj.magic

let ast_opn_of_gen : G.ast_opn -> Common.ast_opn = Obj.magic
let ast_opn_to_gen : Common.ast_opn -> G.ast_opn = Obj.magic

let ast_opw_of_gen : G.ast_opw -> Common.ast_opw = Obj.magic
let ast_opw_to_gen : Common.ast_opw -> G.ast_opw = Obj.magic

let ast_word_size_of_gen : G.ast_word_size -> Common.ast_word_size = Obj.magic
let ast_word_size_to_gen : Common.ast_word_size -> G.ast_word_size = Obj.magic

let ast_fp_uop_of_gen : G.ast_fp_uop -> Common.ast_fp_uop = Obj.magic
let ast_fp_uop_to_gen : Common.ast_fp_uop -> G.ast_fp_uop = Obj.magic

let ast_fp_bop_of_gen : G.ast_fp_bop -> Common.ast_fp_bop = Obj.magic
let ast_fp_bop_to_gen : Common.ast_fp_bop -> G.ast_fp_bop = Obj.magic

let ast_fp_top_of_gen : G.ast_fp_top -> Common.ast_fp_top = Obj.magic
let ast_fp_top_to_gen : Common.ast_fp_top -> G.ast_fp_top = Obj.magic

let ast_fp_cmp_of_gen : G.ast_fp_cmp -> Common.ast_fp_cmp = Obj.magic
let ast_fp_cmp_to_gen : Common.ast_fp_cmp -> G.ast_fp_cmp = Obj.magic

let ast_thunk_mode_of_gen : G.ast_thunk_mode -> Common.ast_thunk_mode = Obj.magic
let ast_thunk_mode_to_gen : Common.ast_thunk_mode -> G.ast_thunk_mode = Obj.magic

let ast_thunk_op_of_gen : G.ast_thunk_op -> Common.ast_thunk_op = Obj.magic
let ast_thunk_op_to_gen : Common.ast_thunk_op -> G.ast_thunk_op = Obj.magic

let ast_prim_type_of_gen : G.ast_prim_type -> Common.ast_prim_type = Obj.magic
let ast_prim_type_to_gen : Common.ast_prim_type -> G.ast_prim_type = Obj.magic

(* -- sptree_spt: identical constructors -- *)

let sptree_spt_of_gen : 'a G.M_to_flatProg.sptree_spt -> 'a Common.sptree_spt = Obj.magic
let sptree_spt_to_gen : 'a Common.sptree_spt -> 'a G.M_to_flatProg.sptree_spt = Obj.magic

(* -- backend_common_tra: None_1 <-> None -- *)

let rec tra_of_gen : G.M_to_flatProg.backend_common_tra -> Common.backend_common_tra = function
  | G.M_to_flatProg.None_1 -> Common.None
  | G.M_to_flatProg.Union (a, b) -> Common.Union (tra_of_gen a, tra_of_gen b)
  | G.M_to_flatProg.Cons (a, n) -> Common.Cons (tra_of_gen a, n)
  | G.M_to_flatProg.Sourceloc (a, b, c, d) -> Common.Sourceloc (a, b, c, d)

let rec tra_to_gen : Common.backend_common_tra -> G.M_to_flatProg.backend_common_tra = function
  | Common.None -> G.M_to_flatProg.None_1
  | Common.Union (a, b) -> G.M_to_flatProg.Union (tra_to_gen a, tra_to_gen b)
  | Common.Cons (a, n) -> G.M_to_flatProg.Cons (tra_to_gen a, n)
  | Common.Sourceloc (a, b, c, d) -> G.M_to_flatProg.Sourceloc (a, b, c, d)

(* -- namespace_namespace: Bind_1 <-> Bind -- *)

let namespace_of_gen : ('m, 'n, 'w) G.M_to_flatProg.namespace_namespace -> ('m, 'n, 'w) Common.namespace_namespace = Obj.magic
let namespace_to_gen : ('m, 'n, 'w) Common.namespace_namespace -> ('m, 'n, 'w) G.M_to_flatProg.namespace_namespace = Obj.magic

(* -- asm types from M_to_word64Prog -- *)

(* asm_memop: identical constructors *)
let asm_memop_of_gen : G.M_to_word64Prog.asm_memop -> Common.asm_memop = Obj.magic
let asm_memop_to_gen : Common.asm_memop -> G.M_to_word64Prog.asm_memop = Obj.magic

(* asm_binop: Add_2/Sub_2/And_2/Or_2/Xor_2 <-> Add/Sub/And/Or/Xor *)
let asm_binop_of_gen : G.M_to_word64Prog.asm_binop -> Common.asm_binop = function
  | G.M_to_word64Prog.Xor_2 -> Common.Xor
  | G.M_to_word64Prog.Or_2 -> Common.Or
  | G.M_to_word64Prog.And_2 -> Common.And
  | G.M_to_word64Prog.Sub_2 -> Common.Sub
  | G.M_to_word64Prog.Add_2 -> Common.Add

let asm_binop_to_gen : Common.asm_binop -> G.M_to_word64Prog.asm_binop = function
  | Common.Xor -> G.M_to_word64Prog.Xor_2
  | Common.Or -> G.M_to_word64Prog.Or_2
  | Common.And -> G.M_to_word64Prog.And_2
  | Common.Sub -> G.M_to_word64Prog.Sub_2
  | Common.Add -> G.M_to_word64Prog.Add_2

(* store_name: identical constructors *)
let store_name_of_gen : G.M_to_word64Prog.stackLang_store_name -> Common.store_name = Obj.magic
let store_name_to_gen : Common.store_name -> G.M_to_word64Prog.stackLang_store_name = Obj.magic

(* asm_cmp: Equal_2/Less_1/Test_1 <-> Equal/Less/Test *)
let asm_cmp_of_gen : G.M_to_word64Prog.asm_cmp -> Common.asm_cmp = function
  | G.M_to_word64Prog.Nottest -> Common.Nottest
  | G.M_to_word64Prog.Notless -> Common.Notless
  | G.M_to_word64Prog.Notlower -> Common.Notlower
  | G.M_to_word64Prog.Notequal -> Common.Notequal
  | G.M_to_word64Prog.Test_1 -> Common.Test
  | G.M_to_word64Prog.Less_1 -> Common.Less
  | G.M_to_word64Prog.Lower -> Common.Lower
  | G.M_to_word64Prog.Equal_2 -> Common.Equal

let asm_cmp_to_gen : Common.asm_cmp -> G.M_to_word64Prog.asm_cmp = function
  | Common.Nottest -> G.M_to_word64Prog.Nottest
  | Common.Notless -> G.M_to_word64Prog.Notless
  | Common.Notlower -> G.M_to_word64Prog.Notlower
  | Common.Notequal -> G.M_to_word64Prog.Notequal
  | Common.Test -> G.M_to_word64Prog.Test_1
  | Common.Less -> G.M_to_word64Prog.Less_1
  | Common.Lower -> G.M_to_word64Prog.Lower
  | Common.Equal -> G.M_to_word64Prog.Equal_2

(* asm_reg_imm: identical constructors *)
let asm_reg_imm_of_gen : G.M_to_word64Prog.asm_reg_imm -> Common.asm_reg_imm = Obj.magic
let asm_reg_imm_to_gen : Common.asm_reg_imm -> G.M_to_word64Prog.asm_reg_imm = Obj.magic

(* asm_addr: identical constructor *)
let asm_addr_of_gen : G.M_to_word64Prog.asm_addr -> Common.asm_addr = Obj.magic
let asm_addr_to_gen : Common.asm_addr -> G.M_to_word64Prog.asm_addr = Obj.magic

(* asm_fp: identical constructors *)
let asm_fp_of_gen : G.M_to_word64Prog.asm_fp -> Common.asm_fp = Obj.magic
let asm_fp_to_gen : Common.asm_fp -> G.M_to_word64Prog.asm_fp = Obj.magic

(* asm_arith: Div_2/Shift_3 <-> Div/Shift *)
let asm_arith_of_gen : G.M_to_word64Prog.asm_arith -> Common.asm_arith = function
  | G.M_to_word64Prog.Suboverflow (a, b, c, d) -> Common.Suboverflow (a, b, c, d)
  | G.M_to_word64Prog.Addoverflow (a, b, c, d) -> Common.Addoverflow (a, b, c, d)
  | G.M_to_word64Prog.Addcarry (a, b, c, d) -> Common.Addcarry (a, b, c, d)
  | G.M_to_word64Prog.Longdiv (a, b, c, d, e) -> Common.Longdiv (a, b, c, d, e)
  | G.M_to_word64Prog.Longmul (a, b, c, d) -> Common.Longmul (a, b, c, d)
  | G.M_to_word64Prog.Div_2 (a, b, c) -> Common.Div (a, b, c)
  | G.M_to_word64Prog.Shift_3 (s, a, b, c) -> Common.Shift (ast_shift_of_gen s, a, b, c)
  | G.M_to_word64Prog.Binop (op, a, b, ri) -> Common.Binop (asm_binop_of_gen op, a, b, asm_reg_imm_of_gen ri)

let asm_arith_to_gen : Common.asm_arith -> G.M_to_word64Prog.asm_arith = function
  | Common.Suboverflow (a, b, c, d) -> G.M_to_word64Prog.Suboverflow (a, b, c, d)
  | Common.Addoverflow (a, b, c, d) -> G.M_to_word64Prog.Addoverflow (a, b, c, d)
  | Common.Addcarry (a, b, c, d) -> G.M_to_word64Prog.Addcarry (a, b, c, d)
  | Common.Longdiv (a, b, c, d, e) -> G.M_to_word64Prog.Longdiv (a, b, c, d, e)
  | Common.Longmul (a, b, c, d) -> G.M_to_word64Prog.Longmul (a, b, c, d)
  | Common.Div (a, b, c) -> G.M_to_word64Prog.Div_2 (a, b, c)
  | Common.Shift (s, a, b, c) -> G.M_to_word64Prog.Shift_3 (ast_shift_to_gen s, a, b, c)
  | Common.Binop (op, a, b, ri) -> G.M_to_word64Prog.Binop (asm_binop_to_gen op, a, b, asm_reg_imm_to_gen ri)

(* asm_inst: Arith_1/Const_3 <-> Arith/Const *)
let asm_inst_of_gen : G.M_to_word64Prog.asm_inst -> Common.asm_inst = function
  | G.M_to_word64Prog.Fp fp -> Common.Fp (asm_fp_of_gen fp)
  | G.M_to_word64Prog.Mem (m, r, a) -> Common.Mem (asm_memop_of_gen m, r, asm_addr_of_gen a)
  | G.M_to_word64Prog.Arith_1 ar -> Common.Arith (asm_arith_of_gen ar)
  | G.M_to_word64Prog.Const_3 (r, w) -> Common.Const (r, w)
  | G.M_to_word64Prog.Skip -> Common.Skip

let asm_inst_to_gen : Common.asm_inst -> G.M_to_word64Prog.asm_inst = function
  | Common.Fp fp -> G.M_to_word64Prog.Fp (asm_fp_to_gen fp)
  | Common.Mem (m, r, a) -> G.M_to_word64Prog.Mem (asm_memop_to_gen m, r, asm_addr_to_gen a)
  | Common.Arith ar -> G.M_to_word64Prog.Arith_1 (asm_arith_to_gen ar)
  | Common.Const (r, w) -> G.M_to_word64Prog.Const_3 (r, w)
  | Common.Skip -> G.M_to_word64Prog.Skip

(* asm_asm: Loc_1/Call_1/Inst_1 <-> Loc/Call/Inst *)
let asm_asm_of_gen : G.M_to_word64Prog.asm_asm -> Common.asm_asm = function
  | G.M_to_word64Prog.Loc_1 (r, w) -> Common.Loc (r, w)
  | G.M_to_word64Prog.Jumpreg r -> Common.Jumpreg r
  | G.M_to_word64Prog.Call_1 w -> Common.Call w
  | G.M_to_word64Prog.Jumpcmp (c, r, ri, w) ->
    Common.Jumpcmp (asm_cmp_of_gen c, r, asm_reg_imm_of_gen ri, w)
  | G.M_to_word64Prog.Jump w -> Common.Jump w
  | G.M_to_word64Prog.Inst_1 i -> Common.Inst (asm_inst_of_gen i)

let asm_asm_to_gen : Common.asm_asm -> G.M_to_word64Prog.asm_asm = function
  | Common.Loc (r, w) -> G.M_to_word64Prog.Loc_1 (r, w)
  | Common.Jumpreg r -> G.M_to_word64Prog.Jumpreg r
  | Common.Call w -> G.M_to_word64Prog.Call_1 w
  | Common.Jumpcmp (c, r, ri, w) ->
    G.M_to_word64Prog.Jumpcmp (asm_cmp_to_gen c, r, asm_reg_imm_to_gen ri, w)
  | Common.Jump w -> G.M_to_word64Prog.Jump w
  | Common.Inst i -> G.M_to_word64Prog.Inst_1 (asm_inst_to_gen i)
