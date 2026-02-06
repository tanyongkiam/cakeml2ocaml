(* Conversion functions between Clos_lang clean types and
   M_to_closProg generated types in cake64.ml *)

module G = Cake64
open Common_conv

(* -- closLang_mem_op: Configgc_1 <-> Configgc -- *)
(* Only Configgc has a suffix; rest are identical *)

let mem_op_of_gen : G.M_to_closProg.closLang_mem_op -> Clos_lang.mem_op = function
  | G.M_to_closProg.Configgc_1 -> Clos_lang.Configgc
  | G.M_to_closProg.Boundscheckbyte b -> Clos_lang.Boundscheckbyte b
  | G.M_to_closProg.Boundscheckarray -> Clos_lang.Boundscheckarray
  | G.M_to_closProg.Xorbyte -> Clos_lang.Xorbyte
  | G.M_to_closProg.Derefbytevec -> Clos_lang.Derefbytevec
  | G.M_to_closProg.Lengthbytevec -> Clos_lang.Lengthbytevec
  | G.M_to_closProg.Tolistbyte -> Clos_lang.Tolistbyte
  | G.M_to_closProg.Fromlistbyte -> Clos_lang.Fromlistbyte
  | G.M_to_closProg.Copybyte b -> Clos_lang.Copybyte b
  | G.M_to_closProg.Concatbytevec -> Clos_lang.Concatbytevec
  | G.M_to_closProg.Updatebyte -> Clos_lang.Updatebyte
  | G.M_to_closProg.Derefbyte -> Clos_lang.Derefbyte
  | G.M_to_closProg.Refarray -> Clos_lang.Refarray
  | G.M_to_closProg.Refbyte b -> Clos_lang.Refbyte b
  | G.M_to_closProg.Lengthbyte -> Clos_lang.Lengthbyte
  | G.M_to_closProg.Length -> Clos_lang.Length
  | G.M_to_closProg.El -> Clos_lang.El
  | G.M_to_closProg.Update -> Clos_lang.Update
  | G.M_to_closProg.Ref -> Clos_lang.Ref

let mem_op_to_gen : Clos_lang.mem_op -> G.M_to_closProg.closLang_mem_op = function
  | Clos_lang.Configgc -> G.M_to_closProg.Configgc_1
  | Clos_lang.Boundscheckbyte b -> G.M_to_closProg.Boundscheckbyte b
  | Clos_lang.Boundscheckarray -> G.M_to_closProg.Boundscheckarray
  | Clos_lang.Xorbyte -> G.M_to_closProg.Xorbyte
  | Clos_lang.Derefbytevec -> G.M_to_closProg.Derefbytevec
  | Clos_lang.Lengthbytevec -> G.M_to_closProg.Lengthbytevec
  | Clos_lang.Tolistbyte -> G.M_to_closProg.Tolistbyte
  | Clos_lang.Fromlistbyte -> G.M_to_closProg.Fromlistbyte
  | Clos_lang.Copybyte b -> G.M_to_closProg.Copybyte b
  | Clos_lang.Concatbytevec -> G.M_to_closProg.Concatbytevec
  | Clos_lang.Updatebyte -> G.M_to_closProg.Updatebyte
  | Clos_lang.Derefbyte -> G.M_to_closProg.Derefbyte
  | Clos_lang.Refarray -> G.M_to_closProg.Refarray
  | Clos_lang.Refbyte b -> G.M_to_closProg.Refbyte b
  | Clos_lang.Lengthbyte -> G.M_to_closProg.Lengthbyte
  | Clos_lang.Length -> G.M_to_closProg.Length
  | Clos_lang.El -> G.M_to_closProg.El
  | Clos_lang.Update -> G.M_to_closProg.Update
  | Clos_lang.Ref -> G.M_to_closProg.Ref

(* -- closLang_glob_op: identical -- *)
let glob_op_of_gen : G.M_to_closProg.closLang_glob_op -> Clos_lang.glob_op = Obj.magic
let glob_op_to_gen : Clos_lang.glob_op -> G.M_to_closProg.closLang_glob_op = Obj.magic

(* -- closLang_const_part: Con_1 <-> Con -- *)
let const_part_of_gen : G.M_to_closProg.closLang_const_part -> Clos_lang.const_part = function
  | G.M_to_closProg.W64_1 w -> Clos_lang.W64 w
  | G.M_to_closProg.Str s -> Clos_lang.Str s
  | G.M_to_closProg.Int n -> Clos_lang.Int n
  | G.M_to_closProg.Con_1 (t, ns) -> Clos_lang.Con (t, ns)

let const_part_to_gen : Clos_lang.const_part -> G.M_to_closProg.closLang_const_part = function
  | Clos_lang.W64 w -> G.M_to_closProg.W64_1 w
  | Clos_lang.Str s -> G.M_to_closProg.Str s
  | Clos_lang.Int n -> G.M_to_closProg.Int n
  | Clos_lang.Con (t, ns) -> G.M_to_closProg.Con_1 (t, ns)

(* -- closLang_const: identical -- *)
let const_of_gen : G.M_to_closProg.closLang_const -> Clos_lang.const = Obj.magic
let const_to_gen : Clos_lang.const -> G.M_to_closProg.closLang_const = Obj.magic

(* -- closLang_block_op: Equal_2/Listappend_1 <-> Equal/Listappend -- *)
let block_op_of_gen : G.M_to_closProg.closLang_block_op -> Clos_lang.block_op = function
  | G.M_to_closProg.Build ps -> Clos_lang.Build (List.map const_part_of_gen ps)
  | G.M_to_closProg.Equalconst p -> Clos_lang.Equalconst (const_part_of_gen p)
  | G.M_to_closProg.Equal_2 -> Clos_lang.Equal
  | G.M_to_closProg.Constant c -> Clos_lang.Constant (const_of_gen c)
  | G.M_to_closProg.Listappend_1 -> Clos_lang.Listappend
  | G.M_to_closProg.Fromlist n -> Clos_lang.Fromlist n
  | G.M_to_closProg.Consextend n -> Clos_lang.Consextend n
  | G.M_to_closProg.Boundscheckblock -> Clos_lang.Boundscheckblock
  | G.M_to_closProg.Booltest t -> Clos_lang.Booltest (ast_test_of_gen t)
  | G.M_to_closProg.Lengthblock -> Clos_lang.Lengthblock
  | G.M_to_closProg.Tageq n -> Clos_lang.Tageq n
  | G.M_to_closProg.Leneq n -> Clos_lang.Leneq n
  | G.M_to_closProg.Tagleneq (t, l) -> Clos_lang.Tagleneq (t, l)
  | G.M_to_closProg.Elemat n -> Clos_lang.Elemat n
  | G.M_to_closProg.Cons n -> Clos_lang.Cons n

let block_op_to_gen : Clos_lang.block_op -> G.M_to_closProg.closLang_block_op = function
  | Clos_lang.Build ps -> G.M_to_closProg.Build (List.map const_part_to_gen ps)
  | Clos_lang.Equalconst p -> G.M_to_closProg.Equalconst (const_part_to_gen p)
  | Clos_lang.Equal -> G.M_to_closProg.Equal_2
  | Clos_lang.Constant c -> G.M_to_closProg.Constant (const_to_gen c)
  | Clos_lang.Listappend -> G.M_to_closProg.Listappend_1
  | Clos_lang.Fromlist n -> G.M_to_closProg.Fromlist n
  | Clos_lang.Consextend n -> G.M_to_closProg.Consextend n
  | Clos_lang.Boundscheckblock -> G.M_to_closProg.Boundscheckblock
  | Clos_lang.Booltest t -> G.M_to_closProg.Booltest (ast_test_to_gen t)
  | Clos_lang.Lengthblock -> G.M_to_closProg.Lengthblock
  | Clos_lang.Tageq n -> G.M_to_closProg.Tageq n
  | Clos_lang.Leneq n -> G.M_to_closProg.Leneq n
  | Clos_lang.Tagleneq (t, l) -> G.M_to_closProg.Tagleneq (t, l)
  | Clos_lang.Elemat n -> G.M_to_closProg.Elemat n
  | Clos_lang.Cons n -> G.M_to_closProg.Cons n

(* -- closLang_word_op: Fp_*_1/Wordtoint_1/Wordfromint_1 <-> clean -- *)
let word_op_of_gen : G.M_to_closProg.closLang_word_op -> Clos_lang.word_op = function
  | G.M_to_closProg.Fp_top_1 t -> Clos_lang.Fp_top (ast_fp_top_of_gen t)
  | G.M_to_closProg.Fp_bop_1 b -> Clos_lang.Fp_bop (ast_fp_bop_of_gen b)
  | G.M_to_closProg.Fp_uop_1 u -> Clos_lang.Fp_uop (ast_fp_uop_of_gen u)
  | G.M_to_closProg.Fp_cmp_1 c -> Clos_lang.Fp_cmp (ast_fp_cmp_of_gen c)
  | G.M_to_closProg.Wordfromword b -> Clos_lang.Wordfromword b
  | G.M_to_closProg.Wordtoint_1 -> Clos_lang.Wordtoint
  | G.M_to_closProg.Wordfromint_1 -> Clos_lang.Wordfromint
  | G.M_to_closProg.Wordtest (ws, t) -> Clos_lang.Wordtest (ast_word_size_of_gen ws, ast_test_of_gen t)
  | G.M_to_closProg.Wordshift (ws, s, n) -> Clos_lang.Wordshift (ast_word_size_of_gen ws, ast_shift_of_gen s, n)
  | G.M_to_closProg.Wordopw (ws, ow) -> Clos_lang.Wordopw (ast_word_size_of_gen ws, ast_opw_of_gen ow)

let word_op_to_gen : Clos_lang.word_op -> G.M_to_closProg.closLang_word_op = function
  | Clos_lang.Fp_top t -> G.M_to_closProg.Fp_top_1 (ast_fp_top_to_gen t)
  | Clos_lang.Fp_bop b -> G.M_to_closProg.Fp_bop_1 (ast_fp_bop_to_gen b)
  | Clos_lang.Fp_uop u -> G.M_to_closProg.Fp_uop_1 (ast_fp_uop_to_gen u)
  | Clos_lang.Fp_cmp c -> G.M_to_closProg.Fp_cmp_1 (ast_fp_cmp_to_gen c)
  | Clos_lang.Wordfromword b -> G.M_to_closProg.Wordfromword b
  | Clos_lang.Wordtoint -> G.M_to_closProg.Wordtoint_1
  | Clos_lang.Wordfromint -> G.M_to_closProg.Wordfromint_1
  | Clos_lang.Wordtest (ws, t) -> G.M_to_closProg.Wordtest (ast_word_size_to_gen ws, ast_test_to_gen t)
  | Clos_lang.Wordshift (ws, s, n) -> G.M_to_closProg.Wordshift (ast_word_size_to_gen ws, ast_shift_to_gen s, n)
  | Clos_lang.Wordopw (ws, ow) -> G.M_to_closProg.Wordopw (ast_word_size_to_gen ws, ast_opw_to_gen ow)

(* -- closLang_int_op: Less_1/Div_2/Sub_2/Add_2/Const_2/Greater_1/Mod_1 <-> clean -- *)
let int_op_of_gen : G.M_to_closProg.closLang_int_op -> Clos_lang.int_op = function
  | G.M_to_closProg.Lessconstsmall n -> Clos_lang.Lessconstsmall n
  | G.M_to_closProg.Greatereq -> Clos_lang.Greatereq
  | G.M_to_closProg.Greater_1 -> Clos_lang.Greater
  | G.M_to_closProg.Lesseq -> Clos_lang.Lesseq
  | G.M_to_closProg.Less_1 -> Clos_lang.Less
  | G.M_to_closProg.Mod_1 -> Clos_lang.Mod
  | G.M_to_closProg.Div_2 -> Clos_lang.Div
  | G.M_to_closProg.Mult -> Clos_lang.Mult
  | G.M_to_closProg.Sub_2 -> Clos_lang.Sub
  | G.M_to_closProg.Add_2 -> Clos_lang.Add
  | G.M_to_closProg.Const_2 n -> Clos_lang.Const n

let int_op_to_gen : Clos_lang.int_op -> G.M_to_closProg.closLang_int_op = function
  | Clos_lang.Lessconstsmall n -> G.M_to_closProg.Lessconstsmall n
  | Clos_lang.Greatereq -> G.M_to_closProg.Greatereq
  | Clos_lang.Greater -> G.M_to_closProg.Greater_1
  | Clos_lang.Lesseq -> G.M_to_closProg.Lesseq
  | Clos_lang.Less -> G.M_to_closProg.Less_1
  | Clos_lang.Mod -> G.M_to_closProg.Mod_1
  | Clos_lang.Div -> G.M_to_closProg.Div_2
  | Clos_lang.Mult -> G.M_to_closProg.Mult
  | Clos_lang.Sub -> G.M_to_closProg.Sub_2
  | Clos_lang.Add -> G.M_to_closProg.Add_2
  | Clos_lang.Const n -> G.M_to_closProg.Const_2 n

(* -- closLang_op: Thunkop_1/Ffi_1 <-> Thunkop/Ffi -- *)
let op_of_gen : G.M_to_closProg.closLang_op -> Clos_lang.op = function
  | G.M_to_closProg.Thunkop_1 t -> Clos_lang.Thunkop (ast_thunk_op_of_gen t)
  | G.M_to_closProg.Install -> Clos_lang.Install
  | G.M_to_closProg.Memop m -> Clos_lang.Memop (mem_op_of_gen m)
  | G.M_to_closProg.Globop g -> Clos_lang.Globop (glob_op_of_gen g)
  | G.M_to_closProg.Blockop b -> Clos_lang.Blockop (block_op_of_gen b)
  | G.M_to_closProg.Wordop w -> Clos_lang.Wordop (word_op_of_gen w)
  | G.M_to_closProg.Intop i -> Clos_lang.Intop (int_op_of_gen i)
  | G.M_to_closProg.Ffi_1 s -> Clos_lang.Ffi s
  | G.M_to_closProg.Label n -> Clos_lang.Label n

let op_to_gen : Clos_lang.op -> G.M_to_closProg.closLang_op = function
  | Clos_lang.Thunkop t -> G.M_to_closProg.Thunkop_1 (ast_thunk_op_to_gen t)
  | Clos_lang.Install -> G.M_to_closProg.Install
  | Clos_lang.Memop m -> G.M_to_closProg.Memop (mem_op_to_gen m)
  | Clos_lang.Globop g -> G.M_to_closProg.Globop (glob_op_to_gen g)
  | Clos_lang.Blockop b -> G.M_to_closProg.Blockop (block_op_to_gen b)
  | Clos_lang.Wordop w -> G.M_to_closProg.Wordop (word_op_to_gen w)
  | Clos_lang.Intop i -> G.M_to_closProg.Intop (int_op_to_gen i)
  | Clos_lang.Ffi s -> G.M_to_closProg.Ffi_1 s
  | Clos_lang.Label n -> G.M_to_closProg.Label n

(* -- closLang_exp: Letrec_1/App_1/Handle_1/Raise_1/Let_1/If_1/Var_3 <-> clean -- *)
let rec exp_of_gen : G.M_to_closProg.closLang_exp -> Clos_lang.exp = function
  | G.M_to_closProg.Op (t, o, es) ->
    Clos_lang.Op (tra_of_gen t, op_of_gen o, List.map exp_of_gen es)
  | G.M_to_closProg.Letrec_1 (ns, loc, env, fns, e) ->
    Clos_lang.Letrec (ns, loc, env, List.map (fun (n, e) -> (n, exp_of_gen e)) fns, exp_of_gen e)
  | G.M_to_closProg.Fn (n, loc, env, arity, e) ->
    Clos_lang.Fn (n, loc, env, arity, exp_of_gen e)
  | G.M_to_closProg.App_1 (t, loc, f, args) ->
    Clos_lang.App (tra_of_gen t, loc, exp_of_gen f, List.map exp_of_gen args)
  | G.M_to_closProg.Call (t, ticks, dest, args) ->
    Clos_lang.Call (tra_of_gen t, ticks, dest, List.map exp_of_gen args)
  | G.M_to_closProg.Tick (t, e) -> Clos_lang.Tick (tra_of_gen t, exp_of_gen e)
  | G.M_to_closProg.Handle_1 (t, e, h) ->
    Clos_lang.Handle (tra_of_gen t, exp_of_gen e, exp_of_gen h)
  | G.M_to_closProg.Raise_1 (t, e) -> Clos_lang.Raise (tra_of_gen t, exp_of_gen e)
  | G.M_to_closProg.Let_1 (t, es, e) ->
    Clos_lang.Let (tra_of_gen t, List.map exp_of_gen es, exp_of_gen e)
  | G.M_to_closProg.If_1 (t, c, th, el) ->
    Clos_lang.If (tra_of_gen t, exp_of_gen c, exp_of_gen th, exp_of_gen el)
  | G.M_to_closProg.Var_3 (t, n) -> Clos_lang.Var (tra_of_gen t, n)

let rec exp_to_gen : Clos_lang.exp -> G.M_to_closProg.closLang_exp = function
  | Clos_lang.Op (t, o, es) ->
    G.M_to_closProg.Op (tra_to_gen t, op_to_gen o, List.map exp_to_gen es)
  | Clos_lang.Letrec (ns, loc, env, fns, e) ->
    G.M_to_closProg.Letrec_1 (ns, loc, env, List.map (fun (n, e) -> (n, exp_to_gen e)) fns, exp_to_gen e)
  | Clos_lang.Fn (n, loc, env, arity, e) ->
    G.M_to_closProg.Fn (n, loc, env, arity, exp_to_gen e)
  | Clos_lang.App (t, loc, f, args) ->
    G.M_to_closProg.App_1 (tra_to_gen t, loc, exp_to_gen f, List.map exp_to_gen args)
  | Clos_lang.Call (t, ticks, dest, args) ->
    G.M_to_closProg.Call (tra_to_gen t, ticks, dest, List.map exp_to_gen args)
  | Clos_lang.Tick (t, e) -> G.M_to_closProg.Tick (tra_to_gen t, exp_to_gen e)
  | Clos_lang.Handle (t, e, h) ->
    G.M_to_closProg.Handle_1 (tra_to_gen t, exp_to_gen e, exp_to_gen h)
  | Clos_lang.Raise (t, e) -> G.M_to_closProg.Raise_1 (tra_to_gen t, exp_to_gen e)
  | Clos_lang.Let (t, es, e) ->
    G.M_to_closProg.Let_1 (tra_to_gen t, List.map exp_to_gen es, exp_to_gen e)
  | Clos_lang.If (t, c, th, el) ->
    G.M_to_closProg.If_1 (tra_to_gen t, exp_to_gen c, exp_to_gen th, exp_to_gen el)
  | Clos_lang.Var (t, n) -> G.M_to_closProg.Var_3 (tra_to_gen t, n)
