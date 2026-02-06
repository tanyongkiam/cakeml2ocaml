(* Conversion functions between Flat_lang clean types and
   M_to_flatProg generated types in cake64.ml *)

module G = Cake64
open Common_conv

(* -- flatLang_pat: Pref_1/Pas_1/Pcon_1/Plit_1/Pvar_1/Pany_1 <-> clean -- *)

let rec pat_of_gen : G.M_to_flatProg.flatLang_pat -> Flat_lang.pat = function
  | G.M_to_flatProg.Pref_1 p -> Flat_lang.Pref (pat_of_gen p)
  | G.M_to_flatProg.Pas_1 (p, s) -> Flat_lang.Pas (pat_of_gen p, s)
  | G.M_to_flatProg.Pcon_1 (c, ps) -> Flat_lang.Pcon (c, List.map pat_of_gen ps)
  | G.M_to_flatProg.Plit_1 l -> Flat_lang.Plit (ast_lit_of_gen l)
  | G.M_to_flatProg.Pvar_1 s -> Flat_lang.Pvar s
  | G.M_to_flatProg.Pany_1 -> Flat_lang.Pany

let rec pat_to_gen : Flat_lang.pat -> G.M_to_flatProg.flatLang_pat = function
  | Flat_lang.Pref p -> G.M_to_flatProg.Pref_1 (pat_to_gen p)
  | Flat_lang.Pas (p, s) -> G.M_to_flatProg.Pas_1 (pat_to_gen p, s)
  | Flat_lang.Pcon (c, ps) -> G.M_to_flatProg.Pcon_1 (c, List.map pat_to_gen ps)
  | Flat_lang.Plit l -> G.M_to_flatProg.Plit_1 (ast_lit_to_gen l)
  | Flat_lang.Pvar s -> G.M_to_flatProg.Pvar_1 s
  | Flat_lang.Pany -> G.M_to_flatProg.Pany_1

(* -- flatLang_op: many *_1 suffixes <-> clean -- *)

let op_of_gen : G.M_to_flatProg.flatLang_op -> Flat_lang.op = function
  | G.M_to_flatProg.Thunkop_1 t -> Flat_lang.Thunkop (ast_thunk_op_of_gen t)
  | G.M_to_flatProg.Id -> Flat_lang.Id
  | G.M_to_flatProg.El n -> Flat_lang.El n
  | G.M_to_flatProg.Leneq n -> Flat_lang.Leneq n
  | G.M_to_flatProg.Tagleneq (a, b) -> Flat_lang.Tagleneq (a, b)
  | G.M_to_flatProg.Eval_1 -> Flat_lang.Eval
  | G.M_to_flatProg.Globalvarlookup n -> Flat_lang.Globalvarlookup n
  | G.M_to_flatProg.Globalvarinit n -> Flat_lang.Globalvarinit n
  | G.M_to_flatProg.Globalvaralloc n -> Flat_lang.Globalvaralloc n
  | G.M_to_flatProg.Ffi_1 s -> Flat_lang.Ffi s
  | G.M_to_flatProg.Configgc_1 -> Flat_lang.Configgc
  | G.M_to_flatProg.Listappend_1 -> Flat_lang.Listappend
  | G.M_to_flatProg.Aw8xor_unsafe -> Flat_lang.Aw8xor_unsafe
  | G.M_to_flatProg.Aw8update_unsafe_1 -> Flat_lang.Aw8update_unsafe
  | G.M_to_flatProg.Aw8sub_unsafe_1 -> Flat_lang.Aw8sub_unsafe
  | G.M_to_flatProg.Aupdate_unsafe_1 -> Flat_lang.Aupdate_unsafe
  | G.M_to_flatProg.Asub_unsafe_1 -> Flat_lang.Asub_unsafe
  | G.M_to_flatProg.Aupdate_1 -> Flat_lang.Aupdate
  | G.M_to_flatProg.Alength_1 -> Flat_lang.Alength
  | G.M_to_flatProg.Asub_1 -> Flat_lang.Asub
  | G.M_to_flatProg.Aallocfixed_1 -> Flat_lang.Aallocfixed
  | G.M_to_flatProg.Aalloc_1 -> Flat_lang.Aalloc
  | G.M_to_flatProg.Vlength_1 -> Flat_lang.Vlength
  | G.M_to_flatProg.Vsub_unsafe_1 -> Flat_lang.Vsub_unsafe
  | G.M_to_flatProg.Vsub_1 -> Flat_lang.Vsub
  | G.M_to_flatProg.Vfromlist_1 -> Flat_lang.Vfromlist
  | G.M_to_flatProg.Strcat_1 -> Flat_lang.Strcat
  | G.M_to_flatProg.Strlen_1 -> Flat_lang.Strlen
  | G.M_to_flatProg.Strsub_1 -> Flat_lang.Strsub
  | G.M_to_flatProg.Explode_1 -> Flat_lang.Explode
  | G.M_to_flatProg.Implode_1 -> Flat_lang.Implode
  | G.M_to_flatProg.Chr_2 -> Flat_lang.Chr
  | G.M_to_flatProg.Ord_1 -> Flat_lang.Ord
  | G.M_to_flatProg.Copyaw8aw8_1 -> Flat_lang.Copyaw8aw8
  | G.M_to_flatProg.Copyaw8str_1 -> Flat_lang.Copyaw8str
  | G.M_to_flatProg.Copystraw8_1 -> Flat_lang.Copystraw8
  | G.M_to_flatProg.Copystrstr_1 -> Flat_lang.Copystrstr
  | G.M_to_flatProg.Wordtoint_1 ws -> Flat_lang.Wordtoint (ast_word_size_of_gen ws)
  | G.M_to_flatProg.Wordfromint_1 ws -> Flat_lang.Wordfromint (ast_word_size_of_gen ws)
  | G.M_to_flatProg.Aw8update_1 -> Flat_lang.Aw8update
  | G.M_to_flatProg.Aw8length_1 -> Flat_lang.Aw8length
  | G.M_to_flatProg.Aw8sub_1 -> Flat_lang.Aw8sub
  | G.M_to_flatProg.Aw8alloc_1 -> Flat_lang.Aw8alloc
  | G.M_to_flatProg.Opref_1 -> Flat_lang.Opref
  | G.M_to_flatProg.Opassign_1 -> Flat_lang.Opassign
  | G.M_to_flatProg.Opapp_1 -> Flat_lang.Opapp
  | G.M_to_flatProg.Fptoword_1 -> Flat_lang.Fptoword
  | G.M_to_flatProg.Fpfromword_1 -> Flat_lang.Fpfromword
  | G.M_to_flatProg.Fp_top_1 t -> Flat_lang.Fp_top (ast_fp_top_of_gen t)
  | G.M_to_flatProg.Fp_bop_1 b -> Flat_lang.Fp_bop (ast_fp_bop_of_gen b)
  | G.M_to_flatProg.Fp_uop_1 u -> Flat_lang.Fp_uop (ast_fp_uop_of_gen u)
  | G.M_to_flatProg.Fp_cmp_1 c -> Flat_lang.Fp_cmp (ast_fp_cmp_of_gen c)
  | G.M_to_flatProg.Test_1 (t, p) -> Flat_lang.Test (ast_test_of_gen t, ast_prim_type_of_gen p)
  | G.M_to_flatProg.Equality_1 -> Flat_lang.Equality
  | G.M_to_flatProg.Shift_1 (ws, s, n) -> Flat_lang.Shift (ast_word_size_of_gen ws, ast_shift_of_gen s, n)
  | G.M_to_flatProg.Opw_1 (ws, ow) -> Flat_lang.Opw (ast_word_size_of_gen ws, ast_opw_of_gen ow)
  | G.M_to_flatProg.Opb_1 ob -> Flat_lang.Opb (ast_opb_of_gen ob)
  | G.M_to_flatProg.Opn_1 on_ -> Flat_lang.Opn (ast_opn_of_gen on_)
  | G.M_to_flatProg.Fromto_1 (a, b) -> Flat_lang.Fromto (ast_prim_type_of_gen a, ast_prim_type_of_gen b)
  | G.M_to_flatProg.Arith_1 (ar, p) -> Flat_lang.Arith (ast_temp_arith_of_gen ar, ast_prim_type_of_gen p)

let op_to_gen : Flat_lang.op -> G.M_to_flatProg.flatLang_op = function
  | Flat_lang.Thunkop t -> G.M_to_flatProg.Thunkop_1 (ast_thunk_op_to_gen t)
  | Flat_lang.Id -> G.M_to_flatProg.Id
  | Flat_lang.El n -> G.M_to_flatProg.El n
  | Flat_lang.Leneq n -> G.M_to_flatProg.Leneq n
  | Flat_lang.Tagleneq (a, b) -> G.M_to_flatProg.Tagleneq (a, b)
  | Flat_lang.Eval -> G.M_to_flatProg.Eval_1
  | Flat_lang.Globalvarlookup n -> G.M_to_flatProg.Globalvarlookup n
  | Flat_lang.Globalvarinit n -> G.M_to_flatProg.Globalvarinit n
  | Flat_lang.Globalvaralloc n -> G.M_to_flatProg.Globalvaralloc n
  | Flat_lang.Ffi s -> G.M_to_flatProg.Ffi_1 s
  | Flat_lang.Configgc -> G.M_to_flatProg.Configgc_1
  | Flat_lang.Listappend -> G.M_to_flatProg.Listappend_1
  | Flat_lang.Aw8xor_unsafe -> G.M_to_flatProg.Aw8xor_unsafe
  | Flat_lang.Aw8update_unsafe -> G.M_to_flatProg.Aw8update_unsafe_1
  | Flat_lang.Aw8sub_unsafe -> G.M_to_flatProg.Aw8sub_unsafe_1
  | Flat_lang.Aupdate_unsafe -> G.M_to_flatProg.Aupdate_unsafe_1
  | Flat_lang.Asub_unsafe -> G.M_to_flatProg.Asub_unsafe_1
  | Flat_lang.Aupdate -> G.M_to_flatProg.Aupdate_1
  | Flat_lang.Alength -> G.M_to_flatProg.Alength_1
  | Flat_lang.Asub -> G.M_to_flatProg.Asub_1
  | Flat_lang.Aallocfixed -> G.M_to_flatProg.Aallocfixed_1
  | Flat_lang.Aalloc -> G.M_to_flatProg.Aalloc_1
  | Flat_lang.Vlength -> G.M_to_flatProg.Vlength_1
  | Flat_lang.Vsub_unsafe -> G.M_to_flatProg.Vsub_unsafe_1
  | Flat_lang.Vsub -> G.M_to_flatProg.Vsub_1
  | Flat_lang.Vfromlist -> G.M_to_flatProg.Vfromlist_1
  | Flat_lang.Strcat -> G.M_to_flatProg.Strcat_1
  | Flat_lang.Strlen -> G.M_to_flatProg.Strlen_1
  | Flat_lang.Strsub -> G.M_to_flatProg.Strsub_1
  | Flat_lang.Explode -> G.M_to_flatProg.Explode_1
  | Flat_lang.Implode -> G.M_to_flatProg.Implode_1
  | Flat_lang.Chr -> G.M_to_flatProg.Chr_2
  | Flat_lang.Ord -> G.M_to_flatProg.Ord_1
  | Flat_lang.Copyaw8aw8 -> G.M_to_flatProg.Copyaw8aw8_1
  | Flat_lang.Copyaw8str -> G.M_to_flatProg.Copyaw8str_1
  | Flat_lang.Copystraw8 -> G.M_to_flatProg.Copystraw8_1
  | Flat_lang.Copystrstr -> G.M_to_flatProg.Copystrstr_1
  | Flat_lang.Wordtoint ws -> G.M_to_flatProg.Wordtoint_1 (ast_word_size_to_gen ws)
  | Flat_lang.Wordfromint ws -> G.M_to_flatProg.Wordfromint_1 (ast_word_size_to_gen ws)
  | Flat_lang.Aw8update -> G.M_to_flatProg.Aw8update_1
  | Flat_lang.Aw8length -> G.M_to_flatProg.Aw8length_1
  | Flat_lang.Aw8sub -> G.M_to_flatProg.Aw8sub_1
  | Flat_lang.Aw8alloc -> G.M_to_flatProg.Aw8alloc_1
  | Flat_lang.Opref -> G.M_to_flatProg.Opref_1
  | Flat_lang.Opassign -> G.M_to_flatProg.Opassign_1
  | Flat_lang.Opapp -> G.M_to_flatProg.Opapp_1
  | Flat_lang.Fptoword -> G.M_to_flatProg.Fptoword_1
  | Flat_lang.Fpfromword -> G.M_to_flatProg.Fpfromword_1
  | Flat_lang.Fp_top t -> G.M_to_flatProg.Fp_top_1 (ast_fp_top_to_gen t)
  | Flat_lang.Fp_bop b -> G.M_to_flatProg.Fp_bop_1 (ast_fp_bop_to_gen b)
  | Flat_lang.Fp_uop u -> G.M_to_flatProg.Fp_uop_1 (ast_fp_uop_to_gen u)
  | Flat_lang.Fp_cmp c -> G.M_to_flatProg.Fp_cmp_1 (ast_fp_cmp_to_gen c)
  | Flat_lang.Test (t, p) -> G.M_to_flatProg.Test_1 (ast_test_to_gen t, ast_prim_type_to_gen p)
  | Flat_lang.Equality -> G.M_to_flatProg.Equality_1
  | Flat_lang.Shift (ws, s, n) -> G.M_to_flatProg.Shift_1 (ast_word_size_to_gen ws, ast_shift_to_gen s, n)
  | Flat_lang.Opw (ws, ow) -> G.M_to_flatProg.Opw_1 (ast_word_size_to_gen ws, ast_opw_to_gen ow)
  | Flat_lang.Opb ob -> G.M_to_flatProg.Opb_1 (ast_opb_to_gen ob)
  | Flat_lang.Opn on_ -> G.M_to_flatProg.Opn_1 (ast_opn_to_gen on_)
  | Flat_lang.Fromto (a, b) -> G.M_to_flatProg.Fromto_1 (ast_prim_type_to_gen a, ast_prim_type_to_gen b)
  | Flat_lang.Arith (ar, p) -> G.M_to_flatProg.Arith_1 (ast_temp_arith_to_gen ar, ast_prim_type_to_gen p)

(* -- flatLang_exp: Letrec_1/Let_1/Mat_1/If_1/App_1/Fun_1/Con_1/Lit_1/Handle_1/Raise_1 <-> clean -- *)

let rec exp_of_gen : G.M_to_flatProg.flatLang_exp -> Flat_lang.exp = function
  | G.M_to_flatProg.Letrec_1 (s, fns, e) ->
    Flat_lang.Letrec (s, List.map (fun (a, (b, e)) -> (a, (b, exp_of_gen e))) fns, exp_of_gen e)
  | G.M_to_flatProg.Let_1 (t, n, e1, e2) ->
    Flat_lang.Let (tra_of_gen t, n, exp_of_gen e1, exp_of_gen e2)
  | G.M_to_flatProg.Mat_1 (t, e, pes) ->
    Flat_lang.Mat (tra_of_gen t, exp_of_gen e, List.map (fun (p, e) -> (pat_of_gen p, exp_of_gen e)) pes)
  | G.M_to_flatProg.If_1 (t, c, th, el) ->
    Flat_lang.If (tra_of_gen t, exp_of_gen c, exp_of_gen th, exp_of_gen el)
  | G.M_to_flatProg.App_1 (t, o, es) ->
    Flat_lang.App (tra_of_gen t, op_of_gen o, List.map exp_of_gen es)
  | G.M_to_flatProg.Fun_1 (a, b, e) -> Flat_lang.Fun (a, b, exp_of_gen e)
  | G.M_to_flatProg.Var_local (t, s) -> Flat_lang.Var_local (tra_of_gen t, s)
  | G.M_to_flatProg.Con_1 (t, c, es) ->
    Flat_lang.Con (tra_of_gen t, c, List.map exp_of_gen es)
  | G.M_to_flatProg.Lit_1 (t, l) -> Flat_lang.Lit (tra_of_gen t, ast_lit_of_gen l)
  | G.M_to_flatProg.Handle_1 (t, e, pes) ->
    Flat_lang.Handle (tra_of_gen t, exp_of_gen e, List.map (fun (p, e) -> (pat_of_gen p, exp_of_gen e)) pes)
  | G.M_to_flatProg.Raise_1 (t, e) -> Flat_lang.Raise (tra_of_gen t, exp_of_gen e)

let rec exp_to_gen : Flat_lang.exp -> G.M_to_flatProg.flatLang_exp = function
  | Flat_lang.Letrec (s, fns, e) ->
    G.M_to_flatProg.Letrec_1 (s, List.map (fun (a, (b, e)) -> (a, (b, exp_to_gen e))) fns, exp_to_gen e)
  | Flat_lang.Let (t, n, e1, e2) ->
    G.M_to_flatProg.Let_1 (tra_to_gen t, n, exp_to_gen e1, exp_to_gen e2)
  | Flat_lang.Mat (t, e, pes) ->
    G.M_to_flatProg.Mat_1 (tra_to_gen t, exp_to_gen e, List.map (fun (p, e) -> (pat_to_gen p, exp_to_gen e)) pes)
  | Flat_lang.If (t, c, th, el) ->
    G.M_to_flatProg.If_1 (tra_to_gen t, exp_to_gen c, exp_to_gen th, exp_to_gen el)
  | Flat_lang.App (t, o, es) ->
    G.M_to_flatProg.App_1 (tra_to_gen t, op_to_gen o, List.map exp_to_gen es)
  | Flat_lang.Fun (a, b, e) -> G.M_to_flatProg.Fun_1 (a, b, exp_to_gen e)
  | Flat_lang.Var_local (t, s) -> G.M_to_flatProg.Var_local (tra_to_gen t, s)
  | Flat_lang.Con (t, c, es) ->
    G.M_to_flatProg.Con_1 (tra_to_gen t, c, List.map exp_to_gen es)
  | Flat_lang.Lit (t, l) -> G.M_to_flatProg.Lit_1 (tra_to_gen t, ast_lit_to_gen l)
  | Flat_lang.Handle (t, e, pes) ->
    G.M_to_flatProg.Handle_1 (tra_to_gen t, exp_to_gen e, List.map (fun (p, e) -> (pat_to_gen p, exp_to_gen e)) pes)
  | Flat_lang.Raise (t, e) -> G.M_to_flatProg.Raise_1 (tra_to_gen t, exp_to_gen e)

(* -- flatLang_dec: Dlet_1 <-> Dlet -- *)

let dec_of_gen : G.M_to_flatProg.flatLang_dec -> Flat_lang.dec = function
  | G.M_to_flatProg.Dlet_1 e -> Flat_lang.Dlet (exp_of_gen e)

let dec_to_gen : Flat_lang.dec -> G.M_to_flatProg.flatLang_dec = function
  | Flat_lang.Dlet e -> G.M_to_flatProg.Dlet_1 (exp_to_gen e)
