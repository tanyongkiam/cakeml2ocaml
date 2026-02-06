(* Conversion functions between Bvl_lang clean types and
   M_to_bvlProg generated types in cake64.ml *)

module G = Cake64

(* bvl_exp uses closLang_op, which is converted by Clos_lang_conv *)

let rec exp_of_gen : G.M_to_bvlProg.bvl_exp -> Bvl_lang.exp = function
  | G.M_to_bvlProg.Op (o, es) -> Bvl_lang.Op (Clos_lang_conv.op_of_gen o, List.map exp_of_gen es)
  | G.M_to_bvlProg.Force (a, b) -> Bvl_lang.Force (a, b)
  | G.M_to_bvlProg.Call (t, d, es) -> Bvl_lang.Call (t, d, List.map exp_of_gen es)
  | G.M_to_bvlProg.Tick e -> Bvl_lang.Tick (exp_of_gen e)
  | G.M_to_bvlProg.Handle_1 (e, h) -> Bvl_lang.Handle (exp_of_gen e, exp_of_gen h)
  | G.M_to_bvlProg.Raise_1 e -> Bvl_lang.Raise (exp_of_gen e)
  | G.M_to_bvlProg.Let_1 (es, e) -> Bvl_lang.Let (List.map exp_of_gen es, exp_of_gen e)
  | G.M_to_bvlProg.If_1 (c, t, f) -> Bvl_lang.If (exp_of_gen c, exp_of_gen t, exp_of_gen f)
  | G.M_to_bvlProg.Var_3 n -> Bvl_lang.Var n

let rec exp_to_gen : Bvl_lang.exp -> G.M_to_bvlProg.bvl_exp = function
  | Bvl_lang.Op (o, es) -> G.M_to_bvlProg.Op (Clos_lang_conv.op_to_gen o, List.map exp_to_gen es)
  | Bvl_lang.Force (a, b) -> G.M_to_bvlProg.Force (a, b)
  | Bvl_lang.Call (t, d, es) -> G.M_to_bvlProg.Call (t, d, List.map exp_to_gen es)
  | Bvl_lang.Tick e -> G.M_to_bvlProg.Tick (exp_to_gen e)
  | Bvl_lang.Handle (e, h) -> G.M_to_bvlProg.Handle_1 (exp_to_gen e, exp_to_gen h)
  | Bvl_lang.Raise e -> G.M_to_bvlProg.Raise_1 (exp_to_gen e)
  | Bvl_lang.Let (es, e) -> G.M_to_bvlProg.Let_1 (List.map exp_to_gen es, exp_to_gen e)
  | Bvl_lang.If (c, t, f) -> G.M_to_bvlProg.If_1 (exp_to_gen c, exp_to_gen t, exp_to_gen f)
  | Bvl_lang.Var n -> G.M_to_bvlProg.Var_3 n
