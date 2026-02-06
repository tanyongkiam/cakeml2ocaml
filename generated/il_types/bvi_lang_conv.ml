(* Conversion functions between Bvi_lang clean types and
   M_to_bviProg generated types in cake64.ml *)

module G = Cake64

(* bvi_exp uses closLang_op, which is converted by Clos_lang_conv *)

let rec exp_of_gen : G.M_to_bviProg.bvi_exp -> Bvi_lang.exp = function
  | G.M_to_bviProg.Op (o, es) -> Bvi_lang.Op (Clos_lang_conv.op_of_gen o, List.map exp_of_gen es)
  | G.M_to_bviProg.Force (a, b) -> Bvi_lang.Force (a, b)
  | G.M_to_bviProg.Call (t, d, es, h) ->
    Bvi_lang.Call (t, d, List.map exp_of_gen es, Option.map exp_of_gen h)
  | G.M_to_bviProg.Tick e -> Bvi_lang.Tick (exp_of_gen e)
  | G.M_to_bviProg.Raise_1 e -> Bvi_lang.Raise (exp_of_gen e)
  | G.M_to_bviProg.Let_1 (es, e) -> Bvi_lang.Let (List.map exp_of_gen es, exp_of_gen e)
  | G.M_to_bviProg.If_1 (c, t, f) -> Bvi_lang.If (exp_of_gen c, exp_of_gen t, exp_of_gen f)
  | G.M_to_bviProg.Var_3 n -> Bvi_lang.Var n

let rec exp_to_gen : Bvi_lang.exp -> G.M_to_bviProg.bvi_exp = function
  | Bvi_lang.Op (o, es) -> G.M_to_bviProg.Op (Clos_lang_conv.op_to_gen o, List.map exp_to_gen es)
  | Bvi_lang.Force (a, b) -> G.M_to_bviProg.Force (a, b)
  | Bvi_lang.Call (t, d, es, h) ->
    G.M_to_bviProg.Call (t, d, List.map exp_to_gen es, Option.map exp_to_gen h)
  | Bvi_lang.Tick e -> G.M_to_bviProg.Tick (exp_to_gen e)
  | Bvi_lang.Raise e -> G.M_to_bviProg.Raise_1 (exp_to_gen e)
  | Bvi_lang.Let (es, e) -> G.M_to_bviProg.Let_1 (List.map exp_to_gen es, exp_to_gen e)
  | Bvi_lang.If (c, t, f) -> G.M_to_bviProg.If_1 (exp_to_gen c, exp_to_gen t, exp_to_gen f)
  | Bvi_lang.Var n -> G.M_to_bviProg.Var_3 n
