(* bvlLang AST — clean type definitions.
   Corresponds to M_to_bvlProg.bvl_exp in cake64.ml
   and bvlScript.sml in HOL. *)

(* BVL reuses closLang operations *)

(* bvl expressions *)
(* Clean names: Handle/Raise/Let/If/Var instead of Handle_1/Raise_1/Let_1/If_1/Var_3 *)
type exp =
  | Op of Clos_lang.op * exp list
  | Force of Z.t * Z.t
  | Call of Z.t * Z.t option * exp list
  | Tick of exp
  | Handle of exp * exp
  | Raise of exp
  | Let of exp list * exp
  | If of exp * exp * exp
  | Var of Z.t
