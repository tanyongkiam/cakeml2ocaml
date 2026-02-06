(* bviLang AST — clean type definitions.
   Corresponds to M_to_bviProg.bvi_exp in cake64.ml
   and bviScript.sml in HOL. *)

(* BVI reuses closLang operations *)

(* bvi expressions *)
(* Clean names: Raise/Let/If/Var instead of Raise_1/Let_1/If_1/Var_3 *)
type exp =
  | Op of Clos_lang.op * exp list
  | Force of Z.t * Z.t
  | Call of Z.t * Z.t option * exp list * exp option
  | Tick of exp
  | Raise of exp
  | Let of exp list * exp
  | If of exp * exp * exp
  | Var of Z.t

(* bvi tail-recursion associative operation *)
(* Clean names: Append/Times/Plus instead of Append_1/Times_1/Plus_1 *)
type tailrec_assoc_op =
  | Noop
  | Append
  | Times
  | Plus
