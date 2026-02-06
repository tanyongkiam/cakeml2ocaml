(* dataLang AST — clean type definitions.
   Corresponds to M_to_dataProg.dataLang_prog in cake64.ml
   and dataLangScript.sml in HOL. *)

(* dataLang reuses closLang operations *)

open Common

(* dataLang programs *)
(* Clean names: Raise/If instead of Raise_1/If_1 *)
type prog =
  | Force of (Z.t * unit sptree_spt) option * Z.t * Z.t
  | Tick
  | Return of Z.t
  | Raise of Z.t
  | Makespace of Z.t * unit sptree_spt
  | If of Z.t * prog * prog
  | Seq of prog * prog
  | Assign of Z.t * Clos_lang.op * Z.t list * unit sptree_spt option
  | Call of (Z.t * unit sptree_spt) option * Z.t option * Z.t list * (Z.t * prog) option
  | Move of Z.t * Z.t
  | Skip
