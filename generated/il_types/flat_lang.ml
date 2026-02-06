(* flatLang AST — clean type definitions.
   Corresponds to M_to_flatProg.flatLang_exp/pat/op/dec in cake64.ml
   and flatLangScript.sml in HOL. *)

open Common

(* flatLang patterns *)
(* Clean names: Pref/Pas/Pcon/Plit/Pvar/Pany instead of *_1 *)
type pat =
  | Pref of pat
  | Pas of pat * string
  | Pcon of (Z.t * (Z.t * (Z.t * Z.t) list) option) option * pat list
  | Plit of ast_lit
  | Pvar of string
  | Pany

(* flatLang operations *)
(* Clean names: Ffi/Thunkop/Test/Shift/Arith/... instead of *_1 *)
type op =
  | Thunkop of ast_thunk_op
  | Id
  | El of Z.t
  | Leneq of Z.t
  | Tagleneq of Z.t * Z.t
  | Eval
  | Globalvarlookup of Z.t
  | Globalvarinit of Z.t
  | Globalvaralloc of Z.t
  | Ffi of string
  | Configgc
  | Listappend
  | Aw8xor_unsafe
  | Aw8update_unsafe
  | Aw8sub_unsafe
  | Aupdate_unsafe
  | Asub_unsafe
  | Aupdate
  | Alength
  | Asub
  | Aallocfixed
  | Aalloc
  | Vlength
  | Vsub_unsafe
  | Vsub
  | Vfromlist
  | Strcat
  | Strlen
  | Strsub
  | Explode
  | Implode
  | Chr
  | Ord
  | Copyaw8aw8
  | Copyaw8str
  | Copystraw8
  | Copystrstr
  | Wordtoint of ast_word_size
  | Wordfromint of ast_word_size
  | Aw8update
  | Aw8length
  | Aw8sub
  | Aw8alloc
  | Opref
  | Opassign
  | Opapp
  | Fptoword
  | Fpfromword
  | Fp_top of ast_fp_top
  | Fp_bop of ast_fp_bop
  | Fp_uop of ast_fp_uop
  | Fp_cmp of ast_fp_cmp
  | Test of ast_test * ast_prim_type
  | Equality
  | Shift of ast_word_size * ast_shift * Z.t
  | Opw of ast_word_size * ast_opw
  | Opb of ast_opb
  | Opn of ast_opn
  | Fromto of ast_prim_type * ast_prim_type
  | Arith of ast_temp_arith * ast_prim_type

(* flatLang expressions *)
(* Clean names: Letrec/Let/Mat/If/App/Fun/Con/Lit/Handle/Raise instead of *_1 *)
type exp =
  | Letrec of string * (string * (string * exp)) list * exp
  | Let of backend_common_tra * string option * exp * exp
  | Mat of backend_common_tra * exp * (pat * exp) list
  | If of backend_common_tra * exp * exp * exp
  | App of backend_common_tra * op * exp list
  | Fun of string * string * exp
  | Var_local of backend_common_tra * string
  | Con of backend_common_tra * (Z.t * Z.t option) option * exp list
  | Lit of backend_common_tra * ast_lit
  | Handle of backend_common_tra * exp * (pat * exp) list
  | Raise of backend_common_tra * exp

(* flatLang declarations *)
(* Clean name: Dlet instead of Dlet_1 *)
type dec = Dlet of exp
