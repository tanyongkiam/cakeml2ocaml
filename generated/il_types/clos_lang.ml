(* closLang AST — clean type definitions.
   Corresponds to M_to_closProg.closLang_* in cake64.ml
   and closLangScript.sml in HOL. *)

open Common

(* closLang memory operations *)
(* Clean name: Configgc instead of Configgc_1 *)
type mem_op =
  | Configgc
  | Boundscheckbyte of bool
  | Boundscheckarray
  | Xorbyte
  | Derefbytevec
  | Lengthbytevec
  | Tolistbyte
  | Fromlistbyte
  | Copybyte of bool
  | Concatbytevec
  | Updatebyte
  | Derefbyte
  | Refarray
  | Refbyte of bool
  | Lengthbyte
  | Length
  | El
  | Update
  | Ref

(* closLang global operations *)
type glob_op =
  | Setglobalsptr
  | Globalsptr
  | Allocglobal
  | Setglobal of Z.t
  | Global of Z.t

(* closLang constant parts *)
(* Clean name: Con instead of Con_1 *)
type const_part =
  | W64 of int64
  | Str of string
  | Int of Z.t
  | Con of Z.t * Z.t list

(* closLang constants *)
type const =
  | Constword64 of int64
  | Conststr of string
  | Constint of Z.t
  | Constcons of Z.t * const list

(* closLang block operations *)
(* Clean name: Equal instead of Equal_2; Listappend instead of Listappend_1 *)
type block_op =
  | Build of const_part list
  | Equalconst of const_part
  | Equal
  | Constant of const
  | Listappend
  | Fromlist of Z.t
  | Consextend of Z.t
  | Boundscheckblock
  | Booltest of ast_test
  | Lengthblock
  | Tageq of Z.t
  | Leneq of Z.t
  | Tagleneq of Z.t * Z.t
  | Elemat of Z.t
  | Cons of Z.t

(* closLang word operations *)
(* Clean names: Fp_top/Fp_bop/Fp_uop/Fp_cmp/Wordtoint/Wordfromint instead of *_1 *)
type word_op =
  | Fp_top of ast_fp_top
  | Fp_bop of ast_fp_bop
  | Fp_uop of ast_fp_uop
  | Fp_cmp of ast_fp_cmp
  | Wordfromword of bool
  | Wordtoint
  | Wordfromint
  | Wordtest of ast_word_size * ast_test
  | Wordshift of ast_word_size * ast_shift * Z.t
  | Wordopw of ast_word_size * ast_opw

(* closLang integer operations *)
(* Clean names: Less/Div/Sub/Add/Const instead of Less_1/Div_2/Sub_2/Add_2/Const_2 *)
type int_op =
  | Lessconstsmall of Z.t
  | Greatereq
  | Greater
  | Lesseq
  | Less
  | Mod
  | Div
  | Mult
  | Sub
  | Add
  | Const of Z.t

(* closLang operations *)
(* Clean names: Thunkop/Ffi instead of Thunkop_1/Ffi_1 *)
type op =
  | Thunkop of ast_thunk_op
  | Install
  | Memop of mem_op
  | Globop of glob_op
  | Blockop of block_op
  | Wordop of word_op
  | Intop of int_op
  | Ffi of string
  | Label of Z.t

(* closLang expressions *)
(* Clean names: Letrec/App/Handle/Raise/Let/If/Var instead of *_1/*_3 *)
type exp =
  | Op of backend_common_tra * op * exp list
  | Letrec of string list * Z.t option * Z.t list option * (Z.t * exp) list * exp
  | Fn of string * Z.t option * Z.t list option * Z.t * exp
  | App of backend_common_tra * Z.t option * exp * exp list
  | Call of backend_common_tra * Z.t * Z.t * exp list
  | Tick of backend_common_tra * exp
  | Handle of backend_common_tra * exp * exp
  | Raise of backend_common_tra * exp
  | Let of backend_common_tra * exp list * exp
  | If of backend_common_tra * exp * exp * exp
  | Var of backend_common_tra * Z.t
