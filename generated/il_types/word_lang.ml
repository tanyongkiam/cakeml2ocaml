(* wordLang AST — clean type definitions.
   Corresponds to M_to_word64Prog.wordLang_prog / wordLang_exp in cake64.ml
   and wordLangScript.sml in HOL. *)

open Common

(* wordLang expressions *)
(* Clean names: Shift/Load/Var/Const instead of Shift_2/Load_1/Var_3/Const_2 *)
type exp =
  | Shift of ast_shift * exp * Z.t
  | Op of asm_binop * exp list
  | Load of exp
  | Lookup of store_name
  | Var of Z.t
  | Const of int64

(* wordLang word_loc *)
type word_loc =
  | Loc of Z.t * Z.t
  | Word of int64

(* Cutsets: a pair of num_sets *)
type cutsets = unit sptree_spt * unit sptree_spt

(* wordLang programs *)
(* Clean names: Skip/Seq/If/Raise/Set/Store/Ffi instead of Skip_1/Seq_2/If_1/Raise_1/Set_1/Store_1/Ffi_1 *)
type prog =
  | Shareinst of asm_memop * Z.t * exp
  | Ffi of string * Z.t * Z.t * Z.t * Z.t * cutsets
  | Databufferwrite of Z.t * Z.t
  | Codebufferwrite of Z.t * Z.t
  | Install of Z.t * Z.t * Z.t * Z.t * cutsets
  | Locvalue of Z.t * Z.t
  | Opcurrheap of asm_binop * Z.t * Z.t
  | Tick
  | Return of Z.t * Z.t list
  | Raise of Z.t
  | Storeconsts of Z.t * Z.t * Z.t * Z.t * (bool * int64) list
  | Alloc of Z.t * cutsets
  | If of asm_cmp * Z.t * asm_reg_imm * prog * prog
  | Seq of prog * prog
  | Call of (Z.t list * (cutsets * (prog * (Z.t * Z.t)))) option
         * Z.t option * Z.t list
         * (Z.t * (prog * (Z.t * Z.t))) option
  | Mustterminate of prog
  | Store of exp * Z.t
  | Set of store_name * exp
  | Get of Z.t * store_name
  | Assign of Z.t * exp
  | Inst of asm_inst
  | Move of Z.t * (Z.t * Z.t) list
  | Skip
