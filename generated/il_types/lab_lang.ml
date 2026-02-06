(* labLang AST — clean type definitions.
   Corresponds to M_to_target64Prog.labLang_* in cake64.ml
   and labLangScript.sml in HOL. *)

open Common

(* labLang labels *)
type lab = Lab of Z.t * Z.t

(* labLang assembly with labels *)
(* Clean names: Halt/Install/Call/Locvalue instead of Halt_1/Install_1/Call_1/Locvalue_1 *)
type asm_with_lab =
  | Halt
  | Install
  | Callffi of string
  | Locvalue of Z.t * lab
  | Call of lab
  | Jumpcmp of asm_cmp * Z.t * asm_reg_imm * lab
  | Jump of lab

(* labLang assembly or code-buffer-write *)
type asm_or_cbw =
  | Sharemem of asm_memop * Z.t * asm_addr
  | Cbw of Z.t * Z.t
  | Asmi of asm_asm

(* labLang lines *)
type line =
  | Labasm of asm_with_lab * int64 * int list * Z.t
  | Asm of asm_or_cbw * int list * Z.t
  | Label of Z.t * Z.t * Z.t

(* labLang sections *)
type sec = Section of Z.t * line list
