(* stackLang AST — clean type definitions.
   Corresponds to M_to_target64Prog.stackLang_prog in cake64.ml
   and stackLangScript.sml in HOL. *)

open Common

(* stackLang programs *)
(* Clean names: Skip→Skip (no suffix), Seq/If/Raise/Set/Ffi instead of *_1/*_2 *)
type prog =
  | Halt of Z.t
  | Bitmapload of Z.t * Z.t
  | Stacksetsize of Z.t
  | Stackgetsize of Z.t
  | Stackloadany of Z.t * Z.t
  | Stackload of Z.t * Z.t
  | Stackstoreany of Z.t * Z.t
  | Stackstore of Z.t * Z.t
  | Stackfree of Z.t
  | Stackalloc of Z.t
  | Rawcall of Z.t
  | Databufferwrite of Z.t * Z.t
  | Codebufferwrite of Z.t * Z.t
  | Shmemop of asm_memop * Z.t * asm_addr
  | Install of Z.t * Z.t * Z.t * Z.t * Z.t
  | Locvalue of Z.t * Z.t * Z.t
  | Tick
  | Ffi of string * Z.t * Z.t * Z.t * Z.t * Z.t
  | Return of Z.t
  | Raise of Z.t
  | Storeconsts of Z.t * Z.t * Z.t option
  | Alloc of Z.t
  | Jumplower of Z.t * Z.t * Z.t
  | While of asm_cmp * Z.t * asm_reg_imm * prog
  | If of asm_cmp * Z.t * asm_reg_imm * prog * prog
  | Seq of prog * prog
  | Call of (prog * (Z.t * (Z.t * Z.t))) option
         * (Z.t, Z.t) sum
         * (prog * (Z.t * Z.t)) option
  | Opcurrheap of asm_binop * Z.t * Z.t
  | Set of store_name * Z.t
  | Get of Z.t * store_name
  | Inst of asm_inst
  | Skip
