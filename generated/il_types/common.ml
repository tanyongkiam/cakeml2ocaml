(* Common types shared across CakeML compiler ILs.
   These mirror the types in cake64.ml but with clean constructor names
   where needed. Types that don't need renaming are defined identically. *)

(* -- Sum type (matches generated code's Inl/Inr) -- *)

type ('a, 'b) sum = Inl of 'a | Inr of 'b

(* -- SPTree (from sptreeTheory) -- *)

type 'a sptree_spt =
  | Bs of 'a sptree_spt * 'a * 'a sptree_spt
  | Bn of 'a sptree_spt * 'a sptree_spt
  | Ls of 'a
  | Ln

(* -- AST literals (from astScript.sml) -- *)

type ast_lit =
  | Float64 of int64
  | Word64 of int64
  | Word8 of int
  | Strlit of string
  | Char of char
  | Intlit of Z.t

(* -- AST shift (from asmScript.sml) -- *)

type ast_shift = Ror | Asr | Lsr | Lsl

(* -- AST numeric/comparison ops (from astScript.sml) -- *)

type ast_opn = Modulo | Divide | Times | Minus | Plus

type ast_opb = Geq | Leq | Gt | Lt

type ast_opw = Sub | Add | Xor | Orw | Andw

type ast_word_size = W64 | W8

(* -- AST floating-point ops -- *)

type ast_fp_uop = Fp_sqrt | Fp_neg | Fp_abs

type ast_fp_bop = Fp_div | Fp_mul | Fp_sub | Fp_add

type ast_fp_top = Fp_fma

type ast_fp_cmp = Fp_equal | Fp_greaterequal | Fp_greater | Fp_lessequal | Fp_less

(* -- Thunk ops -- *)

type ast_thunk_mode = Notevaluated | Evaluated

type ast_thunk_op =
  | Forcethunk
  | Updatethunk of ast_thunk_mode
  | Allocthunk of ast_thunk_mode

(* -- AST test -- *)
(* Clean name: Equal instead of Equal_1 *)

type ast_test =
  | Altcompare of ast_opb
  | Compare of ast_opb
  | Equal

(* -- AST prim_type -- *)

type ast_prim_type =
  | Float64t
  | Wordt of ast_word_size
  | Strt
  | Chart
  | Intt
  | Boolt

(* -- AST temp_arith -- *)
(* Clean names: Add/Sub/Or/And/Xor/Div/Abs instead of *_1/*_2 *)

type ast_temp_arith =
  | Fma
  | Sqrt
  | Abs
  | Not
  | Or
  | Xor
  | And
  | Neg
  | Mod
  | Div
  | Mul
  | Sub
  | Add

(* -- Namespace (from namespaceTheory) -- *)

type ('m, 'n) namespace_id =
  | Long of 'm * ('m, 'n) namespace_id
  | Short of 'n

type ('m, 'n, 'w) namespace_namespace =
  | Bind of ('n * 'w) list * ('m * ('m, 'n, 'w) namespace_namespace) list

(* -- Backend common trace (from backend_commonScript.sml) -- *)
(* Clean name: None instead of None_1 *)

type backend_common_tra =
  | None
  | Union of backend_common_tra * backend_common_tra
  | Cons of backend_common_tra * Z.t
  | Sourceloc of Z.t * Z.t * Z.t * Z.t

(* -- ASM types (from asmScript.sml) -- *)

type asm_memop = Store32 | Store16 | Store8 | Store | Load32 | Load16 | Load8 | Load

(* Clean names: Add/Sub/And/Or/Xor instead of *_2 *)
type asm_binop = Xor | Or | And | Sub | Add

(* Clean names: Equal/Less/Test instead of *_1/*_2 *)
type asm_cmp = Nottest | Notless | Notlower | Notequal | Test | Less | Lower | Equal

type asm_reg_imm = Imm of int64 | Reg of Z.t

type asm_addr = Addr of Z.t * int64

type asm_fp =
  | Fpfromint of Z.t * Z.t
  | Fptoint of Z.t * Z.t
  | Fpmovfromreg of Z.t * Z.t * Z.t
  | Fpmovtoreg of Z.t * Z.t * Z.t
  | Fpmov of Z.t * Z.t
  | Fpfma of Z.t * Z.t * Z.t
  | Fpdiv of Z.t * Z.t * Z.t
  | Fpmul of Z.t * Z.t * Z.t
  | Fpsub of Z.t * Z.t * Z.t
  | Fpadd of Z.t * Z.t * Z.t
  | Fpsqrt of Z.t * Z.t
  | Fpneg of Z.t * Z.t
  | Fpabs of Z.t * Z.t
  | Fpequal of Z.t * Z.t * Z.t
  | Fplessequal of Z.t * Z.t * Z.t
  | Fpless of Z.t * Z.t * Z.t

(* Clean names: Arith/Const/Div/Shift instead of *_1/*_2/*_3 *)
type asm_arith =
  | Suboverflow of Z.t * Z.t * Z.t * Z.t
  | Addoverflow of Z.t * Z.t * Z.t * Z.t
  | Addcarry of Z.t * Z.t * Z.t * Z.t
  | Longdiv of Z.t * Z.t * Z.t * Z.t * Z.t
  | Longmul of Z.t * Z.t * Z.t * Z.t
  | Div of Z.t * Z.t * Z.t
  | Shift of ast_shift * Z.t * Z.t * asm_reg_imm
  | Binop of asm_binop * Z.t * Z.t * asm_reg_imm

type asm_inst =
  | Fp of asm_fp
  | Mem of asm_memop * Z.t * asm_addr
  | Arith of asm_arith
  | Const of Z.t * int64
  | Skip

(* -- ASM instruction (from asmScript.sml) -- *)
(* Clean names: Loc/Call/Inst instead of *_1 *)

type asm_asm =
  | Loc of Z.t * int64
  | Jumpreg of Z.t
  | Call of int64
  | Jumpcmp of asm_cmp * Z.t * asm_reg_imm * int64
  | Jump of int64
  | Inst of asm_inst

(* -- Store names (used by both wordLang and stackLang) -- *)

type store_name =
  | Temp of int
  | Bitmapbufferend
  | Bitmapbuffer
  | Codebufferend
  | Codebuffer
  | Genstart
  | Handler
  | Globreal
  | Globals
  | Allocsize
  | Otherheap
  | Currheap
  | Bitmapbase
  | Progstart
  | Heaplength
  | Triggergc
  | Endofheap
  | Nextfree
