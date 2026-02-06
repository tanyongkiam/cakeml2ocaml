(* Conversion functions between Lab_lang clean types and
   M_to_target64Prog generated types in cake64.ml *)

module G = Cake64
open Common_conv

(* labLang_lab: identical *)
let lab_of_gen : G.M_to_target64Prog.labLang_lab -> Lab_lang.lab = Obj.magic
let lab_to_gen : Lab_lang.lab -> G.M_to_target64Prog.labLang_lab = Obj.magic

(* labLang_asm_with_lab: Halt_1/Install_1/Locvalue_1/Call_1 <-> clean *)

let asm_with_lab_of_gen : G.M_to_target64Prog.labLang_asm_with_lab -> Lab_lang.asm_with_lab = function
  | G.M_to_target64Prog.Halt_1 -> Lab_lang.Halt
  | G.M_to_target64Prog.Install_1 -> Lab_lang.Install
  | G.M_to_target64Prog.Callffi s -> Lab_lang.Callffi s
  | G.M_to_target64Prog.Locvalue_1 (r, l) -> Lab_lang.Locvalue (r, lab_of_gen l)
  | G.M_to_target64Prog.Call_1 l -> Lab_lang.Call (lab_of_gen l)
  | G.M_to_target64Prog.Jumpcmp (c, r, ri, l) ->
    Lab_lang.Jumpcmp (asm_cmp_of_gen c, r, asm_reg_imm_of_gen ri, lab_of_gen l)
  | G.M_to_target64Prog.Jump l -> Lab_lang.Jump (lab_of_gen l)

let asm_with_lab_to_gen : Lab_lang.asm_with_lab -> G.M_to_target64Prog.labLang_asm_with_lab = function
  | Lab_lang.Halt -> G.M_to_target64Prog.Halt_1
  | Lab_lang.Install -> G.M_to_target64Prog.Install_1
  | Lab_lang.Callffi s -> G.M_to_target64Prog.Callffi s
  | Lab_lang.Locvalue (r, l) -> G.M_to_target64Prog.Locvalue_1 (r, lab_to_gen l)
  | Lab_lang.Call l -> G.M_to_target64Prog.Call_1 (lab_to_gen l)
  | Lab_lang.Jumpcmp (c, r, ri, l) ->
    G.M_to_target64Prog.Jumpcmp (asm_cmp_to_gen c, r, asm_reg_imm_to_gen ri, lab_to_gen l)
  | Lab_lang.Jump l -> G.M_to_target64Prog.Jump (lab_to_gen l)

(* labLang_asm_or_cbw: identical constructor names *)

let asm_or_cbw_of_gen : G.M_to_target64Prog.labLang_asm_or_cbw -> Lab_lang.asm_or_cbw = function
  | G.M_to_target64Prog.Sharemem (m, r, a) ->
    Lab_lang.Sharemem (asm_memop_of_gen m, r, asm_addr_of_gen a)
  | G.M_to_target64Prog.Cbw (a, b) -> Lab_lang.Cbw (a, b)
  | G.M_to_target64Prog.Asmi a -> Lab_lang.Asmi (asm_asm_of_gen a)

let asm_or_cbw_to_gen : Lab_lang.asm_or_cbw -> G.M_to_target64Prog.labLang_asm_or_cbw = function
  | Lab_lang.Sharemem (m, r, a) ->
    G.M_to_target64Prog.Sharemem (asm_memop_to_gen m, r, asm_addr_to_gen a)
  | Lab_lang.Cbw (a, b) -> G.M_to_target64Prog.Cbw (a, b)
  | Lab_lang.Asmi a -> G.M_to_target64Prog.Asmi (asm_asm_to_gen a)

(* labLang_line: identical constructor names *)

let line_of_gen : G.M_to_target64Prog.labLang_line -> Lab_lang.line = function
  | G.M_to_target64Prog.Labasm (a, b, c, d) -> Lab_lang.Labasm (asm_with_lab_of_gen a, b, c, d)
  | G.M_to_target64Prog.Asm (a, b, c) -> Lab_lang.Asm (asm_or_cbw_of_gen a, b, c)
  | G.M_to_target64Prog.Label (a, b, c) -> Lab_lang.Label (a, b, c)

let line_to_gen : Lab_lang.line -> G.M_to_target64Prog.labLang_line = function
  | Lab_lang.Labasm (a, b, c, d) -> G.M_to_target64Prog.Labasm (asm_with_lab_to_gen a, b, c, d)
  | Lab_lang.Asm (a, b, c) -> G.M_to_target64Prog.Asm (asm_or_cbw_to_gen a, b, c)
  | Lab_lang.Label (a, b, c) -> G.M_to_target64Prog.Label (a, b, c)

(* labLang_sec: identical *)

let sec_of_gen : G.M_to_target64Prog.labLang_sec -> Lab_lang.sec = function
  | G.M_to_target64Prog.Section (n, ls) -> Lab_lang.Section (n, List.map line_of_gen ls)

let sec_to_gen : Lab_lang.sec -> G.M_to_target64Prog.labLang_sec = function
  | Lab_lang.Section (n, ls) -> G.M_to_target64Prog.Section (n, List.map line_to_gen ls)
