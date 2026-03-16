(* asm_dump_setup.ml — Hook that extracts labLang sections and prints a dummy
   summary, then exits before lab_to_target runs.

   This is the first step toward producing a .S file with assembly text
   instead of raw bytes. *)

(* Dummy printer for labLang instructions *)
let pp_reg r = Printf.sprintf "r%s" (Z.to_string r)

let pp_imm64 w = Printf.sprintf "0x%Lx" w

let pp_lab (Lab_lang.Lab (s, l)) =
  Printf.sprintf "L%s_%s" (Z.to_string s) (Z.to_string l)

let pp_reg_imm = function
  | Common.Reg r -> pp_reg r
  | Common.Imm w -> pp_imm64 w

let pp_shift = function
  | Common.Lsl -> "lsl" | Common.Lsr -> "lsr"
  | Common.Asr -> "asr" | Common.Ror -> "ror"

let pp_binop = function
  | Common.Add -> "add" | Common.Sub -> "sub"
  | Common.And -> "and" | Common.Or -> "or"
  | Common.Xor -> "xor"

let pp_cmp = function
  | Common.Equal -> "eq" | Common.Less -> "lt"
  | Common.Lower -> "lo" | Common.Test -> "test"
  | Common.Notequal -> "ne" | Common.Notless -> "ge"
  | Common.Notlower -> "hs" | Common.Nottest -> "ntest"

let pp_memop = function
  | Common.Load -> "load" | Common.Load8 -> "load8"
  | Common.Load16 -> "load16" | Common.Load32 -> "load32"
  | Common.Store -> "store" | Common.Store8 -> "store8"
  | Common.Store16 -> "store16" | Common.Store32 -> "store32"

let pp_addr (Common.Addr (r, off)) =
  Printf.sprintf "[%s + %Ld]" (pp_reg r) off

let pp_arith = function
  | Common.Binop (op, d, s, ri) ->
    Printf.sprintf "%s %s, %s, %s" (pp_binop op) (pp_reg d) (pp_reg s) (pp_reg_imm ri)
  | Common.Shift (sh, d, s, ri) ->
    Printf.sprintf "%s %s, %s, %s" (pp_shift sh) (pp_reg d) (pp_reg s) (pp_reg_imm ri)
  | Common.Div (d, s1, s2) ->
    Printf.sprintf "div %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Longmul (d1, d2, s1, s2) ->
    Printf.sprintf "longmul %s, %s, %s, %s" (pp_reg d1) (pp_reg d2) (pp_reg s1) (pp_reg s2)
  | Common.Longdiv (d1, d2, s1, s2, s3) ->
    Printf.sprintf "longdiv %s, %s, %s, %s, %s" (pp_reg d1) (pp_reg d2) (pp_reg s1) (pp_reg s2) (pp_reg s3)
  | Common.Addcarry (d, s1, s2, s3) ->
    Printf.sprintf "addcarry %s, %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2) (pp_reg s3)
  | Common.Addoverflow (d, s1, s2, s3) ->
    Printf.sprintf "addoverflow %s, %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2) (pp_reg s3)
  | Common.Suboverflow (d, s1, s2, s3) ->
    Printf.sprintf "suboverflow %s, %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2) (pp_reg s3)

let pp_fp = function
  | Common.Fpless (d, s1, s2) ->
    Printf.sprintf "fpless %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fplessequal (d, s1, s2) ->
    Printf.sprintf "fple %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpequal (d, s1, s2) ->
    Printf.sprintf "fpeq %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpabs (d, s) ->
    Printf.sprintf "fpabs %s, %s" (pp_reg d) (pp_reg s)
  | Common.Fpneg (d, s) ->
    Printf.sprintf "fpneg %s, %s" (pp_reg d) (pp_reg s)
  | Common.Fpsqrt (d, s) ->
    Printf.sprintf "fpsqrt %s, %s" (pp_reg d) (pp_reg s)
  | Common.Fpadd (d, s1, s2) ->
    Printf.sprintf "fpadd %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpsub (d, s1, s2) ->
    Printf.sprintf "fpsub %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpmul (d, s1, s2) ->
    Printf.sprintf "fpmul %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpdiv (d, s1, s2) ->
    Printf.sprintf "fpdiv %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpfma (d, s1, s2) ->
    Printf.sprintf "fpfma %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpmov (d, s) ->
    Printf.sprintf "fpmov %s, %s" (pp_reg d) (pp_reg s)
  | Common.Fpmovtoreg (d, s1, s2) ->
    Printf.sprintf "fpmovtoreg %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fpmovfromreg (d, s1, s2) ->
    Printf.sprintf "fpmovfromreg %s, %s, %s" (pp_reg d) (pp_reg s1) (pp_reg s2)
  | Common.Fptoint (d, s) ->
    Printf.sprintf "fptoint %s, %s" (pp_reg d) (pp_reg s)
  | Common.Fpfromint (d, s) ->
    Printf.sprintf "fpfromint %s, %s" (pp_reg d) (pp_reg s)

let pp_inst = function
  | Common.Skip -> "skip"
  | Common.Const (r, w) -> Printf.sprintf "const %s, %s" (pp_reg r) (pp_imm64 w)
  | Common.Arith ar -> pp_arith ar
  | Common.Mem (op, r, a) -> Printf.sprintf "%s %s, %s" (pp_memop op) (pp_reg r) (pp_addr a)
  | Common.Fp fp -> pp_fp fp

let pp_asm = function
  | Common.Inst i -> pp_inst i
  | Common.Jump w -> Printf.sprintf "jmp %s" (pp_imm64 w)
  | Common.Jumpcmp (c, r, ri, w) ->
    Printf.sprintf "j%s %s, %s, %s" (pp_cmp c) (pp_reg r) (pp_reg_imm ri) (pp_imm64 w)
  | Common.Call w -> Printf.sprintf "call %s" (pp_imm64 w)
  | Common.Loc (r, w) -> Printf.sprintf "loc %s, %s" (pp_reg r) (pp_imm64 w)
  | Common.Jumpreg r -> Printf.sprintf "jmpreg %s" (pp_reg r)

let pp_asm_with_lab = function
  | Lab_lang.Halt -> "halt"
  | Lab_lang.Install -> "install"
  | Lab_lang.Callffi name -> Printf.sprintf "callffi \"%s\"" name
  | Lab_lang.Locvalue (r, l) -> Printf.sprintf "locvalue %s, %s" (pp_reg r) (pp_lab l)
  | Lab_lang.Call l -> Printf.sprintf "call %s" (pp_lab l)
  | Lab_lang.Jumpcmp (c, r, ri, l) ->
    Printf.sprintf "j%s %s, %s, %s" (pp_cmp c) (pp_reg r) (pp_reg_imm ri) (pp_lab l)
  | Lab_lang.Jump l -> Printf.sprintf "jmp %s" (pp_lab l)

let pp_asm_or_cbw = function
  | Lab_lang.Asmi a -> pp_asm a
  | Lab_lang.Cbw (a, b) -> Printf.sprintf "cbw %s, %s" (Z.to_string a) (Z.to_string b)
  | Lab_lang.Sharemem (op, r, a) ->
    Printf.sprintf "sharemem.%s %s, %s" (pp_memop op) (pp_reg r) (pp_addr a)

let pp_line = function
  | Lab_lang.Label (sec, lab, len) ->
    Printf.sprintf "  .label %s_%s (len=%s):"
      (Z.to_string sec) (Z.to_string lab) (Z.to_string len)
  | Lab_lang.Asm (acbw, bytes, len) ->
    Printf.sprintf "    %s  ; %d bytes (len=%s)"
      (pp_asm_or_cbw acbw) (List.length bytes) (Z.to_string len)
  | Lab_lang.Labasm (awl, _, bytes, len) ->
    Printf.sprintf "    %s  ; %d bytes (len=%s)"
      (pp_asm_with_lab awl) (List.length bytes) (Z.to_string len)

let dump_sections ch secs =
  let n_secs = List.length secs in
  let n_lines = List.fold_left (fun acc (Lab_lang.Section (_, ls)) -> acc + List.length ls) 0 secs in
  Printf.fprintf ch "; labLang dump: %d sections, %d lines\n" n_secs n_lines;
  List.iter (fun (Lab_lang.Section (name, lines)) ->
    Printf.fprintf ch "\n; === Section %s (%d lines) ===\n" (Z.to_string name) (List.length lines);
    List.iter (fun line ->
      Printf.fprintf ch "%s\n" (pp_line line)
    ) lines
  ) secs

let () =
  Hook_ref.lab_hook := (fun prog_obj ->
    let secs = Obj.obj prog_obj in
    (* Convert from generated types to clean types *)
    let clean = List.map Lab_lang_conv.sec_of_gen secs in
    (* Dump to stdout *)
    dump_sections stdout clean;
    flush stdout;
    (* Exit before lab_to_target runs *)
    exit 0)
