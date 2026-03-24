let prelude = "/* Preprocessor to get around Mac OS, Windows, and Linux differences in naming and calling conventions */

#if defined(__APPLE__)
# define cdecl(s) _##s
#else
# define cdecl(s) s
#endif

.intel_syntax noprefix

.data
.globl cdecl(cml_heap)
.globl cdecl(cml_stack)
.globl cdecl(cml_stackend)
.globl cdecl(cake_bitmaps_buffer_begin)
.globl cdecl(cake_bitmaps_buffer_end)
.globl cdecl(cake_codebuffer_begin)
.globl cdecl(cake_codebuffer_end)
cdecl(cml_heap): .quad 0
cdecl(cml_stack): .quad 0
cdecl(cml_stackend): .quad 0
cdecl(cake_bitmaps_buffer_begin): .quad 0
cdecl(cake_bitmaps_buffer_end): .quad 0
cdecl(cake_codebuffer_begin): .quad 0
cdecl(cake_codebuffer_end): .quad 0

.text
.globl cdecl(cml_main)
cdecl(cml_main):
  push rbp
  mov rbp, rsp
  lea rdi, [rip + cake_main]
  mov rsi, [rip + cdecl(cml_heap)]
  lea rax, [rip + cake_bitmaps]
  mov [rsi + 0], rax
  lea rax, [rip + cdecl(cake_bitmaps_buffer_begin)]
  mov [rsi + 8], rax
  lea rax, [rip + cdecl(cake_bitmaps_buffer_end)]
  mov [rsi + 16], rax
  lea rax, [rip + cdecl(cake_codebuffer_begin)]
  mov [rsi + 24], rax
  lea rax, [rip + cdecl(cake_codebuffer_end)]
  mov [rsi + 32], rax
  mov rdx, [rip + cdecl(cml_stack)]
  mov rcx, [rip + cdecl(cml_stackend)]
  jmp cake_main

cake_clear:
  push rax
  push rdi
  call cdecl(cml_clear)
  pop rdi
  ret

cake_exit:
  call cdecl(cml_exit)"

open X64_ast

let x64_regs = [|
  Rax; Rcx; Rdx; Rbx; Rbp; Rsp; Rsi; Rdi;
  R8; R9; R10; R11; R12; R13; R14; R15
|]

let to_reg r =
  let n = Z.to_int r in
  if n >= 0 && n < Array.length x64_regs then x64_regs.(n)
  else Rax (* Should probably error out instead, but matching textual dump *)

let rec spt_lookup k = function
  | Common.Ln -> None
  | Common.Ls v -> if Z.equal k Z.zero then Some v else None
  | Common.Bn (t1, t2) ->
    if Z.equal k Z.zero then None
    else let k' = Z.div (Z.sub k Z.one) (Z.of_int 2) in
    if Z.is_even k then spt_lookup k' t1 else spt_lookup k' t2
  | Common.Bs (t1, v, t2) ->
    if Z.equal k Z.zero then Some v
    else let k' = Z.div (Z.sub k Z.one) (Z.of_int 2) in
    if Z.is_even k then spt_lookup k' t1 else spt_lookup k' t2

let sanitize_name s =
  String.init (String.length s) (fun i ->
    let c = s.[i] in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' then c
    else '_')

let pp_sec_name names sec =
  match spt_lookup sec names with
  | Some name -> Printf.sprintf "%s_%s" (sanitize_name name) (Z.to_string sec)
  | None -> Z.to_string sec

let pp_lab names (Lab_lang.Lab (s, l)) =
  Printf.sprintf "L%s_%s" (pp_sec_name names s) (Z.to_string l)

let to_operand = function
  | Common.Reg r -> Reg (to_reg r)
  | Common.Imm w -> Imm w

let to_cond = function
  | Common.Equal -> E | Common.Less -> L
  | Common.Lower -> B | Common.Test -> E
  | Common.Notequal -> Ne | Common.Notless -> Ge
  | Common.Notlower -> Ae | Common.Nottest -> Ne

let to_jmp_cond = function
  | Common.Equal -> E | Common.Less -> L
  | Common.Lower -> B | Common.Test -> E
  | Common.Notequal -> Ne | Common.Notless -> Ge
  | Common.Notlower -> Ae | Common.Nottest -> Ne

let create_prog secs names =
  let main_jump = Jmp (Lbl (Printf.sprintf "L%s_0" (pp_sec_name names Z.zero))) in
  let prelude_block = { label = ""; instrs = [Directive prelude; Directive ""; Directive ".globl cake_main"; InlineLbl "cake_main"; main_jump; Directive ""] } in

  let blocks = List.map (fun (Lab_lang.Section (name, lines)) ->
    let label = "" in
    let instrs = [Directive ".p2align 3"; InlineLbl (Printf.sprintf "L%s_0" (pp_sec_name names name))] in
    let rec process_lines lines current_instrs =
      match lines with
      | [] -> List.rev current_instrs
      | line :: rest ->
        let new_instrs = match line with
        | Lab_lang.Label (sec, lab, _) ->
          [InlineLbl (Printf.sprintf "L%s_%s" (pp_sec_name names sec) (Z.to_string lab))]
        | Lab_lang.Asm (acbw, _, _) ->
          (match acbw with
           | Lab_lang.Asmi (Common.Inst (Common.Skip)) -> []
           | Lab_lang.Asmi (Common.Inst (Common.Const (r, w))) ->
             [Mov (Reg (to_reg r), Imm w)]
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Binop (op, d, s, ri)))) ->
             let op_instr = match op with
               | Common.Add -> fun dst src -> Add (dst, src)
               | Common.Sub -> fun dst src -> Sub (dst, src)
               | Common.And -> fun dst src -> And (dst, src)
               | Common.Or -> fun dst src -> Or (dst, src)
               | Common.Xor -> fun dst src -> Xor (dst, src)
             in
             if d = s then
               [op_instr (Reg (to_reg d)) (to_operand ri)]
             else
               [Mov (Reg (to_reg d), Reg (to_reg s)); op_instr (Reg (to_reg d)) (to_operand ri)]
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Shift (sh, d, s, ri)))) ->
             let sh_instr = match sh with
               | Common.Lsl -> fun dst src -> Shl (dst, src)
               | Common.Lsr -> fun dst src -> Shr (dst, src)
               | Common.Asr -> fun dst src -> Sar (dst, src)
               | Common.Ror -> fun dst src -> Ror (dst, src)
             in
             let ri_op = match ri with Common.Reg _ -> Reg8 Rcx | Common.Imm w -> Imm w in
             if d = s then
               [sh_instr (Reg (to_reg d)) ri_op]
             else
               [Mov (Reg (to_reg d), Reg (to_reg s)); sh_instr (Reg (to_reg d)) ri_op]
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Longmul (d1, d2, s1, s2)))) ->
             [Mul (Reg (to_reg s2))]
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Longdiv (d1, d2, s1, s2, s3)))) ->
             [Div (Reg (to_reg s3))]
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Addcarry (d, s1, s2, s3)))) ->
             let is_d_s1 = d = s1 in
             let init = [Cmp (Reg (to_reg s3), Imm 1L); Cmc] in
             let adc = if is_d_s1 then [Adc (Reg (to_reg d), Reg (to_reg s2))]
                       else [Mov (Reg (to_reg d), Reg (to_reg s1)); Adc (Reg (to_reg d), Reg (to_reg s2))] in
             let fin = [Mov (Reg (to_reg s3), Imm 0L); Setb (Reg8 (to_reg s3))] in
             init @ adc @ fin
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Addoverflow (d, s1, s2, s3)))) ->
             let is_d_s1 = d = s1 in
             let add = if is_d_s1 then [Add (Reg (to_reg d), Reg (to_reg s2))]
                       else [Mov (Reg (to_reg d), Reg (to_reg s1)); Add (Reg (to_reg d), Reg (to_reg s2))] in
             let fin = [Mov (Reg (to_reg s3), Imm 0L); Seto (Reg8 (to_reg s3))] in
             add @ fin
           | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Suboverflow (d, s1, s2, s3)))) ->
             let is_d_s1 = d = s1 in
             let sub = if is_d_s1 then [Sub (Reg (to_reg d), Reg (to_reg s2))]
                       else [Mov (Reg (to_reg d), Reg (to_reg s1)); Sub (Reg (to_reg d), Reg (to_reg s2))] in
             let fin = [Mov (Reg (to_reg s3), Imm 0L); Seto (Reg8 (to_reg s3))] in
             sub @ fin
           | Lab_lang.Asmi (Common.Inst (Common.Mem (op, r, Common.Addr(a, off)))) ->
             (match op with
              | Common.Load -> [Mov (Reg (to_reg r), Mem { size = Some Quad; base = Some (to_reg a); index = None; scale = S1; disp = off; is_rip = false })]
              | Common.Load8 -> [Movzx (Reg (to_reg r), Mem { size = Some Byte; base = Some (to_reg a); index = None; scale = S1; disp = off; is_rip = false })]
              | Common.Store -> [Mov (Mem { size = Some Quad; base = Some (to_reg a); index = None; scale = S1; disp = off; is_rip = false }, Reg (to_reg r))]
              | Common.Store8 -> [Mov (Mem { size = Some Byte; base = Some (to_reg a); index = None; scale = S1; disp = off; is_rip = false }, Reg8 (to_reg r))]
              | _ -> [])
           | Lab_lang.Asmi (Common.Jump w) ->
             [Directive (Printf.sprintf "  jmp .+%Ld" w)]
           | Lab_lang.Asmi (Common.Jumpcmp (c, r, ri, w)) ->
             if c = Common.Test || c = Common.Nottest then
               [Test (Reg (to_reg r), to_operand ri); Directive (Printf.sprintf "  j%s .+%Ld" (X64_intel_emit.string_of_cond (to_cond c)) w)]
             else
               [Cmp (Reg (to_reg r), to_operand ri); Directive (Printf.sprintf "  j%s .+%Ld" (X64_intel_emit.string_of_cond (to_cond c)) w)]
           | Lab_lang.Asmi (Common.Loc (r, w)) ->
             []
           | Lab_lang.Asmi (Common.Call w) ->
             [Directive (Printf.sprintf "  call .+%Ld" w)]
           | Lab_lang.Asmi (Common.Jumpreg r) ->
             [Jmp (Reg (to_reg r))]
           | Lab_lang.Cbw (a, b) ->
             [Mov (Mem { size = Some Byte; base = Some (to_reg a); index = None; scale = S1; disp = 0L; is_rip = false }, Reg8 (to_reg b))]
           | Lab_lang.Sharemem _ -> []
           | _ -> [])
        | Lab_lang.Labasm (awl, _, _, _) ->
          (match awl with
           | Lab_lang.Halt -> [Jmp (Lbl "cake_exit")]
           | Lab_lang.Jump l -> [Jmp (Lbl (pp_lab names l))]
           | Lab_lang.Call l -> [Call (Lbl (pp_lab names l))]
           | Lab_lang.Jumpcmp (c, r, ri, l) ->
             if c = Common.Test || c = Common.Nottest then
               [Test (Reg (to_reg r), to_operand ri); Jcc (to_cond c, pp_lab names l)]
             else
               [Cmp (Reg (to_reg r), to_operand ri); Jcc (to_cond c, pp_lab names l)]
           | Lab_lang.Callffi name ->
             [Call (Lbl (Printf.sprintf "cdecl(ffi%s)" name))]
           | Lab_lang.Locvalue (r, l) -> [Directive (Printf.sprintf "  lea %s, [rip + %s]" (X64_intel_emit.string_of_reg (to_reg r)) (pp_lab names l))]
           | Lab_lang.Install -> [Jmp (Lbl "cake_clear")]
           | _ -> [])
        in
        process_lines rest (List.rev new_instrs @ current_instrs)
    in
    { label; instrs = process_lines lines (List.rev instrs) }
  ) secs in

  let bitmaps_block =
    let bitmaps_instrs =
      [Directive ""; Directive ".data"; Directive ".align 8"; InlineLbl "cake_bitmaps"]
    in
    { label = ""; instrs = bitmaps_instrs }
  in
  prelude_block :: blocks @ [bitmaps_block]

let () =
  Hook_ref.lab_hook := (fun prog_obj ->
    let secs = Obj.obj prog_obj in
    let clean = List.map Lab_lang_conv.sec_of_gen secs in
    let bitmaps : int64 list = Obj.obj !(Hook_ref.bitmaps) in
    let names : string Common.sptree_spt = Obj.obj !(Hook_ref.names) in

    let prog = create_prog clean names in

    (* Manually handling the bitmaps print to match exact output structure if possible, but X64_intel_emit takes care of the prog *)
    X64_intel_emit.emit_prog stdout prog;
    List.iter (fun w -> Printf.fprintf stdout "  .quad %Ld\n" w) bitmaps;

    flush stdout;
    exit 0)
