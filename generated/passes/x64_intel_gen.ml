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
  call cdecl(cml_exit)

"

let x64_reg_names = [|
  "rax"; "rcx"; "rdx"; "rbx"; "rbp"; "rsp"; "rsi"; "rdi";
  "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15"
|]

let pp_reg r =
  let n = Z.to_int r in
  if n >= 0 && n < Array.length x64_reg_names then x64_reg_names.(n)
  else Printf.sprintf "r%d" n

let pp_reg_byte r =
  match pp_reg r with
  | "rax" -> "al" | "rcx" -> "cl" | "rdx" -> "dl" | "rbx" -> "bl"
  | "rbp" -> "bpl" | "rsp" -> "spl" | "rsi" -> "sil" | "rdi" -> "dil"
  | s -> s ^ "b"

let pp_imm64 w = Printf.sprintf "%Ld" w

let pp_lab (Lab_lang.Lab (s, l)) =
  Printf.sprintf "L%s_%s" (Z.to_string s) (Z.to_string l)

let pp_reg_imm = function
  | Common.Reg r -> pp_reg r
  | Common.Imm w -> pp_imm64 w

let pp_cmp = function
  | Common.Equal -> "e" | Common.Less -> "l"
  | Common.Lower -> "b" | Common.Test -> "e"
  | Common.Notequal -> "ne" | Common.Notless -> "ge"
  | Common.Notlower -> "ae" | Common.Nottest -> "ne"

let emit_assembly ch secs bitmaps names =
  Printf.fprintf ch "%s" prelude;
  Printf.fprintf ch ".globl cake_main\n";
  Printf.fprintf ch "cake_main:\n";
  Printf.fprintf ch "  jmp L0_0\n\n";

  List.iter (fun (Lab_lang.Section (name, lines)) ->
    Printf.fprintf ch ".p2align 3\n";
    Printf.fprintf ch "L%s_0:\n" (Z.to_string name);
    List.iter (fun line ->
      match line with
      | Lab_lang.Label (sec, lab, len) ->
        (* Internal labels do not need alignment (CakeML aligns them with NOPs) *)
        assert (not (Z.equal lab Z.zero));
        Printf.fprintf ch "L%s_%s:\n" (Z.to_string sec) (Z.to_string lab)
      | Lab_lang.Asm (acbw, _, _) ->
        (match acbw with
         | Lab_lang.Asmi (Common.Inst (Common.Skip)) -> ()
         | Lab_lang.Asmi (Common.Inst (Common.Const (r, w))) ->
           Printf.fprintf ch "  mov %s, %Ld\n" (pp_reg r) w
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Binop (op, d, s, ri)))) ->
           let op_str = match op with
             | Common.Add -> "add" | Common.Sub -> "sub"
             | Common.And -> "and" | Common.Or -> "or" | Common.Xor -> "xor"
           in
           if d = s then
             Printf.fprintf ch "  %s %s, %s\n" op_str (pp_reg d) (pp_reg_imm ri)
           else begin
             Printf.fprintf ch "  mov %s, %s\n" (pp_reg d) (pp_reg s);
             Printf.fprintf ch "  %s %s, %s\n" op_str (pp_reg d) (pp_reg_imm ri)
           end
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Shift (sh, d, s, ri)))) ->
           let sh_str = match sh with
             | Common.Lsl -> "shl" | Common.Lsr -> "shr" | Common.Asr -> "sar" | Common.Ror -> "ror"
           in
           let ri_str = match ri with Common.Reg _ -> "cl" | Common.Imm w -> pp_imm64 w in
           if d = s then (
             Printf.fprintf ch "  %s %s, %s\n" sh_str (pp_reg d) ri_str
           ) else begin
             Printf.fprintf ch "  mov %s, %s\n" (pp_reg d) (pp_reg s);
             Printf.fprintf ch "  %s %s, %s\n" sh_str (pp_reg d) ri_str
           end
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Longmul (d1, d2, s1, s2)))) ->
           (* s1 must be rax, d1 must be rdx, d2 must be rax. The instruction `mul s2` implicitly does this. *)
           Printf.fprintf ch "  mul %s\n" (pp_reg s2)
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Longdiv (d1, d2, s1, s2, s3)))) ->
           (* s1 must be rdx, s2 must be rax. d1=rax, d2=rdx. `div s3` implicitly does this. *)
           Printf.fprintf ch "  div %s\n" (pp_reg s3)
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Addcarry (d, s1, s2, s3)))) ->
           (* Check d == s1. x64 backend uses 3 registers implicitly: d, s2, s3. *)
           Printf.fprintf ch "  cmp %s, 1\n" (pp_reg s3);
           Printf.fprintf ch "  cmc\n";
           if d = s1 then
             Printf.fprintf ch "  adc %s, %s\n" (pp_reg d) (pp_reg s2)
           else begin
             Printf.fprintf ch "  mov %s, %s\n" (pp_reg d) (pp_reg s1);
             Printf.fprintf ch "  adc %s, %s\n" (pp_reg d) (pp_reg s2)
           end;
           Printf.fprintf ch "  mov %s, 0\n" (pp_reg s3);
           Printf.fprintf ch "  setb %s\n" (pp_reg_byte s3)
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Addoverflow (d, s1, s2, s3)))) ->
           if d = s1 then
             Printf.fprintf ch "  add %s, %s\n" (pp_reg d) (pp_reg s2)
           else begin
             Printf.fprintf ch "  mov %s, %s\n" (pp_reg d) (pp_reg s1);
             Printf.fprintf ch "  add %s, %s\n" (pp_reg d) (pp_reg s2)
           end;
           Printf.fprintf ch "  mov %s, 0\n" (pp_reg s3);
           Printf.fprintf ch "  seto %s\n" (pp_reg_byte s3)
         | Lab_lang.Asmi (Common.Inst (Common.Arith (Common.Suboverflow (d, s1, s2, s3)))) ->
           if d = s1 then
             Printf.fprintf ch "  sub %s, %s\n" (pp_reg d) (pp_reg s2)
           else begin
             Printf.fprintf ch "  mov %s, %s\n" (pp_reg d) (pp_reg s1);
             Printf.fprintf ch "  sub %s, %s\n" (pp_reg d) (pp_reg s2)
           end;
           Printf.fprintf ch "  mov %s, 0\n" (pp_reg s3);
           Printf.fprintf ch "  seto %s\n" (pp_reg_byte s3)
         | Lab_lang.Asmi (Common.Inst (Common.Mem (op, r, Common.Addr(a, off)))) ->
           (match op with
            | Common.Load -> Printf.fprintf ch "  mov %s, QWORD PTR [%s + %Ld]\n" (pp_reg r) (pp_reg a) off
            | Common.Load8 -> Printf.fprintf ch "  movzx %s, BYTE PTR [%s + %Ld]\n" (pp_reg r) (pp_reg a) off
            | Common.Store -> Printf.fprintf ch "  mov QWORD PTR [%s + %Ld], %s\n" (pp_reg a) off (pp_reg r)
            | Common.Store8 -> Printf.fprintf ch "  mov BYTE PTR [%s + %Ld], %s\n" (pp_reg a) off (pp_reg_byte r)
            | _ -> ())
         | Lab_lang.Asmi (Common.Jump w) ->
           Printf.fprintf ch "  jmp .+%Ld\n" w
         | Lab_lang.Asmi (Common.Jumpcmp (c, r, ri, w)) ->
           if c = Common.Test || c = Common.Nottest then
             Printf.fprintf ch "  test %s, %s\n  j%s .+%Ld\n" (pp_reg r) (pp_reg_imm ri) (pp_cmp c) w
           else
             Printf.fprintf ch "  cmp %s, %s\n  j%s .+%Ld\n" (pp_reg r) (pp_reg_imm ri) (pp_cmp c) w
         | Lab_lang.Asmi (Common.Loc (r, w)) ->
           ()
         | Lab_lang.Asmi (Common.Call w) ->
           Printf.fprintf ch "  call .+%Ld\n" w
         | Lab_lang.Asmi (Common.Jumpreg r) ->
           Printf.fprintf ch "  jmp %s\n" (pp_reg r)
         | Lab_lang.Cbw (a, b) ->
           Printf.fprintf ch "  mov BYTE PTR [%s + 0], %s\n" (pp_reg a) (pp_reg_byte b)
         | Lab_lang.Sharemem _ -> ()
         | _ -> ())
      | Lab_lang.Labasm (awl, _, _, _) ->
        (match awl with
         | Lab_lang.Halt -> Printf.fprintf ch "  jmp cake_exit\n"
         | Lab_lang.Jump l -> Printf.fprintf ch "  jmp %s\n" (pp_lab l)
         | Lab_lang.Call l -> Printf.fprintf ch "  call %s\n" (pp_lab l)
         | Lab_lang.Jumpcmp (c, r, ri, l) ->
           if c = Common.Test || c = Common.Nottest then
             Printf.fprintf ch "  test %s, %s\n  j%s %s\n" (pp_reg r) (pp_reg_imm ri) (pp_cmp c) (pp_lab l)
           else
             Printf.fprintf ch "  cmp %s, %s\n  j%s %s\n" (pp_reg r) (pp_reg_imm ri) (pp_cmp c) (pp_lab l)
         | Lab_lang.Callffi name ->
           
           Printf.fprintf ch "  call cdecl(ffi%s)\n" name;
           
         | Lab_lang.Locvalue (r, l) -> Printf.fprintf ch "  lea %s, [rip + %s]\n" (pp_reg r) (pp_lab l)
         | Lab_lang.Install -> Printf.fprintf ch "  jmp cake_clear\n"
         | _ -> ())
    ) lines
  ) secs;
  
  Printf.fprintf ch "\n.data\n";
  Printf.fprintf ch ".align 8\n";
  Printf.fprintf ch "cake_bitmaps:\n";
  List.iter (fun w -> Printf.fprintf ch "  .quad %Ld\n" w) bitmaps

let () =
  Hook_ref.lab_hook := (fun prog_obj ->
    let secs = Obj.obj prog_obj in
    let clean = List.map Lab_lang_conv.sec_of_gen secs in
    let bitmaps : int64 list = Obj.obj !(Hook_ref.bitmaps) in
    let names : string Common.sptree_spt = Obj.obj !(Hook_ref.names) in
    emit_assembly stdout clean bitmaps names;
    flush stdout;
    exit 0)
