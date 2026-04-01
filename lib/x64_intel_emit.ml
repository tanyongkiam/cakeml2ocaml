open X64_ast

let string_of_reg = function
  | Rax -> "rax" | Rbx -> "rbx" | Rcx -> "rcx" | Rdx -> "rdx"
  | Rsi -> "rsi" | Rdi -> "rdi" | Rbp -> "rbp" | Rsp -> "rsp"
  | R8  -> "r8"  | R9  -> "r9"  | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"

let string_of_reg32 = function
  | Rax -> "eax" | Rbx -> "ebx" | Rcx -> "ecx" | Rdx -> "edx"
  | Rsi -> "esi" | Rdi -> "edi" | Rbp -> "ebp" | Rsp -> "esp"
  | R8  -> "r8d"  | R9  -> "r9d"  | R10 -> "r10d" | R11 -> "r11d"
  | R12 -> "r12d" | R13 -> "r13d" | R14 -> "r14d" | R15 -> "r15d"

let string_of_reg8 = function
  | Rax -> "al" | Rbx -> "bl" | Rcx -> "cl" | Rdx -> "dl"
  | Rsi -> "sil" | Rdi -> "dil" | Rbp -> "bpl" | Rsp -> "spl"
  | R8  -> "r8b"  | R9  -> "r9b"  | R10 -> "r10b" | R11 -> "r11b"
  | R12 -> "r12b" | R13 -> "r13b" | R14 -> "r14b" | R15 -> "r15b"

let string_of_scale = function
  | S1 -> "1" | S2 -> "2" | S4 -> "4" | S8 -> "8"

let string_of_size = function
  | Quad -> "QWORD PTR"
  | Byte -> "BYTE PTR"

let string_of_mem m =
  let size_str = match m.size with
    | None -> ""
    | Some s -> string_of_size s ^ " "
  in
  let disp_str = Printf.sprintf " + %Ld" m.disp in
  if m.is_rip then
    let rip_disp =
      if m.disp = 0L then ""
      else if m.disp > 0L then Printf.sprintf " + %Ld" m.disp
      else Printf.sprintf " - %Ld" (Int64.neg m.disp)
    in
    Printf.sprintf "%s[rip%s]" size_str rip_disp
  else
    match m.base, m.index with
    | None, None -> Printf.sprintf "%s[%Ld]" size_str m.disp
    | Some b, None -> Printf.sprintf "%s[%s%s]" size_str (string_of_reg b) disp_str
    | None, Some i -> Printf.sprintf "%s[%s*%s%s]" size_str (string_of_reg i) (string_of_scale m.scale) disp_str
    | Some b, Some i -> Printf.sprintf "%s[%s + %s*%s%s]" size_str (string_of_reg b) (string_of_reg i) (string_of_scale m.scale) disp_str

let string_of_operand = function
  | Reg r -> string_of_reg r
  | Reg32 r -> string_of_reg32 r
  | Reg8 r -> string_of_reg8 r
  | Xmm n -> Printf.sprintf "xmm%d" n
  | Imm i -> Int64.to_string i
  | Mem m -> string_of_mem m
  | Lbl l -> l

let string_of_cond = function
  | E -> "e" | Ne -> "ne" | L -> "l" | Le -> "le" | G -> "g" | Ge -> "ge"
  | B -> "b" | Be -> "be" | A -> "a" | Ae -> "ae" | Z -> "z" | Nz -> "nz" | O -> "o"

let string_of_instr = function
  | Mov (dst, src) -> Printf.sprintf "mov %s, %s" (string_of_operand dst) (string_of_operand src)
  | Movzx (dst, src) -> Printf.sprintf "movzx %s, %s" (string_of_operand dst) (string_of_operand src)
  | Lea (dst, src) -> Printf.sprintf "lea %s, %s" (string_of_operand dst) (string_of_operand src)
  | Add (dst, src) -> Printf.sprintf "add %s, %s" (string_of_operand dst) (string_of_operand src)
  | Adc (dst, src) -> Printf.sprintf "adc %s, %s" (string_of_operand dst) (string_of_operand src)
  | Sub (dst, src) -> Printf.sprintf "sub %s, %s" (string_of_operand dst) (string_of_operand src)
  | Mul src -> Printf.sprintf "mul %s" (string_of_operand src)
  | Div src -> Printf.sprintf "div %s" (string_of_operand src)
  | Imul (dst, src) -> Printf.sprintf "imul %s, %s" (string_of_operand dst) (string_of_operand src)
  | Idiv src -> Printf.sprintf "idiv %s" (string_of_operand src)
  | Inc dst -> Printf.sprintf "inc %s" (string_of_operand dst)
  | Dec dst -> Printf.sprintf "dec %s" (string_of_operand dst)
  | And (dst, src) -> Printf.sprintf "and %s, %s" (string_of_operand dst) (string_of_operand src)
  | Or (dst, src)  -> Printf.sprintf "or %s, %s" (string_of_operand dst) (string_of_operand src)
  | Xor (dst, src) -> Printf.sprintf "xor %s, %s" (string_of_operand dst) (string_of_operand src)
  | Not dst -> Printf.sprintf "not %s" (string_of_operand dst)
  | Shl (dst, src) -> Printf.sprintf "shl %s, %s" (string_of_operand dst) (string_of_operand src)
  | Shr (dst, src) -> Printf.sprintf "shr %s, %s" (string_of_operand dst) (string_of_operand src)
  | Sar (dst, src) -> Printf.sprintf "sar %s, %s" (string_of_operand dst) (string_of_operand src)
  | Ror (dst, src) -> Printf.sprintf "ror %s, %s" (string_of_operand dst) (string_of_operand src)
  | Cmp (op1, op2) -> Printf.sprintf "cmp %s, %s" (string_of_operand op1) (string_of_operand op2)
  | Test (op1, op2) -> Printf.sprintf "test %s, %s" (string_of_operand op1) (string_of_operand op2)
  | Cmc -> "cmc"
  | Setb op -> Printf.sprintf "setb %s" (string_of_operand op)
  | Seto op -> Printf.sprintf "seto %s" (string_of_operand op)
  | Jmp op -> Printf.sprintf "jmp %s" (string_of_operand op)
  | Jcc (cond, lbl) -> Printf.sprintf "j%s %s" (string_of_cond cond) lbl
  | Call op -> Printf.sprintf "call %s" (string_of_operand op)
  | Ret -> "ret"
  | Push op -> Printf.sprintf "push %s" (string_of_operand op)
  | Pop op -> Printf.sprintf "pop %s" (string_of_operand op)
  | Cqo -> "cqo"
  | Nop -> "nop"
  | Directive d -> d
  | InlineLbl l -> l ^ ":"

let emit_instr oc = function
  | Directive d -> Printf.fprintf oc "%s\n" d
  | InlineLbl l -> Printf.fprintf oc "%s:\n" l
  | instr -> Printf.fprintf oc "  %s\n" (string_of_instr instr)

let emit_block oc block =
  if block.label <> "" then Printf.fprintf oc "%s:\n" block.label;
  List.iter (emit_instr oc) block.instrs

let emit_prog oc prog =
  List.iter (emit_block oc) prog
