open X64_ast

let string_of_reg = function
  | Rax -> "%rax" | Rbx -> "%rbx" | Rcx -> "%rcx" | Rdx -> "%rdx"
  | Rsi -> "%rsi" | Rdi -> "%rdi" | Rbp -> "%rbp" | Rsp -> "%rsp"
  | R8  -> "%r8"  | R9  -> "%r9"  | R10 -> "%r10" | R11 -> "%r11"
  | R12 -> "%r12" | R13 -> "%r13" | R14 -> "%r14" | R15 -> "%r15"

let string_of_reg32 = function
  | Rax -> "%eax" | Rbx -> "%ebx" | Rcx -> "%ecx" | Rdx -> "%edx"
  | Rsi -> "%esi" | Rdi -> "%edi" | Rbp -> "%ebp" | Rsp -> "%esp"
  | R8  -> "%r8d"  | R9  -> "%r9d"  | R10 -> "%r10d" | R11 -> "%r11d"
  | R12 -> "%r12d" | R13 -> "%r13d" | R14 -> "%r14d" | R15 -> "%r15d"

let string_of_reg8 = function
  | Rax -> "%al" | Rbx -> "%bl" | Rcx -> "%cl" | Rdx -> "%dl"
  | Rsi -> "%sil" | Rdi -> "%dil" | Rbp -> "%bpl" | Rsp -> "%spl"
  | R8  -> "%r8b"  | R9  -> "%r9b"  | R10 -> "%r10b" | R11 -> "%r11b"
  | R12 -> "%r12b" | R13 -> "%r13b" | R14 -> "%r14b" | R15 -> "%r15b"

let string_of_scale = function
  | S1 -> "1" | S2 -> "2" | S4 -> "4" | S8 -> "8"

let string_of_mem m =
  let disp_str = if m.disp = 0L then "" else Int64.to_string m.disp in
  if m.is_rip then
    Printf.sprintf "%s(%%rip)" disp_str
  else
    match m.base, m.index with
    | None, None -> disp_str
    | Some b, None -> Printf.sprintf "%s(%s)" disp_str (string_of_reg b)
    | None, Some i -> Printf.sprintf "%s(,%s,%s)" disp_str (string_of_reg i) (string_of_scale m.scale)
    | Some b, Some i -> Printf.sprintf "%s(%s,%s,%s)" disp_str (string_of_reg b) (string_of_reg i) (string_of_scale m.scale)

let string_of_operand = function
  | Reg r -> string_of_reg r
  | Reg32 r -> string_of_reg32 r
  | Reg8 r -> string_of_reg8 r
  | Imm i -> "$" ^ Int64.to_string i
  | Mem m -> string_of_mem m
  | Lbl l -> l

let string_of_cond = function
  | E -> "e" | Ne -> "ne" | L -> "l" | Le -> "le" | G -> "g" | Ge -> "ge"
  | B -> "b" | Be -> "be" | A -> "a" | Ae -> "ae" | Z -> "z" | Nz -> "nz" | O -> "o"

let string_of_instr = function
  | Mov (dst, src) -> Printf.sprintf "movq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Movzx (dst, src) -> Printf.sprintf "movzbq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Lea (dst, src) -> Printf.sprintf "leaq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Add (dst, src) -> Printf.sprintf "addq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Adc (dst, src) -> Printf.sprintf "adcq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Sub (dst, src) -> Printf.sprintf "subq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Mul src -> Printf.sprintf "mulq %s" (string_of_operand src)
  | Div src -> Printf.sprintf "divq %s" (string_of_operand src)
  | Imul (dst, src) -> Printf.sprintf "imulq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Idiv src -> Printf.sprintf "idivq %s" (string_of_operand src)
  | Inc dst -> Printf.sprintf "incq %s" (string_of_operand dst)
  | Dec dst -> Printf.sprintf "decq %s" (string_of_operand dst)
  | And (dst, src) -> Printf.sprintf "andq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Or (dst, src)  -> Printf.sprintf "orq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Xor (dst, src) -> Printf.sprintf "xorq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Not dst -> Printf.sprintf "notq %s" (string_of_operand dst)
  | Shl (dst, src) -> Printf.sprintf "shlq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Shr (dst, src) -> Printf.sprintf "shrq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Sar (dst, src) -> Printf.sprintf "sarq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Ror (dst, src) -> Printf.sprintf "rorq %s, %s" (string_of_operand src) (string_of_operand dst)
  | Cmp (op1, op2) -> Printf.sprintf "cmpq %s, %s" (string_of_operand op2) (string_of_operand op1)
  | Test (op1, op2) -> Printf.sprintf "testq %s, %s" (string_of_operand op2) (string_of_operand op1)
  | Cmc -> "cmc"
  | Setb op -> Printf.sprintf "setb %s" (string_of_operand op)
  | Seto op -> Printf.sprintf "seto %s" (string_of_operand op)
  | Jmp (Lbl l) -> Printf.sprintf "jmp %s" l
  | Jmp op -> Printf.sprintf "jmp *%s" (string_of_operand op)
  | Jcc (cond, lbl) -> Printf.sprintf "j%s %s" (string_of_cond cond) lbl
  | Call (Lbl l) -> Printf.sprintf "call %s" l
  | Call op -> Printf.sprintf "call *%s" (string_of_operand op)
  | Ret -> "ret"
  | Push op -> Printf.sprintf "pushq %s" (string_of_operand op)
  | Pop op -> Printf.sprintf "popq %s" (string_of_operand op)
  | Cqo -> "cqo"
  | Nop -> "nop"
  | Directive d -> d
  | InlineLbl l -> l ^ ":"

let emit_instr oc = function
  | Directive d -> Printf.fprintf oc "%s\n" d
  | InlineLbl l -> Printf.fprintf oc "%s:\n" l
  | instr -> Printf.fprintf oc "\t%s\n" (string_of_instr instr)

let emit_block oc block =
  Printf.fprintf oc "%s:\n" block.label;
  List.iter (emit_instr oc) block.instrs

let emit_prog oc prog =
  Printf.fprintf oc "\t.text\n";
  List.iter (emit_block oc) prog
