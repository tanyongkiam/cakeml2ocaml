type reg =
  | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type scale = S1 | S2 | S4 | S8

type size = Quad | Byte

type mem = {
  size : size option;
  base : reg option;
  index : reg option;
  scale : scale;
  disp : int64;
  is_rip : bool;
}

type operand =
  | Reg of reg
  | Reg32 of reg
  | Reg8 of reg
  | Xmm of int
  | Imm of int64
  | Mem of mem
  | Lbl of string

type cond =
  | E | Ne | L | Le | G | Ge | B | Be | A | Ae | Z | Nz | O

type instr =
  | Mov of operand * operand
  | Movzx of operand * operand
  | Lea of operand * operand
  | Add of operand * operand
  | Adc of operand * operand
  | Sub of operand * operand
  | Mul of operand
  | Div of operand
  | Imul of operand * operand
  | Idiv of operand
  | Inc of operand
  | Dec of operand
  | And of operand * operand
  | Or of operand * operand
  | Xor of operand * operand
  | Not of operand
  | Shl of operand * operand
  | Shr of operand * operand
  | Sar of operand * operand
  | Ror of operand * operand
  | Cmp of operand * operand
  | Test of operand * operand
  | Cmc
  | Setb of operand
  | Seto of operand
  | Jmp of operand
  | Jcc of cond * string
  | Call of operand
  | Ret
  | Push of operand
  | Pop of operand
  | Cqo
  | Nop
  | Directive of string
  | InlineLbl of string

type block = {
  label : string;
  instrs : instr list;
}

type prog = block list
