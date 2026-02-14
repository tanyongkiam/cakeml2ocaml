type id =
  | Short of string
  | Long of string * id

type cmp = Equal | Less | Greater | LessEq | GreaterEq
         | AltLess | AltLessEq | AltGreater | AltGreaterEq
type word_size = W8 | W64
type comp_type = IntT | BoolT | StrT | CharT | Word8T | Word64T | Float64T

type shift_type = Lsl_ | Lsr_ | Asr_ | Ror_

type fp_bop = FPAdd | FPDiv | FPMul | FPSub
type fp_uop = FPAbs | FPNeg | FPSqrt
type fp_top = FPFma

type op =
  | Opapp
  | Equality
  | OpnPlus | OpnMinus | OpnTimes | OpnDivide | OpnModulo
  | Test of cmp * comp_type
  | W8fromInt | W8toInt
  | W64fromInt | W64toInt
  | Opw8Add | Opw8Sub | Opw8Andw | Opw8Orw | Opw8Xor
  | Opw64Add | Opw64Sub | Opw64Andw | Opw64Orw | Opw64Xor
  | Shift of word_size * shift_type * int
  | Strlen | Strsub | Strcat | Explode | Implode
  | CopyStrStr | CopyStrAw8 | CopyAw8Str | CopyAw8Aw8
  | Aalloc | AallocEmpty | Asub | Alength | Aupdate
  | Aw8alloc | Aw8sub | Aw8length | Aw8update
  | VfromList | Vsub | Vlength
  | Opref | Opderef | Opassign
  | ListAppend
  | Ord | Chr
  | FPbop of fp_bop
  | FPuop of fp_uop
  | FPtop of fp_top
  | FpFromWord | FpToWord
  | BoolNot
  | CharToW8 | W8ToChar
  | Asubunsafe | Aupdateunsafe
  | Aw8subunsafe | Aw8updateunsafe
  | Vsubunsafe | XorAw8Strunsafe
  | ConfigGC
  | Eval
  | FFI of string

type log_op = And | Or

type lit =
  | IntLit of string  (* stored as raw string for bigint *)
  | StrLit of string
  | CharLit of string
  | Word8Lit of int
  | Word64Lit of string
  | Float64Lit of string  (* stored as int64 bit pattern *)

type pat =
  | Pvar of string
  | Pcon of id option * pat list
  | Plit of lit
  | Pany
  | Pref of pat
  | Ptannot of pat * typ

and typ =
  | Atvar of string
  | Atapp of typ list * id
  | Attup of typ list
  | Atfun of typ * typ

type exp =
  | Fun of string * exp
  | Mat of exp * (pat * exp) list
  | App of op * exp list
  | Var of id
  | Con of id option * exp list
  | Lit of lit
  | If of exp * exp * exp
  | Let of string option * exp * exp
  | Log of log_op * exp * exp
  | Raise of exp
  | Handle of exp * (pat * exp) list
  | Letrec of (string * string * exp) list * exp

type dec =
  | Dtype of (string list * string * (string * typ list) list) list
  | Dlet of pat * exp
  | DletSimple of string * exp   (* the common case: Dlet (unk unk) "name" expr *)
  | Dletrec of (string * string * exp) list
  | Dexn of string * typ list
  | Dtabbrev of string list * string * typ
  | Dmod of string * dec list
  | Dlocal of dec list * dec list
  | Denv of string

type program = dec list
