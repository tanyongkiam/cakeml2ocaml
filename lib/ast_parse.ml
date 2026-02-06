open Sexp_parser
open Cakeml_ast

exception Ast_error of string

let ast_error msg sexp =
  raise (Ast_error (Printf.sprintf "%s: %s" msg (sexp_to_string sexp)))

(* Strip surrounding quotes from a string atom *)
let unquote s =
  if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.sub s 1 (String.length s - 2)
  else s

let is_quoted s =
  String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'

(* Parse an identifier *)
let rec parse_id sexp =
  match sexp with
  | List [Atom "Short"; Atom s] -> Short (unquote s)
  | List [Atom "Long"; Atom s; id_sexp] -> Long (unquote s, parse_id id_sexp)
  | _ -> ast_error "Expected identifier" sexp

(* Parse an optional value: NONE or (SOME x) *)
let parse_option parse_inner sexp =
  match sexp with
  | Atom "NONE" -> None
  | List [Atom "SOME"; x] -> Some (parse_inner x)
  | _ -> ast_error "Expected NONE or (SOME x)" sexp

(* Parse a type *)
let rec parse_type sexp =
  match sexp with
  | List [Atom "Atvar"; Atom s] -> Atvar (unquote s)
  | List [Atom "Atapp"; args_sexp; id_sexp] ->
    let args = parse_type_list args_sexp in
    Atapp (args, parse_id id_sexp)
  | List [Atom "Attup"; args_sexp] ->
    Attup (parse_type_list args_sexp)
  | List [Atom "Atfun"; t1; t2] ->
    Atfun (parse_type t1, parse_type t2)
  | _ -> ast_error "Expected type" sexp

and parse_type_list sexp =
  match sexp with
  | List items -> List.map parse_type items
  | Atom "nil" -> []
  | _ -> ast_error "Expected type list" sexp

(* Parse a comparison operator *)
let parse_cmp s =
  match s with
  | "Equal" -> Equal
  | "Less" -> Less
  | "Greater" -> Greater
  | "LessEq" -> LessEq
  | "GreaterEq" -> GreaterEq
  | _ -> raise (Ast_error ("Unknown comparison: " ^ s))

(* Parse a comparison type *)
let parse_comp_type s =
  match s with
  | "IntT" -> IntT
  | "BoolT" -> BoolT
  | "StrT" -> StrT
  | "CharT" -> CharT
  | "Word8T" -> Word8T
  | "Word64T" -> Word64T
  | "Float64T" -> Float64T
  | _ -> raise (Ast_error ("Unknown comparison type: " ^ s))

(* Parse an operator *)
let rec parse_op sexp =
  match sexp with
  | Atom "Opapp" -> Opapp
  | Atom "Equality" -> Equality
  | Atom "OpnPlus" -> OpnPlus
  | Atom "OpnMinus" -> OpnMinus
  | Atom "OpnTimes" -> OpnTimes
  | Atom "OpnDivide" -> OpnDivide
  | Atom "OpnModulo" -> OpnModulo
  | Atom "W8fromInt" -> W8fromInt
  | Atom "W8toInt" -> W8toInt
  | Atom "W64fromInt" -> W64fromInt
  | Atom "W64toInt" -> W64toInt
  | Atom "Opw8Add" -> Opw8Add
  | Atom "Opw8Sub" -> Opw8Sub
  | Atom "Opw8Andw" -> Opw8Andw
  | Atom "Opw8Orw" -> Opw8Orw
  | Atom "Opw8Xor" -> Opw8Xor
  | Atom "Opw64Add" -> Opw64Add
  | Atom "Opw64Sub" -> Opw64Sub
  | Atom "Opw64Andw" -> Opw64Andw
  | Atom "Opw64Orw" -> Opw64Orw
  | Atom "Opw64Xor" -> Opw64Xor
  | Atom "Strlen" -> Strlen
  | Atom "Strsub" -> Strsub
  | Atom "Strcat" -> Strcat
  | Atom "Explode" -> Explode
  | Atom "Implode" -> Implode
  | Atom "CopyStrStr" -> CopyStrStr
  | Atom "CopyStrAw8" -> CopyStrAw8
  | Atom "CopyAw8Str" -> CopyAw8Str
  | Atom "CopyAw8Aw8" -> CopyAw8Aw8
  | Atom "Aalloc" -> Aalloc
  | Atom "AallocEmpty" -> AallocEmpty
  | Atom "Asub" -> Asub
  | Atom "Alength" -> Alength
  | Atom "Aupdate" -> Aupdate
  | Atom "Aw8alloc" -> Aw8alloc
  | Atom "Aw8sub" -> Aw8sub
  | Atom "Aw8length" -> Aw8length
  | Atom "Aw8update" -> Aw8update
  | Atom "VfromList" -> VfromList
  | Atom "Vsub" -> Vsub
  | Atom "Vlength" -> Vlength
  | Atom "Opref" -> Opref
  | Atom "Opderef" -> Opderef
  | Atom "Opassign" -> Opassign
  | Atom "ListAppend" -> ListAppend
  | Atom "Ord" -> Ord
  | Atom "Chr" -> Chr
  | Atom "ConfigGC" -> ConfigGC
  | Atom "Eval" -> Eval
  | Atom "FPbopFPAdd" -> FPbop FPAdd
  | Atom "FPbopFPDiv" -> FPbop FPDiv
  | Atom "FPbopFPMul" -> FPbop FPMul
  | Atom "FPbopFPSub" -> FPbop FPSub
  | Atom "FPuopFPAbs" -> FPuop FPAbs
  | Atom "FPuopFPNeg" -> FPuop FPNeg
  | Atom "FPuopFPSqrt" -> FPuop FPSqrt
  | Atom "FPtopFPFma" -> FPtop FPFma
  | Atom "FpFromWord" -> FpFromWord
  | Atom "FpToWord" -> FpToWord
  (* Test comparison: (Test cmp . type) *)
  | List [Atom "Test"; Atom cmp; Atom "."; Atom typ] ->
    Test (parse_cmp cmp, parse_comp_type typ)
  (* FFI: (FFI . "name") *)
  | List [Atom "FFI"; Atom "."; Atom name] ->
    FFI (unquote name)
  (* Shift: (Shift64Lsl . N) *)
  | List [Atom shift_name; Atom "."; Atom n_str] ->
    parse_shift shift_name (int_of_string n_str)
  | _ -> ast_error "Unknown operator" sexp

and parse_shift name amount =
  match name with
  | "Shift8Lsl" -> Shift (W8, Lsl_, amount)
  | "Shift8Lsr" -> Shift (W8, Lsr_, amount)
  | "Shift8Asr" -> Shift (W8, Asr_, amount)
  | "Shift64Lsl" -> Shift (W64, Lsl_, amount)
  | "Shift64Lsr" -> Shift (W64, Lsr_, amount)
  | "Shift64Asr" -> Shift (W64, Asr_, amount)
  | "Shift64Ror" -> Shift (W64, Ror_, amount)
  | _ -> raise (Ast_error ("Unknown shift: " ^ name))

(* Parse a literal *)
let parse_lit sexp =
  match sexp with
  | Atom s when is_quoted s -> StrLit (unquote s)
  | Atom s ->
    (* Check if it looks like an integer (possibly negative) *)
    (try let _ = int_of_string s in IntLit s
     with _ ->
       (* Could be a very large integer *)
       if s <> "" && (s.[0] = '-' || (s.[0] >= '0' && s.[0] <= '9')) then
         IntLit s
       else
         ast_error "Expected literal" sexp)
  | List [Atom "char"; Atom s] ->
    CharLit (unquote s)
  | List [Atom "word8"; Atom n] ->
    Word8Lit (int_of_string n)
  | List [Atom "word64"; Atom n] ->
    Word64Lit n
  | List [Atom "float64"; Atom n] ->
    Float64Lit n
  | List [Atom "-"; Atom n] ->
    IntLit ("-" ^ n)
  | _ -> ast_error "Expected literal" sexp

(* Parse a pattern *)
let rec parse_pat sexp =
  match sexp with
  | Atom s when is_quoted s -> Pvar (unquote s)
  | Atom "Pany" | List [Atom "Pany"] -> Pany
  | List [Atom "Pcon"; id_opt_sexp; args_sexp] ->
    let id_opt = parse_option parse_id id_opt_sexp in
    let args = parse_pat_list args_sexp in
    Pcon (id_opt, args)
  | List [Atom "Plit"; lit_sexp] ->
    Plit (parse_lit lit_sexp)
  | List (Atom "Pcon" :: id_opt_sexp :: args) ->
    let id_opt = parse_option parse_id id_opt_sexp in
    let pats = List.map parse_pat args in
    Pcon (id_opt, pats)
  | _ -> ast_error "Expected pattern" sexp

and parse_pat_list sexp =
  match sexp with
  | Atom "nil" -> []
  | List items ->
    (* Could be a list of quoted-string patterns (Pvars) or sub-patterns *)
    List.map (fun item ->
      match item with
      | Atom s when is_quoted s -> Pvar (unquote s)
      | _ -> parse_pat item
    ) items
  | _ -> ast_error "Expected pattern list" sexp

(* Parse an expression *)
let rec parse_exp sexp =
  match sexp with
  | Atom s when is_quoted s -> Lit (StrLit (unquote s))
  | List [Atom "Fun"; Atom arg; body] ->
    Fun (unquote arg, parse_exp body)
  | List [Atom "Var"; id_sexp] ->
    Var (parse_id id_sexp)
  | List [Atom "Lit"; lit_sexp] ->
    Lit (parse_lit lit_sexp)
  | List [Atom "Con"; id_opt_sexp; args_sexp] ->
    let id_opt = parse_option parse_id id_opt_sexp in
    let args = parse_exp_list args_sexp in
    Con (id_opt, args)
  | List [Atom "App"; op_sexp; args_sexp] ->
    let op = parse_op op_sexp in
    let args = parse_exp_list args_sexp in
    App (op, args)
  | List [Atom "If"; cond; then_; else_] ->
    If (parse_exp cond, parse_exp then_, parse_exp else_)
  | List [Atom "Let"; name_opt; value; body] ->
    let name = parse_option (function Atom s -> unquote s | s -> ast_error "Expected name" s) name_opt in
    Let (name, parse_exp value, parse_exp body)
  | List [Atom "Log"; Atom "And"; a; b] ->
    Log (And, parse_exp a, parse_exp b)
  | List [Atom "Log"; Atom "Or"; a; b] ->
    Log (Or, parse_exp a, parse_exp b)
  | List [Atom "Raise"; e] ->
    Raise (parse_exp e)
  | List [Atom "Handle"; e; branches_sexp] ->
    Handle (parse_exp e, parse_branches branches_sexp)
  | List [Atom "Mat"; e; branches_sexp] ->
    Mat (parse_exp e, parse_branches branches_sexp)
  | List [Atom "Letrec"; bindings_sexp; body] ->
    Letrec (parse_letrec_bindings bindings_sexp, parse_exp body)
  | _ -> ast_error "Expected expression" sexp

and parse_exp_list sexp =
  match sexp with
  | Atom "nil" -> []
  | List items -> List.map parse_exp items
  | _ -> ast_error "Expected expression list" sexp

(* Parse match/handle branches: ((pat1 body1) (pat2 body2) ...) *)
and parse_branches sexp =
  match sexp with
  | List branches -> List.map parse_branch branches
  | _ -> ast_error "Expected branch list" sexp

and parse_branch sexp =
  (* A branch is a list where the first element(s) form a pattern and the rest form an expression.
     The format is: (pat expr_atoms...)
     where pat can be a quoted string (Pvar), (Pcon ...), (Plit ...), or (Pany)
     The tricky part: the body expression is NOT wrapped in a list - it's inline.
     E.g.: ((Pcon (SOME (Short "None")) nil) Var (Short "v2"))
     means pattern = Pcon(Some(Short "None"), []), body = Var(Short "v2") *)
  match sexp with
  | List items when List.length items >= 2 ->
    parse_branch_items items
  | _ -> ast_error "Expected branch" sexp

and parse_branch_items items =
  (* First, figure out where the pattern ends and the expression begins.
     The pattern is the first element (or could be a bare string for Pvar).
     Then everything after is the expression body, which we need to reconstruct. *)
  match items with
  | pat_sexp :: rest ->
    let pat = parse_pat pat_sexp in
    let body = reconstitute_exp rest in
    (pat, body)
  | [] -> raise (Ast_error "Empty branch")

(* Reconstitute an expression from a flat list of sexp items.
   This handles cases where the expression is "inline" in a branch/binding,
   e.g. [Atom "Var"; List [Atom "Short"; Atom "\"x\""]] -> Var(Short "x")
   or [Atom "App"; Atom "Opapp"; List [...]] -> App(Opapp, [...]) *)
and reconstitute_exp items =
  match items with
  | [single] -> parse_exp single
  | Atom "Fun" :: Atom arg :: rest ->
    Fun (unquote arg, reconstitute_exp rest)
  | Atom "Var" :: [id_sexp] ->
    Var (parse_id id_sexp)
  | Atom "Lit" :: [lit_sexp] ->
    Lit (parse_lit lit_sexp)
  | [Atom "Con"; id_opt_sexp; args_sexp] ->
    let id_opt = parse_option parse_id id_opt_sexp in
    Con (id_opt, parse_exp_list args_sexp)
  | Atom "App" :: op_sexp :: [args_sexp] ->
    App (parse_op op_sexp, parse_exp_list args_sexp)
  | Atom "If" :: rest ->
    (* Need to parse 3 sub-expressions from flat list *)
    parse_if_from_flat rest
  | Atom "Let" :: name_opt :: rest ->
    let name = parse_option (function Atom s -> unquote s | s -> ast_error "Expected name" s) name_opt in
    (* rest has value then body inlined *)
    parse_let_from_flat name rest
  | Atom "Log" :: Atom log_op :: rest ->
    let op = match log_op with "And" -> And | "Or" -> Or | _ -> raise (Ast_error ("Unknown log op: " ^ log_op)) in
    (* two sub-expressions *)
    parse_log_from_flat op rest
  | Atom "Raise" :: rest ->
    Raise (reconstitute_exp rest)
  | [Atom "Handle"; e; branches_sexp] ->
    Handle (parse_exp e, parse_branches branches_sexp)
  | [Atom "Mat"; e; branches_sexp] ->
    Mat (parse_exp e, parse_branches branches_sexp)
  (* Atom "Mat" case covered above *)
  | [Atom "Letrec"; bindings_sexp; body] ->
    Letrec (parse_letrec_bindings bindings_sexp, parse_exp body)
  | _ ->
    (* Try wrapping in a List and parsing *)
    parse_exp (List items)

(* These handle inline expressions where sub-expressions are not clearly delimited.
   In practice, the CakeML sexpr format uses parenthesization for sub-expressions,
   so most of these cases reduce to simple parsing. *)
and parse_if_from_flat rest =
  match rest with
  | [cond; then_; else_] ->
    If (parse_exp cond, parse_exp then_, parse_exp else_)
  | _ ->
    (* The condition is the first sexp, then we need to split the rest *)
    ast_error "Cannot parse if from flat list" (List (Atom "If" :: rest))

and parse_let_from_flat name rest =
  match rest with
  | [value; body] ->
    Let (name, parse_exp value, parse_exp body)
  | value :: body_rest when List.length body_rest > 1 ->
    Let (name, parse_exp value, reconstitute_exp body_rest)
  | _ ->
    ast_error "Cannot parse let from flat list" (List (Atom "Let" :: rest))

and parse_log_from_flat op rest =
  match rest with
  | [a; b] -> Log (op, parse_exp a, parse_exp b)
  | _ -> ast_error "Cannot parse log from flat list" (List rest)

(* Parse letrec bindings: ((name arg body...) (name2 arg2 body2...)) *)
and parse_letrec_bindings sexp =
  match sexp with
  | List bindings -> List.map parse_letrec_binding bindings
  | _ -> ast_error "Expected letrec bindings" sexp

and parse_letrec_binding sexp =
  match sexp with
  | List (Atom name :: Atom arg :: body_parts) ->
    let name = unquote name in
    let arg = unquote arg in
    let body = reconstitute_exp body_parts in
    (name, arg, body)
  | _ -> ast_error "Expected letrec binding" sexp

(* Parse location: (unk unk) or ((n m) (n m)) - we skip these *)
let _parse_loc _sexp = ()

(* Parse a type definition constructor: ("C" type1 type2 ...) or ("C") *)
let parse_constr sexp =
  match sexp with
  | List (Atom name :: types) ->
    let name = unquote name in
    let types = List.map parse_type types in
    (name, types)
  | _ -> ast_error "Expected constructor" sexp

(* Parse a single type definition: ((params) name (C1 ...) (C2 ...)) *)
let rec parse_typedef sexp =
  match sexp with
  | List (params_sexp :: Atom name :: constrs) ->
    let params = parse_type_params params_sexp in
    let name = unquote name in
    let constrs = List.map parse_constr constrs in
    (params, name, constrs)
  | _ -> ast_error "Expected type definition" sexp

and parse_type_params sexp =
  match sexp with
  | Atom "nil" -> []
  | List items -> List.map (function Atom s -> unquote s | s -> ast_error "Expected type param" s) items
  | _ -> ast_error "Expected type params" sexp

(* Parse a declaration *)
let rec parse_dec sexp =
  match sexp with
  | List (Atom "Dtype" :: _loc :: [types_sexp]) ->
    let types = match types_sexp with
      | List items -> List.map parse_typedef items
      | _ -> ast_error "Expected type defs" types_sexp
    in
    Dtype types
  | List [Atom "Dlet"; _loc; Atom name; expr] when is_quoted name ->
    DletSimple (unquote name, parse_exp expr)
  | List [Atom "Dlet"; _loc; pat_sexp; expr] ->
    Dlet (parse_pat pat_sexp, parse_exp expr)
  | List [Atom "Dletrec"; _loc; bindings_sexp] ->
    Dletrec (parse_letrec_bindings bindings_sexp)
  | List [Atom "Dexn"; _loc; Atom name; types_sexp] ->
    let name = unquote name in
    let types = match types_sexp with
      | Atom "nil" -> []
      | List items -> List.map parse_type items
      | _ -> ast_error "Expected types" types_sexp
    in
    Dexn (name, types)
  | List [Atom "Dtabbrev"; _loc; params_sexp; Atom name; typ_sexp] ->
    let params = parse_type_params params_sexp in
    let name = unquote name in
    Dtabbrev (params, name, parse_type typ_sexp)
  | List [Atom "Dmod"; Atom name; decls_sexp] ->
    let name = unquote name in
    let decls = parse_dec_list decls_sexp in
    Dmod (name, decls)
  | List [Atom "Dlocal"; private_sexp; public_sexp] ->
    let priv = parse_dec_list private_sexp in
    let pub = parse_dec_list public_sexp in
    Dlocal (priv, pub)
  | List [Atom "Denv"; Atom name] ->
    Denv (unquote name)
  | _ -> ast_error "Expected declaration" sexp

and parse_dec_list sexp =
  match sexp with
  | List items -> List.map parse_dec items
  | _ -> ast_error "Expected declaration list" sexp

(* Parse the entire program from the outer list *)
let parse_program sexps =
  match sexps with
  | [List items] -> List.map parse_dec items
  | items -> List.map parse_dec items
