open Cakeml_ast

let ocaml_keywords = [
  "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
  "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
  "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
  "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
  "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
  "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
  "virtual"; "when"; "while"; "with"; "ref";
]

let is_keyword s = List.mem s ocaml_keywords

(* Sanitize a CakeML name for use in OCaml *)
let sanitize_name s =
  if s = "" then "empty_name_"
  else
    let buf = Buffer.create (String.length s + 1) in
    let needs_prefix = ref false in
    String.iteri (fun i c ->
      match c with
      | ' ' when i = 0 -> needs_prefix := true
      | ' ' -> Buffer.add_char buf '_'
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> Buffer.add_char buf c
      | '@' -> Buffer.add_string buf "_at_"
      | '#' -> Buffer.add_string buf "_hash_"
      | '!' -> Buffer.add_string buf "_bang_"
      | '?' -> Buffer.add_string buf "_q_"
      | '+' -> Buffer.add_string buf "_plus_"
      | '-' -> Buffer.add_string buf "_minus_"
      | '*' -> Buffer.add_string buf "_star_"
      | '/' -> Buffer.add_string buf "_slash_"
      | '<' -> Buffer.add_string buf "_lt_"
      | '>' -> Buffer.add_string buf "_gt_"
      | '=' -> Buffer.add_string buf "_eq_"
      | '~' -> Buffer.add_string buf "_tilde_"
      | '^' -> Buffer.add_string buf "_caret_"
      | '&' -> Buffer.add_string buf "_amp_"
      | '|' -> Buffer.add_string buf "_pipe_"
      | ':' -> Buffer.add_string buf "_colon_"
      | '.' -> Buffer.add_string buf "_dot_"
      | ',' -> Buffer.add_string buf "_comma_"
      | ';' -> Buffer.add_string buf "_semi_"
      | '\\' -> Buffer.add_string buf "_bslash_"
      | _ -> Buffer.add_string buf (Printf.sprintf "_x%02x_" (Char.code c))
    ) s;
    let result = Buffer.contents buf in
    let result = if !needs_prefix then "_" ^ result else result in
    (* Ensure starts with lowercase letter or _ *)
    let result =
      if result <> "" && result.[0] >= 'A' && result.[0] <= 'Z' then
        "_" ^ result
      else result
    in
    if is_keyword result then result ^ "'"
    else result

(* Sanitize a constructor/module name (must start uppercase) *)
let sanitize_con_name s =
  if s = "" then "Empty_con_"
  else
    (* For constructor names, we sanitize special chars but keep the first letter uppercase *)
    let buf = Buffer.create (String.length s + 1) in
    String.iteri (fun _i c ->
      match c with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> Buffer.add_char buf c
      | ' ' -> Buffer.add_char buf '_'
      | '@' -> Buffer.add_string buf "_at_"
      | '#' -> Buffer.add_string buf "_hash_"
      | '+' -> Buffer.add_string buf "_plus_"
      | '-' -> Buffer.add_string buf "_minus_"
      | '*' -> Buffer.add_string buf "_star_"
      | '/' -> Buffer.add_string buf "_slash_"
      | '.' -> Buffer.add_string buf "_dot_"
      | _ -> Buffer.add_string buf (Printf.sprintf "_x%02x_" (Char.code c))
    ) s;
    let result = Buffer.contents buf in
    (* Ensure starts with uppercase *)
    if result = "" then "Empty_con_"
    else if result.[0] >= 'A' && result.[0] <= 'Z' then result
    else if result.[0] >= 'a' && result.[0] <= 'z' then
      String.make 1 (Char.chr (Char.code result.[0] - 32)) ^ String.sub result 1 (String.length result - 1)
    else "C_" ^ result

let sanitize_module_name s =
  if s = "" then "Empty_mod_"
  else
    let first = s.[0] in
    if first >= 'A' && first <= 'Z' then
      let buf = Buffer.create (String.length s) in
      String.iter (fun c ->
        match c with
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char buf c
        | _ -> Buffer.add_string buf (Printf.sprintf "_x%02x_" (Char.code c))
      ) s;
      Buffer.contents buf
    else
      "M_" ^ sanitize_name s

(* Track defined type names to detect redundant re-exports from Dlocal flattening *)
let defined_types : (string, bool) Hashtbl.t = Hashtbl.create 128

(* Track type arities (number of type params) for phantom type arg stripping *)
let type_arities : (string, int) Hashtbl.t = Hashtbl.create 128

(* Map long (theory-prefixed) type names to short (defined) type names.
   CakeML's translator produces names like "balanced_map_balanced_map" (theory_type)
   but defines the type as just "balanced_map". *)
let type_long_to_short : (string, string) Hashtbl.t = Hashtbl.create 128

(* Try to resolve a potentially long type name to a known short name.
   The pattern is theoryName_typeName where typeName is a defined type.
   We try all possible splits at underscores. *)
let resolve_type_name name =
  let sname = sanitize_name name in
  if Hashtbl.mem type_arities sname then sname
  else match Hashtbl.find_opt type_long_to_short sname with
  | Some short -> short
  | None ->
    (* Try splitting at each underscore to find a suffix that is a known type *)
    let rec try_splits s pos =
      match String.index_from_opt s pos '_' with
      | None -> sname (* no resolution found, use as-is *)
      | Some i ->
        let suffix = String.sub s (i + 1) (String.length s - i - 1) in
        if Hashtbl.mem type_arities suffix then begin
          Hashtbl.replace type_long_to_short sname suffix;
          suffix
        end else
          try_splits s (i + 1)
    in
    try_splits sname 0

(* Counter for unique Dlocal private module names *)
let dlocal_counter = ref 0

(* Buffer-based output *)
type emitter = {
  buf: Buffer.t;
  mutable indent: int;
}

let create_emitter () = { buf = Buffer.create (1024 * 1024); indent = 0 }

let emit e s = Buffer.add_string e.buf s
let emit_char e c = Buffer.add_char e.buf c
let emit_newline e =
  emit_char e '\n';
  for _ = 1 to e.indent * 2 do emit_char e ' ' done

let emit_indent e f =
  e.indent <- e.indent + 1;
  f ();
  e.indent <- e.indent - 1

let contents e = Buffer.contents e.buf

(* Emit a Word64 literal - handle values > Int64.max_int by converting to signed *)
let emit_word64_lit e s =
  (* Try to parse as a zarith value and check if it fits in int64 *)
  let z = Z.of_string s in
  let max_int64 = Z.of_string "9223372036854775807" in
  if Z.leq z max_int64 then begin
    emit e s;
    emit e "L"
  end else begin
    (* Convert unsigned to signed: value - 2^64 *)
    let two64 = Z.of_string "18446744073709551616" in
    let signed = Z.sub z two64 in
    emit e "(";
    emit e (Z.to_string signed);
    emit e "L)"
  end

(* Escape a string for OCaml string literal *)
let ocaml_escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c when Char.code c < 32 || Char.code c > 126 ->
      Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* Convert a CakeML char representation to the actual character *)
let cakeml_char_to_char s =
  if String.length s = 1 then
    s.[0]
  else if String.length s = 3 && s.[0] = '\\' then
    (* \XX hex escape *)
    let h1 = match s.[1] with
      | '0'..'9' -> Char.code s.[1] - Char.code '0'
      | 'a'..'f' -> Char.code s.[1] - Char.code 'a' + 10
      | 'A'..'F' -> Char.code s.[1] - Char.code 'A' + 10
      | _ -> -1
    in
    let h2 = match s.[2] with
      | '0'..'9' -> Char.code s.[2] - Char.code '0'
      | 'a'..'f' -> Char.code s.[2] - Char.code 'a' + 10
      | 'A'..'F' -> Char.code s.[2] - Char.code 'A' + 10
      | _ -> -1
    in
    if h1 >= 0 && h2 >= 0 then Char.chr (h1 * 16 + h2)
    else s.[0] (* fallback *)
  else if String.length s = 2 && s.[0] = '\\' then
    match s.[1] with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\\' -> '\\'
    | c -> c
  else s.[0]  (* fallback *)

(* Escape a char for OCaml char literal *)
let ocaml_escape_char s =
  let c = cakeml_char_to_char s in
  match c with
  | '\\' -> "\\\\"
  | '\'' -> "\\'"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | c when Char.code c < 32 || Char.code c > 126 ->
    Printf.sprintf "\\x%02x" (Char.code c)
  | c -> String.make 1 c

(* Emit an identifier *)
let rec emit_id e id =
  match id with
  | Short s -> emit e (sanitize_name s)
  | Long (m, id) ->
    emit e (sanitize_module_name m);
    emit_char e '.';
    emit_id e id

(* Emit a type *)
let rec emit_type e typ =
  match typ with
  | Atvar s ->
    emit_char e '\'';
    (* Remove leading ' if present *)
    let s = if s <> "" && s.[0] = '\'' then String.sub s 1 (String.length s - 1) else s in
    emit e s
  | Atapp ([], Short "list") -> emit e " list"
  | Atapp ([arg], Short "list") ->
    emit_type e arg; emit e " list"
  | Atapp ([arg], Short "option") ->
    emit_type e arg; emit e " option"
  | Atapp ([arg], Short "ref") ->
    emit_type e arg; emit e " ref"
  | Atapp ([], Short "string") -> emit e "string"
  | Atapp ([], Short "char") -> emit e "char"
  | Atapp ([], Short "int") -> emit e "Z.t"
  | Atapp ([], Short "bool") -> emit e "bool"
  | Atapp ([], Short "word8") -> emit e "int"
  | Atapp ([], Short "word64") -> emit e "int64"
  | Atapp ([], Short "word8array") -> emit e "bytes"
  | Atapp ([], Short "double") -> emit e "float"
  | Atapp ([], Short "vector") -> emit e "Obj.t array"
  | Atapp ([arg], Short "vector") ->
    emit_type e arg; emit e " array"
  | Atapp ([], Short "array") -> emit e "Obj.t array"
  | Atapp ([arg], Short "array") ->
    emit_type e arg; emit e " array"
  | Atapp ([], id) ->
    (* Resolve long type names like balanced_map_balanced_map -> balanced_map *)
    let resolved_id = match id with
      | Short s ->
        let resolved = resolve_type_name s in
        if resolved <> sanitize_name s then Short resolved else id
      | _ -> id
    in
    emit_id e resolved_id
  | Atapp (args, id) ->
    (* Resolve long type names and check known arity to trim phantom type args *)
    let id_name = match id with Short s -> sanitize_name s | Long (_, Short s) -> sanitize_name s | _ -> "" in
    let resolved = resolve_type_name id_name in
    let resolved_id = if resolved <> id_name then Short resolved else id in
    let arity = match Hashtbl.find_opt type_arities resolved with Some a -> a | None ->
      match Hashtbl.find_opt type_arities id_name with Some a -> a | None -> List.length args in
    let args = if arity < List.length args then
      List.filteri (fun i _ -> i < arity) args
    else args in
    (match args with
    | [] -> emit_id e resolved_id
    | [arg] -> emit_type e arg; emit_char e ' '; emit_id e resolved_id
    | _ ->
      emit_char e '(';
      List.iteri (fun i arg ->
        if i > 0 then emit e ", ";
        emit_type e arg
      ) args;
      emit e ") ";
      emit_id e resolved_id)
  | Attup [] -> emit e "unit"
  | Attup [t] -> emit_type e t
  | Attup types ->
    emit_char e '(';
    List.iteri (fun i t ->
      if i > 0 then emit e " * ";
      emit_type e t
    ) types;
    emit_char e ')'
  | Atfun (t1, t2) ->
    emit_char e '(';
    emit_type e t1;
    emit e " -> ";
    emit_type e t2;
    emit_char e ')'

(* Check if a pattern contains literal values that can't be used in OCaml patterns *)
let rec pat_has_nonpat_lit pat =
  match pat with
  | Plit (IntLit _) | Plit (Word64Lit _) | Plit (Float64Lit _) -> true
  | Pcon (_, pats) -> List.exists pat_has_nonpat_lit pats
  | Pref p -> pat_has_nonpat_lit p
  | Ptannot (p, _) -> pat_has_nonpat_lit p
  | _ -> false

(* Replace non-pattern-compatible literals with fresh variables, accumulating guards *)
let pat_guard_counter = ref 0
let rec extract_lit_guards pat =
  match pat with
  | Plit (IntLit s) ->
    let n = !pat_guard_counter in
    incr pat_guard_counter;
    let var = Printf.sprintf "_lit_guard_%d_" n in
    (Pvar var, [(var, IntLit s)])
  | Plit (Word64Lit s) ->
    let n = !pat_guard_counter in
    incr pat_guard_counter;
    let var = Printf.sprintf "_lit_guard_%d_" n in
    (Pvar var, [(var, Word64Lit s)])
  | Plit (Float64Lit s) ->
    let n = !pat_guard_counter in
    incr pat_guard_counter;
    let var = Printf.sprintf "_lit_guard_%d_" n in
    (Pvar var, [(var, Float64Lit s)])
  | Pcon (id, pats) ->
    let pats_guards = List.map extract_lit_guards pats in
    let pats' = List.map fst pats_guards in
    let guards = List.concat_map snd pats_guards in
    (Pcon (id, pats'), guards)
  | Pref p ->
    let (p', guards) = extract_lit_guards p in
    (Pref p', guards)
  | Ptannot (p, t) ->
    let (p', guards) = extract_lit_guards p in
    (Ptannot (p', t), guards)
  | p -> (p, [])

(* Check if a constructor name is a built-in *)
let is_builtin_con name =
  match name with
  | "::" | "[]" | "True" | "False" | "None" | "Some"
  | "Bind" | "Div" | "Chr" | "Subscript" -> true
  | _ -> false

let is_builtin_exn name =
  match name with
  | "Bind" | "Div" | "Chr" | "Subscript" -> true
  | _ -> false

(* Check if a type name is built-in *)
let is_builtin_type name =
  match name with
  | "list" | "option" | "bool" | "string" | "char" | "int"
  | "word8" | "word64" | "unit" | "ref" | "vector"
  | "word8array" | "double" | "array" -> true
  | _ -> false

(* Emit a pattern *)
let rec emit_pat e pat =
  match pat with
  | Pvar s -> emit e (sanitize_name s)
  | Pany -> emit e "_"
  | Plit lit -> emit_lit_pat e lit
  | Pcon (None, []) -> emit e "()"
  | Pcon (None, [p]) -> emit_pat e p  (* singleton = no tuple *)
  | Pcon (None, pats) ->
    emit_char e '(';
    List.iteri (fun i p ->
      if i > 0 then emit e ", ";
      emit_pat e p
    ) pats;
    emit_char e ')'
  | Pcon (Some (Short "::"), [h; t]) ->
    emit_char e '(';
    emit_pat e h;
    emit e " :: ";
    emit_pat e t;
    emit_char e ')'
  | Pcon (Some (Short "[]"), []) -> emit e "[]"
  | Pcon (Some (Short "True"), []) -> emit e "true"
  | Pcon (Some (Short "False"), []) -> emit e "false"
  | Pcon (Some (Short "None"), []) -> emit e "None"
  | Pcon (Some (Short "Some"), [p]) ->
    emit e "(Some (";
    emit_pat e p;
    emit e "))"
  | Pcon (Some (Short "Bind"), []) -> emit e "Match_failure (\"\", 0, 0)"
  | Pcon (Some (Short "Div"), []) -> emit e "Division_by_zero"
  | Pcon (Some (Short "Chr"), []) -> emit e "Cakeml_runtime.Chr_exn"
  | Pcon (Some (Short "Subscript"), []) -> emit e "Cakeml_runtime.Subscript_exn"
  | Pcon (Some id, []) ->
    emit_con_id e id
  | Pcon (Some id, [p]) ->
    emit_char e '(';
    emit_con_id e id;
    emit e " (";
    emit_pat e p;
    emit e "))"
  | Pcon (Some id, pats) ->
    emit_char e '(';
    emit_con_id e id;
    emit e " (";
    List.iteri (fun i p ->
      if i > 0 then emit e ", ";
      emit_pat e p
    ) pats;
    emit e "))"
  | Pref p ->
    emit e "(ref ";
    emit_pat e p;
    emit_char e ')'
  | Ptannot (p, _) -> emit_pat e p

and emit_con_id e id =
  match id with
  | Short s when is_builtin_exn s ->
    (match s with
     | "Bind" -> emit e "Match_failure (\"\", 0, 0)"
     | "Div" -> emit e "Division_by_zero"
     | "Chr" -> emit e "Cakeml_runtime.Chr_exn"
     | "Subscript" -> emit e "Cakeml_runtime.Subscript_exn"
     | _ -> emit e (sanitize_con_name s))
  | Short s -> emit e (sanitize_con_name s)
  | Long (m, Short s) ->
    emit e (sanitize_module_name m);
    emit_char e '.';
    emit e (sanitize_con_name s)
  | _ ->
    let buf = Buffer.create 32 in
    let tmp = { buf; indent = 0 } in
    emit_id tmp id;
    emit e (Buffer.contents buf)

and emit_lit_pat e lit =
  match lit with
  | IntLit s -> emit e "(Z.of_string \""; emit e s; emit e "\")"
  | StrLit s -> emit_char e '"'; emit e (ocaml_escape_string s); emit_char e '"'
  | CharLit s -> emit_char e '\''; emit e (ocaml_escape_char s); emit_char e '\''
  | Word8Lit n -> emit e (string_of_int n)
  | Word64Lit s -> emit_word64_lit e s
  | Float64Lit s -> emit e "(Int64.float_of_bits "; emit_word64_lit e s; emit e ")"

(* Emit a literal in expression context *)
let emit_lit e lit =
  match lit with
  | IntLit s ->
    emit e "(Z.of_string \""; emit e s; emit e "\")"
  | StrLit s ->
    emit_char e '"'; emit e (ocaml_escape_string s); emit_char e '"'
  | CharLit s ->
    emit_char e '\''; emit e (ocaml_escape_char s); emit_char e '\''
  | Word8Lit n ->
    emit e (string_of_int n)
  | Word64Lit s ->
    emit_word64_lit e s
  | Float64Lit s ->
    emit e "(Int64.float_of_bits "; emit_word64_lit e s; emit e ")"

(* Emit the operator function name for non-Opapp ops *)
let emit_op_name e op =
  match op with
  | OpnPlus -> emit e "Z.add"
  | OpnMinus -> emit e "Z.sub"
  | OpnTimes -> emit e "Z.mul"
  | OpnDivide -> emit e "Cakeml_runtime.int_div"
  | OpnModulo -> emit e "Cakeml_runtime.int_mod"
  | Equality -> emit e "Cakeml_runtime.poly_equal"
  | Test (cmp, typ) ->
    let cmp_s = match cmp with
      | Equal -> "equal" | Less -> "lt" | Greater -> "gt"
      | LessEq -> "leq" | GreaterEq -> "geq"
    in
    let typ_s = match typ with
      | IntT -> "int" | BoolT -> "bool" | StrT -> "str"
      | CharT -> "char" | Word8T -> "w8" | Word64T -> "w64"
      | Float64T -> "float"
    in
    emit e "Cakeml_runtime.test_";
    emit e cmp_s;
    emit_char e '_';
    emit e typ_s
  | W8fromInt -> emit e "Cakeml_runtime.w8_from_int"
  | W8toInt -> emit e "Cakeml_runtime.w8_to_int"
  | W64fromInt -> emit e "Cakeml_runtime.w64_from_int"
  | W64toInt -> emit e "Cakeml_runtime.w64_to_int"
  | Opw8Add -> emit e "Cakeml_runtime.w8_add"
  | Opw8Sub -> emit e "Cakeml_runtime.w8_sub"
  | Opw8Andw -> emit e "Cakeml_runtime.w8_and"
  | Opw8Orw -> emit e "Cakeml_runtime.w8_or"
  | Opw8Xor -> emit e "Cakeml_runtime.w8_xor"
  | Opw64Add -> emit e "Cakeml_runtime.w64_add"
  | Opw64Sub -> emit e "Cakeml_runtime.w64_sub"
  | Opw64Andw -> emit e "Cakeml_runtime.w64_and"
  | Opw64Orw -> emit e "Cakeml_runtime.w64_or"
  | Opw64Xor -> emit e "Cakeml_runtime.w64_xor"
  | Shift (W8, Lsl_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w8_lsl a %d)" n)
  | Shift (W8, Lsr_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w8_lsr a %d)" n)
  | Shift (W8, Asr_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w8_asr a %d)" n)
  | Shift (W64, Lsl_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w64_lsl a %d)" n)
  | Shift (W64, Lsr_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w64_lsr a %d)" n)
  | Shift (W64, Asr_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w64_asr a %d)" n)
  | Shift (W64, Ror_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w64_ror a %d)" n)
  | Shift (W8, Ror_, n) -> emit e (Printf.sprintf "(fun a -> Cakeml_runtime.w8_ror a %d)" n)
  | Strlen -> emit e "Cakeml_runtime.strlen"
  | Strsub -> emit e "Cakeml_runtime.strsub"
  | Strcat -> emit e "Cakeml_runtime.strcat"
  | Explode -> emit e "Cakeml_runtime.explode"
  | Implode -> emit e "Cakeml_runtime.implode"
  | CopyStrStr -> emit e "Cakeml_runtime.copy_str_str"
  | CopyStrAw8 -> emit e "Cakeml_runtime.copy_str_aw8"
  | CopyAw8Str -> emit e "Cakeml_runtime.copy_aw8_str"
  | CopyAw8Aw8 -> emit e "Cakeml_runtime.copy_aw8_aw8"
  | Aalloc -> emit e "Cakeml_runtime.aalloc"
  | AallocEmpty -> emit e "Cakeml_runtime.aalloc_empty"
  | Asub -> emit e "Cakeml_runtime.asub"
  | Alength -> emit e "Cakeml_runtime.alength"
  | Aupdate -> emit e "Cakeml_runtime.aupdate"
  | Aw8alloc -> emit e "Cakeml_runtime.aw8alloc"
  | Aw8sub -> emit e "Cakeml_runtime.aw8sub"
  | Aw8length -> emit e "Cakeml_runtime.aw8length"
  | Aw8update -> emit e "Cakeml_runtime.aw8update"
  | VfromList -> emit e "Cakeml_runtime.vfrom_list"
  | Vsub -> emit e "Cakeml_runtime.vsub"
  | Vlength -> emit e "Cakeml_runtime.vlength"
  | Opref -> emit e "ref"
  | Opderef -> emit e "(!)"
  | Opassign -> emit e "(:=)"
  | ListAppend -> emit e "(@)"
  | Ord -> emit e "Cakeml_runtime.ord"
  | Chr -> emit e "Cakeml_runtime.chr"
  | FPbop FPAdd -> emit e "Cakeml_runtime.fp_add"
  | FPbop FPDiv -> emit e "Cakeml_runtime.fp_div"
  | FPbop FPMul -> emit e "Cakeml_runtime.fp_mul"
  | FPbop FPSub -> emit e "Cakeml_runtime.fp_sub"
  | FPuop FPAbs -> emit e "Cakeml_runtime.fp_abs"
  | FPuop FPNeg -> emit e "Cakeml_runtime.fp_neg"
  | FPuop FPSqrt -> emit e "Cakeml_runtime.fp_sqrt"
  | FPtop FPFma -> emit e "Cakeml_runtime.fp_fma"
  | FpFromWord -> emit e "Cakeml_runtime.fp_from_word"
  | FpToWord -> emit e "Cakeml_runtime.fp_to_word"
  | ConfigGC -> emit e "Cakeml_runtime.config_gc"
  | Eval -> emit e "Cakeml_runtime.eval"
  | FFI name -> emit e (Printf.sprintf "Cakeml_runtime.ffi_%s" (sanitize_name name))
  | Opapp -> emit e "OPAPP_BUG" (* should not reach here *)

(* Emit an expression *)
let rec emit_exp e exp =
  match exp with
  | Lit lit -> emit_lit e lit
  | Var id -> emit_id e id
  | Fun (arg, body) ->
    emit e "(fun ";
    emit e (sanitize_name arg);
    emit e " -> ";
    emit_indent e (fun () -> emit_exp e body);
    emit_char e ')'
  | App (Opapp, args) -> emit_app e args
  | App (Opref, [arg]) ->
    emit e "(ref ";
    emit_exp e arg;
    emit_char e ')'
  | App (Opderef, [arg]) ->
    emit e "(!(";
    emit_exp e arg;
    emit e "))"
  | App (Opassign, [r; v]) ->
    emit_char e '(';
    emit_exp e r;
    emit e " := ";
    emit_exp e v;
    emit_char e ')'
  | App (ListAppend, [a; b]) ->
    emit_char e '(';
    emit_exp e a;
    emit e " @ ";
    emit_exp e b;
    emit_char e ')'
  | App (op, args) ->
    emit_char e '(';
    emit_op_name e op;
    List.iter (fun arg ->
      emit_char e ' ';
      emit_paren_exp e arg
    ) args;
    emit_char e ')'
  | Con (None, []) -> emit e "()"
  | Con (None, [x]) -> emit_paren_exp e x  (* singleton = not a tuple *)
  | Con (None, args) ->
    emit_char e '(';
    List.iteri (fun i arg ->
      if i > 0 then emit e ", ";
      emit_exp e arg
    ) args;
    emit_char e ')'
  | Con (Some (Short "::"), [h; t]) ->
    emit_char e '(';
    emit_paren_exp e h;
    emit e " :: ";
    emit_exp e t;
    emit_char e ')'
  | Con (Some (Short "[]"), []) -> emit e "[]"
  | Con (Some (Short "True"), []) -> emit e "true"
  | Con (Some (Short "False"), []) -> emit e "false"
  | Con (Some (Short "None"), []) -> emit e "None"
  | Con (Some (Short "Some"), [x]) ->
    emit e "(Some (";
    emit_exp e x;
    emit e "))"
  | Con (Some (Short "Bind"), []) -> emit e "(Match_failure (\"\", 0, 0))"
  | Con (Some (Short "Div"), []) -> emit e "Division_by_zero"
  | Con (Some (Short "Chr"), []) -> emit e "Cakeml_runtime.Chr_exn"
  | Con (Some (Short "Subscript"), []) -> emit e "Cakeml_runtime.Subscript_exn"
  | Con (Some id, []) ->
    emit_con_id e id
  | Con (Some id, [arg]) ->
    emit_char e '(';
    emit_con_id e id;
    emit e " (";
    emit_exp e arg;
    emit e "))"
  | Con (Some id, args) ->
    emit_char e '(';
    emit_con_id e id;
    emit e " (";
    List.iteri (fun i arg ->
      if i > 0 then emit e ", ";
      emit_exp e arg
    ) args;
    emit e "))"
  | If (cond, then_, else_) ->
    emit e "(if ";
    emit_exp e cond;
    emit e " then ";
    emit_indent e (fun () -> emit_exp e then_);
    emit e " else ";
    emit_indent e (fun () -> emit_exp e else_);
    emit_char e ')'
  | Let (Some name, value, body) ->
    emit e "(let ";
    emit e (sanitize_name name);
    emit e " = ";
    emit_indent e (fun () -> emit_exp e value);
    emit e " in ";
    emit_indent e (fun () -> emit_exp e body);
    emit_char e ')'
  | Let (None, value, body) ->
    emit e "(let _ = ";
    emit_indent e (fun () -> emit_exp e value);
    emit e " in ";
    emit_indent e (fun () -> emit_exp e body);
    emit_char e ')'
  | Log (And, a, b) ->
    emit_char e '(';
    emit_exp e a;
    emit e " && ";
    emit_exp e b;
    emit_char e ')'
  | Log (Or, a, b) ->
    emit_char e '(';
    emit_exp e a;
    emit e " || ";
    emit_exp e b;
    emit_char e ')'
  | Mat (exp, branches) ->
    emit e "(match ";
    emit_exp e exp;
    emit e " with";
    emit_indent e (fun () ->
      List.iter (fun (pat, body) ->
        emit_newline e;
        emit e "| ";
        if pat_has_nonpat_lit pat then begin
          let (pat', guards) = extract_lit_guards pat in
          emit_pat e pat';
          emit e " when ";
          List.iteri (fun i (var, lit) ->
            if i > 0 then emit e " && ";
            emit e var;
            emit e " = ";
            emit_lit e lit
          ) guards;
          emit e " -> ";
          emit_indent e (fun () -> emit_exp e body)
        end else begin
          emit_pat e pat;
          emit e " -> ";
          emit_indent e (fun () -> emit_exp e body)
        end
      ) branches
    );
    emit_char e ')'
  | Raise exp ->
    emit e "(raise ";
    emit_paren_exp e exp;
    emit_char e ')'
  | Handle (exp, branches) ->
    emit e "(try ";
    emit_exp e exp;
    emit e " with";
    emit_indent e (fun () ->
      List.iter (fun (pat, body) ->
        emit_newline e;
        emit e "| ";
        if pat_has_nonpat_lit pat then begin
          let (pat', guards) = extract_lit_guards pat in
          emit_pat e pat';
          emit e " when ";
          List.iteri (fun i (var, lit) ->
            if i > 0 then emit e " && ";
            emit e var;
            emit e " = ";
            emit_lit e lit
          ) guards;
          emit e " -> ";
          emit_indent e (fun () -> emit_exp e body)
        end else begin
          emit_pat e pat;
          emit e " -> ";
          emit_indent e (fun () -> emit_exp e body)
        end
      ) branches
    );
    emit_char e ')'
  | Letrec (bindings, body) ->
    emit e "(let rec ";
    List.iteri (fun i (name, arg, fbody) ->
      if i > 0 then begin
        emit_newline e;
        emit e "and "
      end;
      emit e (sanitize_name name);
      emit_char e ' ';
      emit e (sanitize_name arg);
      emit e " = ";
      emit_indent e (fun () -> emit_exp e fbody)
    ) bindings;
    emit e " in ";
    emit_indent e (fun () -> emit_exp e body);
    emit_char e ')'

and emit_paren_exp e exp =
  match exp with
  | Lit _ | Var _ -> emit_exp e exp
  | _ -> emit_char e '('; emit_exp e exp; emit_char e ')'

and emit_app e args =
  match args with
  | [] -> emit e "()"
  | [f] -> emit_char e '('; emit_exp e f; emit_char e ')'
  | [f; x] ->
    emit_char e '(';
    emit_exp e f;
    emit_char e ' ';
    emit_paren_exp e x;
    emit_char e ')'
  | f :: rest ->
    (* Multi-arg application: ((f x1) x2) ... *)
    emit_char e '(';
    emit_app e [f; List.hd rest];
    List.iter (fun arg ->
      emit_char e ' ';
      emit_paren_exp e arg
    ) (List.tl rest);
    emit_char e ')'

(* Check if a type expression refers to a given type name (for detecting self-referential Dtabbrev) *)
let rec type_refers_to name typ =
  match typ with
  | Atvar _ -> false
  | Atapp (args, Short n) -> sanitize_name n = sanitize_name name || List.exists (type_refers_to name) args
  | Atapp (args, Long (_, Short n)) -> sanitize_name n = sanitize_name name || List.exists (type_refers_to name) args
  | Atapp (args, _) -> List.exists (type_refers_to name) args
  | Attup ts -> List.exists (type_refers_to name) ts
  | Atfun (a, b) -> type_refers_to name a || type_refers_to name b

(* Emit a declaration. ~in_module tracks if we're inside a Dmod struct. *)
let rec emit_dec ?(in_module=false) e dec =
  match dec with
  | Dtype tdefs ->
    List.iter (fun (params, name, constrs) ->
      Hashtbl.replace defined_types (sanitize_name name) true;
      Hashtbl.replace type_arities (sanitize_name name) (List.length params);
      if is_builtin_type name then begin
        emit e "(* type "; emit e name; emit e " already defined *)";
        emit_newline e
      end else begin
        emit e "type ";
        (match params with
         | [] -> ()
         | [p] ->
           emit_char e '\'';
           let p = if p <> "" && p.[0] = '\'' then String.sub p 1 (String.length p - 1) else p in
           emit e p;
           emit_char e ' '
         | ps ->
           emit_char e '(';
           List.iteri (fun i p ->
             if i > 0 then emit e ", ";
             emit_char e '\'';
             let p = if p <> "" && p.[0] = '\'' then String.sub p 1 (String.length p - 1) else p in
             emit e p
           ) ps;
           emit e ") ");
        emit e (sanitize_name name);
        emit e " =";
        List.iteri (fun i (cname, types) ->
          if i = 0 then emit e " " else begin emit_newline e; emit e "| " end;
          emit e (sanitize_con_name cname);
          match types with
          | [] -> ()
          | _ ->
            emit e " of ";
            List.iteri (fun j t ->
              if j > 0 then emit e " * ";
              emit_type e t
            ) types
        ) constrs;
        emit_newline e
      end
    ) tdefs

  | DletSimple (name, expr) ->
    emit e "let ";
    emit e (sanitize_name name);
    emit e " = ";
    emit_indent e (fun () -> emit_exp e expr);
    emit_newline e

  | Dlet (pat, expr) ->
    emit e "let ";
    emit_pat e pat;
    emit e " = ";
    emit_indent e (fun () -> emit_exp e expr);
    emit_newline e

  | Dletrec bindings ->
    emit e "let rec ";
    List.iteri (fun i (name, arg, body) ->
      if i > 0 then begin
        emit_newline e;
        emit e "and "
      end;
      emit e (sanitize_name name);
      emit_char e ' ';
      emit e (sanitize_name arg);
      emit e " = ";
      emit_indent e (fun () -> emit_exp e body)
    ) bindings;
    emit_newline e

  | Dexn (name, types) ->
    if is_builtin_exn name then begin
      emit e "(* exception "; emit e name; emit e " already defined *)";
      emit_newline e
    end else begin
      emit e "exception ";
      emit e (sanitize_con_name name);
      (match types with
       | [] -> ()
       | _ ->
         emit e " of ";
         List.iteri (fun i t ->
           if i > 0 then emit e " * ";
           emit_type e t
         ) types);
      emit_newline e
    end

  | Dtabbrev (params, name, typ) ->
    if is_builtin_type name then begin
      emit e "(* type "; emit e name; emit e " already defined *)";
      emit_newline e
    end else begin
      (* Check if the type refers to itself *)
      let sname = sanitize_name name in
      let is_self_ref = type_refers_to name typ in
      let already_defined = Hashtbl.mem defined_types sname in
      if is_self_ref && already_defined then begin
        (* Type already defined in this scope (from Dlocal flattening), skip *)
        emit e "(* type "; emit e sname; emit e " = re-export, skipped *)";
        emit_newline e
      end else begin
        Hashtbl.replace defined_types sname true;
        emit e "type ";
        if is_self_ref then emit e "nonrec ";
        (match params with
         | [] -> ()
         | [p] ->
           emit_char e '\'';
           let p = if p <> "" && p.[0] = '\'' then String.sub p 1 (String.length p - 1) else p in
           emit e p;
           emit_char e ' '
         | ps ->
           emit_char e '(';
           List.iteri (fun i p ->
             if i > 0 then emit e ", ";
             emit_char e '\'';
             let p = if p <> "" && p.[0] = '\'' then String.sub p 1 (String.length p - 1) else p in
             emit e p
           ) ps;
           emit e ") ");
        emit e (sanitize_name name);
        emit e " = ";
        emit_type e typ;
        emit_newline e
      end
    end

  | Dmod (name, decls) ->
    emit e "module ";
    emit e (sanitize_module_name name);
    emit e " = struct";
    emit_newline e;
    (* Save and clear defined_types for the module scope *)
    let saved_types = Hashtbl.copy defined_types in
    Hashtbl.clear defined_types;
    emit_indent e (fun () ->
      List.iter (fun d ->
        emit_dec ~in_module:true e d;
        emit_newline e
      ) decls
    );
    emit e "end";
    emit_newline e;
    (* Restore outer scope's defined_types *)
    Hashtbl.reset defined_types;
    Hashtbl.iter (fun k v -> Hashtbl.replace defined_types k v) saved_types

  | Dlocal (priv, pub) ->
    (* Wrap private decls in a sub-module + open to avoid name conflicts *)
    let n = !dlocal_counter in
    incr dlocal_counter;
    let mod_name = Printf.sprintf "Dlocal_private_%d_" n in
    emit e "module "; emit e mod_name; emit e " = struct";
    emit_newline e;
    let saved_types = Hashtbl.copy defined_types in
    emit_indent e (fun () ->
      List.iter (fun d ->
        emit_dec ~in_module:true e d;
        emit_newline e
      ) priv
    );
    emit e "end";
    emit_newline e;
    (* Restore parent defined_types and add the private module's types *)
    Hashtbl.reset defined_types;
    Hashtbl.iter (fun k v -> Hashtbl.replace defined_types k v) saved_types;
    emit e "open "; emit e mod_name;
    emit_newline e;
    List.iter (fun d -> emit_dec ~in_module e d) pub

  | Denv name ->
    (* Denv captures runtime environment - emit as a placeholder *)
    emit e "let ";
    emit e (sanitize_name name);
    emit e " = Obj.magic ()";
    emit_newline e

let emit_program e program =
  emit e "open Cakeml_runtime";
  emit_newline e;
  emit_newline e;
  List.iter (fun dec ->
    emit_dec e dec;
    emit_newline e
  ) program

let emit_to_string program =
  let e = create_emitter () in
  emit_program e program;
  contents e
