type sexp = Atom of string | List of sexp list

exception Parse_error of string

let parse_error msg pos =
  raise (Parse_error (Printf.sprintf "%s at position %d" msg pos))

type token = LPAREN | RPAREN | STRING of string | ATOM of string | DOT

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_atom_char c =
  not (is_whitespace c || c = '(' || c = ')' || c = '"')

let hex_digit c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let is_upper_hex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')

let is_print c = let code = Char.code c in code >= 32 && code <= 126

(* decode_control: second-level escape processing matching CakeML's fromSexpTheory.
   Converts \XX (where XX are uppercase hex and decoded char is non-printable) to actual bytes.
   \\ becomes literal \ (already handled by first level, but needed for strings
   that have literal backslashes). *)
let decode_control s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '\\' then begin
      if !i + 1 < len && s.[!i + 1] = '\\' then begin
        (* \\ → literal backslash *)
        Buffer.add_char buf '\\';
        i := !i + 2
      end else if !i + 2 < len && is_upper_hex s.[!i + 1] && is_upper_hex s.[!i + 2] then begin
        let h1 = hex_digit s.[!i + 1] in
        let h2 = hex_digit s.[!i + 2] in
        let byte = h1 * 16 + h2 in
        if not (is_print (Char.chr byte)) then begin
          Buffer.add_char buf (Char.chr byte);
          i := !i + 3
        end else begin
          (* Printable: not a valid decode_control escape, keep as-is *)
          Buffer.add_char buf s.[!i];
          incr i
        end
      end else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let tokenize (s : string) : (token * int) list =
  let len = String.length s in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    if is_whitespace c then
      incr i
    else match c with
    | '(' -> tokens := (LPAREN, !i) :: !tokens; incr i
    | ')' -> tokens := (RPAREN, !i) :: !tokens; incr i
    | '"' ->
      let start = !i in
      incr i;
      let buf = Buffer.create 64 in
      while !i < len && s.[!i] <> '"' do
        if s.[!i] = '\\' then begin
          incr i;
          if !i >= len then parse_error "Unexpected end of string escape" start;
          match s.[!i] with
          | '\\' -> Buffer.add_char buf '\\'; incr i
          | '"' -> Buffer.add_char buf '"'; incr i
          | c -> Buffer.add_char buf '\\'; Buffer.add_char buf c; incr i
        end else begin
          Buffer.add_char buf s.[!i]; incr i
        end
      done;
      if !i >= len then parse_error "Unterminated string" start;
      incr i; (* skip closing quote *)
      tokens := (STRING (decode_control (Buffer.contents buf)), start) :: !tokens
    | _ ->
      let start = !i in
      let buf = Buffer.create 16 in
      while !i < len && is_atom_char s.[!i] do
        Buffer.add_char buf s.[!i]; incr i
      done;
      let atom = Buffer.contents buf in
      if atom = "." then
        tokens := (DOT, start) :: !tokens
      else
        tokens := (ATOM atom, start) :: !tokens
  done;
  List.rev !tokens

let parse (s : string) : sexp list =
  let tokens = tokenize s in
  let pos = ref 0 in
  let toks = ref (Array.of_list tokens) in
  let len = Array.length !toks in
  let cur () = if !pos < len then Some (fst (!toks).(!pos)) else None in
  let cur_pos () = if !pos < len then snd (!toks).(!pos) else String.length s in
  let advance () = incr pos in
  let rec parse_sexp () =
    match cur () with
    | None -> parse_error "Unexpected end of input" (cur_pos ())
    | Some LPAREN ->
      advance ();
      let items = parse_list () in
      (match cur () with
       | Some RPAREN -> advance (); List items
       | _ -> parse_error "Expected )" (cur_pos ()))
    | Some (STRING s) -> advance (); Atom ("\"" ^ s ^ "\"")
    | Some (ATOM s) -> advance (); Atom s
    | Some DOT -> advance (); Atom "."
    | Some RPAREN -> parse_error "Unexpected )" (cur_pos ())
  and parse_list () =
    let items = ref [] in
    while (match cur () with Some RPAREN | None -> false | _ -> true) do
      items := parse_sexp () :: !items
    done;
    List.rev !items
  in
  let results = ref [] in
  while !pos < len do
    results := parse_sexp () :: !results
  done;
  List.rev !results

let parse_file (filename : string) : sexp list =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  parse (Bytes.to_string s)

let rec sexp_to_string = function
  | Atom s -> s
  | List items ->
    "(" ^ String.concat " " (List.map sexp_to_string items) ^ ")"
