let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <sexpr-file>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  Printf.eprintf "Parsing s-expressions from %s...\n%!" filename;
  let sexps = Cakeml_transpiler_lib.Sexp_parser.parse_file filename in
  Printf.eprintf "Parsed %d top-level s-expressions.\n%!" (List.length sexps);
  Printf.eprintf "Converting to CakeML AST...\n%!";
  let program = Cakeml_transpiler_lib.Ast_parse.parse_program sexps in
  Printf.eprintf "Converted %d declarations.\n%!" (List.length program);
  Printf.eprintf "Generating OCaml code...\n%!";
  let code = Cakeml_transpiler_lib.Ocaml_emit.emit_to_string program in
  Printf.eprintf "Generated %d bytes of OCaml code.\n%!" (String.length code);
  print_string code
