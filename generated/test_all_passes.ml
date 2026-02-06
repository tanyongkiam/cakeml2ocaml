(* Identity passes for all ILs — prints a message per program/entry *)

let flat_pass (d : Flat_lang.dec) : Flat_lang.dec =
  Printf.eprintf "[hook:flat] visited a flat declaration\n%!"; d

let clos_pass (e : Clos_lang.exp) : Clos_lang.exp =
  Printf.eprintf "[hook:clos] visited a clos expression\n%!"; e

let bvl_pass (e : Bvl_lang.exp) : Bvl_lang.exp =
  Printf.eprintf "[hook:bvl] visited a bvl expression\n%!"; e

let bvi_pass (e : Bvi_lang.exp) : Bvi_lang.exp =
  Printf.eprintf "[hook:bvi] visited a bvi expression\n%!"; e

let data_pass (p : Data_lang.prog) : Data_lang.prog =
  Printf.eprintf "[hook:data] visited a data program\n%!"; p

let word_pass (p : Word_lang.prog) : Word_lang.prog =
  Printf.eprintf "[hook:word] visited a word program\n%!"; p

let stack_pass (p : Stack_lang.prog) : Stack_lang.prog =
  Printf.eprintf "[hook:stack] visited a stack program\n%!"; p

let lab_pass (s : Lab_lang.sec) : Lab_lang.sec =
  Printf.eprintf "[hook:lab] visited a lab section\n%!"; s
