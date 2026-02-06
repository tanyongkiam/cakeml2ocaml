(* A dummy wordLang pass: identity transformation that prints a message *)
open Word_lang

let my_pass (p : prog) : prog =
  Printf.eprintf "[test_word_pass] visited a word program\n%!";
  p
