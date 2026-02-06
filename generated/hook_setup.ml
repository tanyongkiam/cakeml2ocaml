(* Register the test word pass into the hook_ref.
   v18 in backend_compile has type:
     (Z.t * (Z.t * Cake64.M_to_word64Prog.wordLang_prog)) list
   We convert each program to clean types, apply the pass, convert back. *)
let () =
  Hook_ref.word_hook := (fun raw ->
    let progs : (Z.t * (Z.t * Cake64.M_to_word64Prog.wordLang_prog)) list = Obj.obj raw in
    let result = List.map (fun (loc, (arity, p)) ->
      let clean = Word_lang_conv.prog_of_gen p in
      let transformed = Test_word_pass.my_pass clean in
      (loc, (arity, Word_lang_conv.prog_to_gen transformed))
    ) progs in
    Obj.repr result)
