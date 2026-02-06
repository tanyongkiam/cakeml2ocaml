(* Register identity test passes for ALL IL hook points.
   Each hook converts from generated types to clean types, applies the pass,
   and converts back. *)

module G = Cake64

(* flat: v50 has type flatLang_dec list *)
let () =
  Hook_ref.flat_hook := (fun raw ->
    let decs : G.M_to_flatProg.flatLang_dec list = Obj.obj raw in
    let result = List.map (fun d ->
      Flat_lang_conv.dec_to_gen (Test_all_passes.flat_pass (Flat_lang_conv.dec_of_gen d))
    ) decs in
    Obj.repr result)

(* clos: v47 has type closLang_exp list *)
let () =
  Hook_ref.clos_hook := (fun raw ->
    let exps : G.M_to_closProg.closLang_exp list = Obj.obj raw in
    let result = List.map (fun e ->
      Clos_lang_conv.exp_to_gen (Test_all_passes.clos_pass (Clos_lang_conv.exp_of_gen e))
    ) exps in
    Obj.repr result)

(* bvl: v42 has type (Z.t * (Z.t * bvl_exp)) list *)
let () =
  Hook_ref.bvl_hook := (fun raw ->
    let progs : (Z.t * (Z.t * G.M_to_bvlProg.bvl_exp)) list = Obj.obj raw in
    let result = List.map (fun (name, (arity, e)) ->
      let clean = Bvl_lang_conv.exp_of_gen e in
      let transformed = Test_all_passes.bvl_pass clean in
      (name, (arity, Bvl_lang_conv.exp_to_gen transformed))
    ) progs in
    Obj.repr result)

(* bvi: v35 has type (Z.t * (Z.t * bvi_exp)) list *)
let () =
  Hook_ref.bvi_hook := (fun raw ->
    let progs : (Z.t * (Z.t * G.M_to_bviProg.bvi_exp)) list = Obj.obj raw in
    let result = List.map (fun (name, (arity, e)) ->
      let clean = Bvi_lang_conv.exp_of_gen e in
      let transformed = Test_all_passes.bvi_pass clean in
      (name, (arity, Bvi_lang_conv.exp_to_gen transformed))
    ) progs in
    Obj.repr result)

(* data: v22 has type (Z.t * (Z.t * dataLang_prog)) list *)
let () =
  Hook_ref.data_hook := (fun raw ->
    let progs : (Z.t * (Z.t * G.M_to_dataProg.dataLang_prog)) list = Obj.obj raw in
    let result = List.map (fun (name, (arity, p)) ->
      let clean = Data_lang_conv.prog_of_gen p in
      let transformed = Test_all_passes.data_pass clean in
      (name, (arity, Data_lang_conv.prog_to_gen transformed))
    ) progs in
    Obj.repr result)

(* word: v18 has type (Z.t * (Z.t * wordLang_prog)) list *)
let () =
  Hook_ref.word_hook := (fun raw ->
    let progs : (Z.t * (Z.t * G.M_to_word64Prog.wordLang_prog)) list = Obj.obj raw in
    let result = List.map (fun (loc, (arity, p)) ->
      let clean = Word_lang_conv.prog_of_gen p in
      let transformed = Test_all_passes.word_pass clean in
      (loc, (arity, Word_lang_conv.prog_to_gen transformed))
    ) progs in
    Obj.repr result)

(* stack: v7 has type (Z.t * stackLang_prog) list *)
let () =
  Hook_ref.stack_hook := (fun raw ->
    let progs : (Z.t * G.M_to_target64Prog.stackLang_prog) list = Obj.obj raw in
    let result = List.map (fun (name, p) ->
      let clean = Stack_lang_conv.prog_of_gen p in
      let transformed = Test_all_passes.stack_pass clean in
      (name, Stack_lang_conv.prog_to_gen transformed)
    ) progs in
    Obj.repr result)

(* lab: v4 has type labLang_sec list *)
let () =
  Hook_ref.lab_hook := (fun raw ->
    let secs : G.M_to_target64Prog.labLang_sec list = Obj.obj raw in
    let result = List.map (fun s ->
      let clean = Lab_lang_conv.sec_of_gen s in
      let transformed = Test_all_passes.lab_pass clean in
      Lab_lang_conv.sec_to_gen transformed
    ) secs in
    Obj.repr result)
