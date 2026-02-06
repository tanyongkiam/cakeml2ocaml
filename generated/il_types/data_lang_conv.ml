(* Conversion functions between Data_lang clean types and
   M_to_dataProg generated types in cake64.ml *)

module G = Cake64
open Common_conv

let rec prog_of_gen : G.M_to_dataProg.dataLang_prog -> Data_lang.prog = function
  | G.M_to_dataProg.Force (opt, a, b) ->
    Data_lang.Force (Option.map (fun (n, s) -> (n, sptree_spt_of_gen s)) opt, a, b)
  | G.M_to_dataProg.Tick -> Data_lang.Tick
  | G.M_to_dataProg.Return n -> Data_lang.Return n
  | G.M_to_dataProg.Raise_1 n -> Data_lang.Raise n
  | G.M_to_dataProg.Makespace (n, s) -> Data_lang.Makespace (n, sptree_spt_of_gen s)
  | G.M_to_dataProg.If_1 (n, t, f) -> Data_lang.If (n, prog_of_gen t, prog_of_gen f)
  | G.M_to_dataProg.Seq (a, b) -> Data_lang.Seq (prog_of_gen a, prog_of_gen b)
  | G.M_to_dataProg.Assign (n, o, args, cut) ->
    Data_lang.Assign (n, Clos_lang_conv.op_of_gen o, args,
      Option.map sptree_spt_of_gen cut)
  | G.M_to_dataProg.Call (ret, dest, args, handler) ->
    Data_lang.Call
      (Option.map (fun (n, s) -> (n, sptree_spt_of_gen s)) ret,
       dest, args,
       Option.map (fun (n, p) -> (n, prog_of_gen p)) handler)
  | G.M_to_dataProg.Move (a, b) -> Data_lang.Move (a, b)
  | G.M_to_dataProg.Skip -> Data_lang.Skip

let rec prog_to_gen : Data_lang.prog -> G.M_to_dataProg.dataLang_prog = function
  | Data_lang.Force (opt, a, b) ->
    G.M_to_dataProg.Force (Option.map (fun (n, s) -> (n, sptree_spt_to_gen s)) opt, a, b)
  | Data_lang.Tick -> G.M_to_dataProg.Tick
  | Data_lang.Return n -> G.M_to_dataProg.Return n
  | Data_lang.Raise n -> G.M_to_dataProg.Raise_1 n
  | Data_lang.Makespace (n, s) -> G.M_to_dataProg.Makespace (n, sptree_spt_to_gen s)
  | Data_lang.If (n, t, f) -> G.M_to_dataProg.If_1 (n, prog_to_gen t, prog_to_gen f)
  | Data_lang.Seq (a, b) -> G.M_to_dataProg.Seq (prog_to_gen a, prog_to_gen b)
  | Data_lang.Assign (n, o, args, cut) ->
    G.M_to_dataProg.Assign (n, Clos_lang_conv.op_to_gen o, args,
      Option.map sptree_spt_to_gen cut)
  | Data_lang.Call (ret, dest, args, handler) ->
    G.M_to_dataProg.Call
      (Option.map (fun (n, s) -> (n, sptree_spt_to_gen s)) ret,
       dest, args,
       Option.map (fun (n, p) -> (n, prog_to_gen p)) handler)
  | Data_lang.Move (a, b) -> G.M_to_dataProg.Move (a, b)
  | Data_lang.Skip -> G.M_to_dataProg.Skip
