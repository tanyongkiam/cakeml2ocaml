(* Conversion functions between Stack_lang clean types and
   G.M_to_target64Prog.stackLang_prog in cake64.ml *)

module G = Cake64
open Common_conv

(* stackLang_prog: If_1/Seq_2/Raise_1/Set_1/Ffi_1 <-> clean *)

let rec prog_of_gen : G.M_to_target64Prog.stackLang_prog -> Stack_lang.prog = function
  | G.M_to_target64Prog.Halt r -> Stack_lang.Halt r
  | G.M_to_target64Prog.Bitmapload (a, b) -> Stack_lang.Bitmapload (a, b)
  | G.M_to_target64Prog.Stacksetsize n -> Stack_lang.Stacksetsize n
  | G.M_to_target64Prog.Stackgetsize r -> Stack_lang.Stackgetsize r
  | G.M_to_target64Prog.Stackloadany (a, b) -> Stack_lang.Stackloadany (a, b)
  | G.M_to_target64Prog.Stackload (a, b) -> Stack_lang.Stackload (a, b)
  | G.M_to_target64Prog.Stackstoreany (a, b) -> Stack_lang.Stackstoreany (a, b)
  | G.M_to_target64Prog.Stackstore (a, b) -> Stack_lang.Stackstore (a, b)
  | G.M_to_target64Prog.Stackfree n -> Stack_lang.Stackfree n
  | G.M_to_target64Prog.Stackalloc n -> Stack_lang.Stackalloc n
  | G.M_to_target64Prog.Rawcall n -> Stack_lang.Rawcall n
  | G.M_to_target64Prog.Databufferwrite (a, b) -> Stack_lang.Databufferwrite (a, b)
  | G.M_to_target64Prog.Codebufferwrite (a, b) -> Stack_lang.Codebufferwrite (a, b)
  | G.M_to_target64Prog.Shmemop (m, r, a) ->
    Stack_lang.Shmemop (asm_memop_of_gen m, r, asm_addr_of_gen a)
  | G.M_to_target64Prog.Install (a, b, c, d, e) -> Stack_lang.Install (a, b, c, d, e)
  | G.M_to_target64Prog.Locvalue (a, b, c) -> Stack_lang.Locvalue (a, b, c)
  | G.M_to_target64Prog.Tick -> Stack_lang.Tick
  | G.M_to_target64Prog.Ffi_1 (s, a, b, c, d, e) -> Stack_lang.Ffi (s, a, b, c, d, e)
  | G.M_to_target64Prog.Return r -> Stack_lang.Return r
  | G.M_to_target64Prog.Raise_1 r -> Stack_lang.Raise r
  | G.M_to_target64Prog.Storeconsts (a, b, c) -> Stack_lang.Storeconsts (a, b, c)
  | G.M_to_target64Prog.Alloc n -> Stack_lang.Alloc n
  | G.M_to_target64Prog.Jumplower (a, b, c) -> Stack_lang.Jumplower (a, b, c)
  | G.M_to_target64Prog.While (c, r, ri, p) ->
    Stack_lang.While (asm_cmp_of_gen c, r, asm_reg_imm_of_gen ri, prog_of_gen p)
  | G.M_to_target64Prog.If_1 (c, r, ri, t, f) ->
    Stack_lang.If (asm_cmp_of_gen c, r, asm_reg_imm_of_gen ri, prog_of_gen t, prog_of_gen f)
  | G.M_to_target64Prog.Seq_2 (a, b) -> Stack_lang.Seq (prog_of_gen a, prog_of_gen b)
  | G.M_to_target64Prog.Call (ret, dest, handler) ->
    Stack_lang.Call
      ( (match ret with
         | None -> None
         | Some (p, (n1, (n2, n3))) -> Some (prog_of_gen p, (n1, (n2, n3)))),
        (match dest with G.Inl n -> Common.Inl n | G.Inr n -> Common.Inr n),
        (match handler with
         | None -> None
         | Some (p, (n1, n2)) -> Some (prog_of_gen p, (n1, n2))) )
  | G.M_to_target64Prog.Opcurrheap (op, a, b) ->
    Stack_lang.Opcurrheap (asm_binop_of_gen op, a, b)
  | G.M_to_target64Prog.Set_1 (sn, r) -> Stack_lang.Set (store_name_of_gen sn, r)
  | G.M_to_target64Prog.Get (r, sn) -> Stack_lang.Get (r, store_name_of_gen sn)
  | G.M_to_target64Prog.Inst i -> Stack_lang.Inst (asm_inst_of_gen i)
  | G.M_to_target64Prog.Skip -> Stack_lang.Skip

let rec prog_to_gen : Stack_lang.prog -> G.M_to_target64Prog.stackLang_prog = function
  | Stack_lang.Halt r -> G.M_to_target64Prog.Halt r
  | Stack_lang.Bitmapload (a, b) -> G.M_to_target64Prog.Bitmapload (a, b)
  | Stack_lang.Stacksetsize n -> G.M_to_target64Prog.Stacksetsize n
  | Stack_lang.Stackgetsize r -> G.M_to_target64Prog.Stackgetsize r
  | Stack_lang.Stackloadany (a, b) -> G.M_to_target64Prog.Stackloadany (a, b)
  | Stack_lang.Stackload (a, b) -> G.M_to_target64Prog.Stackload (a, b)
  | Stack_lang.Stackstoreany (a, b) -> G.M_to_target64Prog.Stackstoreany (a, b)
  | Stack_lang.Stackstore (a, b) -> G.M_to_target64Prog.Stackstore (a, b)
  | Stack_lang.Stackfree n -> G.M_to_target64Prog.Stackfree n
  | Stack_lang.Stackalloc n -> G.M_to_target64Prog.Stackalloc n
  | Stack_lang.Rawcall n -> G.M_to_target64Prog.Rawcall n
  | Stack_lang.Databufferwrite (a, b) -> G.M_to_target64Prog.Databufferwrite (a, b)
  | Stack_lang.Codebufferwrite (a, b) -> G.M_to_target64Prog.Codebufferwrite (a, b)
  | Stack_lang.Shmemop (m, r, a) ->
    G.M_to_target64Prog.Shmemop (asm_memop_to_gen m, r, asm_addr_to_gen a)
  | Stack_lang.Install (a, b, c, d, e) -> G.M_to_target64Prog.Install (a, b, c, d, e)
  | Stack_lang.Locvalue (a, b, c) -> G.M_to_target64Prog.Locvalue (a, b, c)
  | Stack_lang.Tick -> G.M_to_target64Prog.Tick
  | Stack_lang.Ffi (s, a, b, c, d, e) -> G.M_to_target64Prog.Ffi_1 (s, a, b, c, d, e)
  | Stack_lang.Return r -> G.M_to_target64Prog.Return r
  | Stack_lang.Raise r -> G.M_to_target64Prog.Raise_1 r
  | Stack_lang.Storeconsts (a, b, c) -> G.M_to_target64Prog.Storeconsts (a, b, c)
  | Stack_lang.Alloc n -> G.M_to_target64Prog.Alloc n
  | Stack_lang.Jumplower (a, b, c) -> G.M_to_target64Prog.Jumplower (a, b, c)
  | Stack_lang.While (c, r, ri, p) ->
    G.M_to_target64Prog.While (asm_cmp_to_gen c, r, asm_reg_imm_to_gen ri, prog_to_gen p)
  | Stack_lang.If (c, r, ri, t, f) ->
    G.M_to_target64Prog.If_1 (asm_cmp_to_gen c, r, asm_reg_imm_to_gen ri, prog_to_gen t, prog_to_gen f)
  | Stack_lang.Seq (a, b) -> G.M_to_target64Prog.Seq_2 (prog_to_gen a, prog_to_gen b)
  | Stack_lang.Call (ret, dest, handler) ->
    G.M_to_target64Prog.Call
      ( (match ret with
         | None -> None
         | Some (p, (n1, (n2, n3))) -> Some (prog_to_gen p, (n1, (n2, n3)))),
        (match dest with Common.Inl n -> G.Inl n | Common.Inr n -> G.Inr n),
        (match handler with
         | None -> None
         | Some (p, (n1, n2)) -> Some (prog_to_gen p, (n1, n2))) )
  | Stack_lang.Opcurrheap (op, a, b) ->
    G.M_to_target64Prog.Opcurrheap (asm_binop_to_gen op, a, b)
  | Stack_lang.Set (sn, r) -> G.M_to_target64Prog.Set_1 (store_name_to_gen sn, r)
  | Stack_lang.Get (r, sn) -> G.M_to_target64Prog.Get (r, store_name_to_gen sn)
  | Stack_lang.Inst i -> G.M_to_target64Prog.Inst (asm_inst_to_gen i)
  | Stack_lang.Skip -> G.M_to_target64Prog.Skip
