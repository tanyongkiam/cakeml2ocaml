(* Mutable hook references for compiler pipeline injection.
   Compiled BEFORE cake64.ml so it can be referenced there.
   Hooks default to identity (Fun.id).

   Each hook has type (Obj.t -> Obj.t) to avoid depending on cake64 types.
   The actual types at each boundary (in backend_compile) are:

     flat_hook : M_to_flatProg.flatLang_dec list
     clos_hook : M_to_closProg.closLang_exp list
     bvl_hook  : (Z.t * (Z.t * M_to_bvlProg.bvl_exp)) list  (v42, not v41 which is names)
     bvi_hook  : (Z.t * (Z.t * M_to_bviProg.bvi_exp)) list
     data_hook : (Z.t * (Z.t * M_to_dataProg.dataLang_prog)) list
     word_hook : (Z.t * (Z.t * M_to_word64Prog.wordLang_prog)) list
     stack_hook: (Z.t * M_to_target64Prog.stackLang_prog) list
     lab_hook  : M_to_target64Prog.labLang_sec list
*)

let flat_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let clos_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let bvl_hook   : (Obj.t -> Obj.t) ref = ref Fun.id
let bvi_hook   : (Obj.t -> Obj.t) ref = ref Fun.id
let data_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let word_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let stack_hook : (Obj.t -> Obj.t) ref = ref Fun.id
let lab_hook   : (Obj.t -> Obj.t) ref = ref Fun.id
