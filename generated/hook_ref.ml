(* Mutable hook references for CakeML compiler pipeline.
   Each hook is a function Obj.t -> Obj.t, initialized to identity.
   The patched cake64.ml calls these hooks at IL boundaries.
   Pass modules set them before main() runs. *)

let flat_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let clos_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let bvl_hook   : (Obj.t -> Obj.t) ref = ref Fun.id
let bvi_hook   : (Obj.t -> Obj.t) ref = ref Fun.id
let data_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let word_hook  : (Obj.t -> Obj.t) ref = ref Fun.id
let stack_hook : (Obj.t -> Obj.t) ref = ref Fun.id
let lab_hook   : (Obj.t -> Obj.t) ref = ref Fun.id

(* Storage refs for data captured at IL boundaries.
   Saved by patched code, read by hooks that run later. *)
let bitmaps : Obj.t ref = ref (Obj.repr ())
let names   : Obj.t ref = ref (Obj.repr ())
