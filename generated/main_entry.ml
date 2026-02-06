(* Entry point for the hooked compiler.
   Hook_setup registers the pass at module init time (before this runs).
   Then we call the original main. *)
let () = Cake64.main ()
