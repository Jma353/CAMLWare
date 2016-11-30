open Extensions

(* The Global State *)
let (circ:Circuit.circuit option ref) = ref None

(* Run the app *)
let _ = plz_run (View.init_view circ)
