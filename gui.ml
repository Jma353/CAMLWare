(* Run the app *)
let _ =
  (* Empty initial storage *)
  Storage.set Model.to_json (Model.make None);
  (* Present view *)
  Extensions.plz_run (View.init_view circ)
