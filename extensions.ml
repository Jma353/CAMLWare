(* Extensions
 *
 * Extensions is a utility file that contains extensions to the js_of_ocaml
 * bindings for D3, as well as some helper functions related to working
 * with JavaScript-esque functionality in OCaml *)


(* Global references *)
let d3 = Js.Unsafe.variable "d3"
let d3_svg   = d3##svg
let d3_scale = d3##scale

(* Alias for useful wrapper of method callbacks *)
let mb = Js.wrap_meth_callback

(* [translate x y] expresses the "transform" command of translation,
 * given an x and a y. *)
let translate x y =
  "translate(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")"

(* OCaml to coordinates in JavaScript *)
let _d lst =
  let make_coord x y =
    let c = Js.Unsafe.obj [||] in
    c##x <- x; c##y <- y;
    c in
  let n = List.length lst in
  let arr = Js.(jsnew array_length (n)) in
  let rec add_vals l i =
    match l with
    | [] -> ()
    | h::k -> Js.array_set arr i (make_coord (fst h) (snd h));
              add_vals k (i+1) in
  add_vals lst 0;
  arr

(* Create a linear scaling function *)
let linear dom rng =
  let lin = Js.Unsafe.new_obj (d3_scale##linear) [||] in
  let _ = (Js.Unsafe.(meth_call lin "domain"
    [| inject (Array.of_list [fst dom; snd dom]) |])) in
  let _ = (Js.Unsafe.(meth_call lin "range"
    [| inject (Array.of_list [fst rng; snd rng]) |])) in
  lin

(* Wrapper for prompt(query, default) *)
let prompt query default =
  let a = (Js.Unsafe.(meth_call Dom_html.window "prompt"
    [| inject (Js.string query); inject (Js.string default) |])) in
  a |> Js.to_string

(* Wrapper for document.querySelector(class_name) *)
let get_element_by_class_name class_name =
  (Js.Unsafe.(meth_call Dom_html.document "querySelector"
    [| inject (Js.string ("." ^ class_name)) |]))

(* Create a line function *)
let line x_scale y_scale interp =
  let use_scale lne x = Js.Unsafe.(fun_call lne [| inject x |]) in
  let dot_line = Js.Unsafe.new_obj (d3_svg##line) [||] in
  let _ = (Js.Unsafe.(meth_call dot_line "x"
    [| inject (mb (fun this d i -> use_scale x_scale d##x)) |])) in
  let _ = (Js.Unsafe.(meth_call dot_line "y"
    [| inject (mb (fun this d i -> use_scale y_scale d##y)) |])) in
  let _ = Js.Unsafe.(meth_call dot_line "interpolate" [| inject interp |]) in
  dot_line

(* Allows us to use a line-auto path generator function *)
let use_line lne data =
  Js.Unsafe.(fun_call lne [| inject data |])

(* Shortcut function to run a D3 change *)
let plz_run a =
  D3.run ~node:(Dom_html.document##body) a ()
