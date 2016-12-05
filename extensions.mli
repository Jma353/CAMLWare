(* Translation style string helper *)
val translate : int -> int -> string

(* OCaml to coordinates in JavaScript *)
val _d : (float * float) list -> 'c Js.js_array Js.t

(* Create a linear scaling function *)
val linear : int * int -> int * int -> 'a Js.t

(* Wrapper for prompt(query, default) *)
val prompt : string -> string -> string

(* Wrapper for document.querySelector(class_name) *)
val get_element_by_class_name : string -> 'a Js.t

(* Create a line function *)
val line : 'a Js.t -> 'b Js.t -> string -> 'c Js.t

(* Allows us to use a line-auto path generator function *)
val use_line : 'a Js.t -> 'a Js.js_array Js.t -> string

(* Shortcut function to run a D3 change *)
val plz_run : (unit, 'a) D3.t -> unit
