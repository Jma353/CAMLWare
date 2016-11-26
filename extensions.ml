open Array

(* Extensions to ocaml-d3 *)
module D3Extended = struct
  include D3



  (* HELPER FUNCTIONS, TYPING, & REFERENCES *)

  (* Generic types (separate but inspired by D3 module being extended) *)
  type 'a s = S
  type ('a, 'b) our_t = 'a s -> 'b s

  (* Reference to global d3 *)
  let d3 = Js.Unsafe.variable "d3"

  (* Original ocaml-d3 helper: https://goo.gl/VvVjHD*)
  let name_call (meth:string) (name:string) f =
    let open Js.Unsafe in
    let name = Js.string name in
    fun cxt -> meth_call cxt meth [| inject name; inject f |]

  (* Original ocaml-d3 helper: https://goo.gl/VvVjHD *)
  let const_call (meth:string) arg cxt =
    Js.(Unsafe.meth_call cxt meth [| Unsafe.inject arg |])

  (* Original ocaml-d3 helper: https://goo.gl/VvVjHD *)
  let thunk_call (meth:string) cxt =
    Js.Unsafe.meth_call cxt meth [| |]

  (* Alias for OCaml function utilization in JS callbacks:
   * https://goo.gl/VvVjHD*)
  let mb = Js.wrap_meth_callback

  (* [rng name min max f] Range method calls on `name` (e.g. .domain (...)) *)
  let rng name min max f = const_call name (mb
    (fun this d i () -> Js.array (f this d i)))



  (* EXTRA BINDINGS *)

  let d3_svg   = d3##svg
  let d3_scale = d3##scale

  let ar f name lst = f name (fun _ _ _ -> of_list lst)

  let linear : ('a, 'a) our_t = thunk_call "linear"
  let line   : ('a, 'a) our_t = thunk_call "line"

  let x f = const_call "x" (mb (fun this d i () -> Js.float (f this d i)))
  let y f = const_call "y" (mb (fun this d i () -> Js.float (f this d i)))
  let domain min max f = rng "domain" min max f
  let range min max f = rng "range" min max f


end
