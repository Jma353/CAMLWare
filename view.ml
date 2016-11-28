open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

(* Representing a location (of inputs &/or outputs) *)
type point = { x : int; y : int }

(* Represents means to get to a specifc component in the view
 * (output and a list of inputs) *)
type channels = {
  output: point;
  inputs: point list;
}

(* Mini-Int module for use in mapping id's to channels *)
module Int = struct
  type t = int
  let make n : t = n
  let compare n1 n2 : int =
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0
end

(* Int of int -> channel *)
module IntMap = Map.Make(Int)

(* Collects a list of functions to be applied to a view, as well as map
 * info regarding inputs & outputs. *)
let collect_views (c: formatted_circuit) = failwith "not implemented"

(*  *)
