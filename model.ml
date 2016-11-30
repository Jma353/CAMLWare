open D3
open Extensions
open Circuit

(* The model object for the GUI *)
type t = circuit option deriving (Json) (* To save & store as JSON *)

(* Make a model object *)
let make circ_op : t = circ_op

(* To JSON from model *)
let to_json m = Json.to_string<t> m

(* To model from JSON *)
let from_json s = Json.from_string<t> s
