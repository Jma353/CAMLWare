open D3
open Extensions

(* Circuit Reference (None or Some) *)
let (circ:Circuit.circuit option ref) = ref None

(* Clock Reference *)
let (clock: int ref) = ref 0

(* Steps the clock (0 -> 1, 1-> 0) and returns the new clock value.
 * Also steps the circuit if it exists *)
let step_and_return () =
  match !circ with
  | None -> (0,None)
  | Some (c) ->
    let cl = !clock in
    let new_c = if cl=1 then 0 else 1 in
    clock := new_c;
    let new_circ = Circuit.Simulator.step c in
    circ := Some(new_circ);
    (new_c,Some(new_circ))
