open D3
open Extensions

(* Circuit Reference (None or Some) *)
let (circ:Circuit.circuit option ref) = ref None

(* Clock Reference *)
let (clock: int ref) = ref 0

(* Steps the clock (0 -> 1, 1-> 0) if the circuit exists & updates the
 * circuit if the circuit exists.
 * Returns: A tuple of the new clock value and the optional circuit *)
let step_and_return () =
  match !circ with
  | None -> (0, None)
  | Some (c) ->
    let cl = !clock in
    let new_c = if cl=1 then 0 else 1 in
    clock := new_c;
    let new_circ = Circuit.Simulator.step c in
    circ := Some(new_circ);
    (new_c, !circ)

(* Compiles the circuit given a string, updates the circuit, and resets
 * the clock.
 * Returns: A tuple of the new clock and the circuit parsing result *)
let compile_and_return code =
  let parse_result = Parse.parse_circuit code in
  clock := 0;
  match parse_result with
  | Parse.Error s -> circ := None; (!clock, parse_result)
  | Parse.Result c -> circ := Some(c); (!clock, parse_result)

(* Changes the input of input-register `id` to the hex-value
 * represented by the string hex_s.
 * Returns: A tuple of the new clock and the circuit parsing result *)
let change_input_and_return id hex_s =
  clock := 0;
  match !circ with
  | None -> (!clock, None)
  | Some (c) ->
    let b = Bitstream.bitstream_of_hexstring hex_s in
    let new_circ = Circuit.Simulator.change_input id b c in
    circ := Some(new_circ);
    (!clock, !circ)
