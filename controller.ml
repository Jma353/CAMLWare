open D3
open Extensions
open Model


(* On clicking on an input register *)
let did_change_input f id =
  let msg = "Enter a new integer value for input " ^ id in
  let num = prompt msg "" in
  let b = Bitstream.bitstream_of_hexstring num in
  let old_circ = !(Model.circ) in
  match old_circ with
  | None -> ()
  | Some (c) ->
    let new_circ = Circuit.Simulator.change_input id b c in
    Model.circ := Some(new_circ);
    f new_circ; ()


(* On stepping a circuit *)
let did_step f () =
  let clock_lol, new_circ = step_and_return () in
  let new_content = "Clock: " ^ string_of_int clock_lol in
  Js.Unsafe.set (clock_lol) "innerHTML" (Js.string new_content);
  match new_circ with
  | None -> ()
  | Some (c) -> f c; ()


(* On compiling - update the state of the reference we're dealing with and
 * call function f *)
let did_compile f () =
  let text_area = get_element_by_class_name "code" in
  let code = Js.Unsafe.get (text_area) "value" |> Js.to_string in
  let circ = Some(Parse.parse_circuit_no_errors code) in
  (* TODO: If errors, make None *)
  Model.circ := circ;
  f circ; ()
