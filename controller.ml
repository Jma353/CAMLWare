open D3
open Extensions
open Model
open Storage


(* On clicking on an input register *)
let did_change_input f id =
  let msg = "Enter a new integer value for input " ^ id in
  let num = prompt msg "" in
  let b = Bitstream.bitstream_of_hexstring num in
  let old_circ = Model.from_json (Storage.find ()) in
  let new_circ = Circuit.Simulator.change_input id b old_circ in
  let m = Model.make (Some new_circ) in
  Storage.set @@ Model.to_json m;
  f new_circ; ()


(* On stepping a circuit *)
let did_step f () =
  let old_circ = Model.from_json (Storage.find ()) in
  let new_circ = Circuit.Simulator.step old_circ in
  match old_circ with
  | None -> ()
  | Some circ ->
    let m = Model.make (Some circ) in
    Storage.set @@ Model.to_json m;
    f circ; ()


(* On compiling - update the state of the reference we're dealing with and
 * call function f *)
let did_compile f () =
  let text_area = get_element_by_class_name "code" in
  let code = Js.Unsafe.get (text_area) "value" |> Js.to_string in
  let circ = Some(Parse.parse_circuit_no_errors code) in
  let m = Model.make a in
  Storage.set @@ Model.to_json m;
  f circ; ()
