open D3
open Extensions


(* On clicking on an input register *)
let did_change_input id =
  let msg = "Enter a new integer value for input " ^ id in
  let num = prompt msg "" in
  let s = Bitstream.(bitstream_to_hexstring (bitstream_of_hexstring num)) in
  let change = (select ("." ^ id)
                |. html (fun _ _ _ -> s)) in
  plz_run change


(* On stepping a circuit *)
let did_step f circ_ref =
  match !circ_ref with
  | None -> ()
  | Some circ -> f circ


(* On compiling - update the state of the reference we're dealing with and
 * call function f *)
let did_compile f circ_ref =
  let text_area = get_element_by_class_name "code" in
  let code = Js.Unsafe.get (text_area) "value" in
  circ_ref := Some(Parse.parse_circuit_no_errors code);
  f circ_ref;
  ()
