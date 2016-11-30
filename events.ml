open D3
open Extensions

(* Module dealing with all circuit-based events *)
module CircuitEvents = struct

  (* On clicking on an input register *)
  let did_change_input id =
    let msg = "Enter a new integer value for input " ^ id in
    let num = prompt msg "" in
    let s = Bitstream.(bitstream_to_hexstring (bitstream_of_hexstring num)) in
    let change = (select ("." ^ id)
                  |. html (fun _ _ _ -> s)) in
    plz_run change
    
end

(* Module dealing with all compilation-based events *)
module CompileEvents = struct

  (* On compiling *)
  let did_compile s circ_ref =
    (* Clear the circuit *)
    plz_run (select ".circuit" |. html (fun _ _ _ -> ""));
    let code = Js.Unsafe.get (get_element_by_class_name "code") "value" in
    circ_ref := Parse.parse_circuit_no_errors code;
    (* TODO: Run the view-augmenting function *)
    ()

end
