open D3
open Extensions

(* Module dealing with all circuit-based events *)
module CircuitEvents = struct

  (* On clicking on an input register *)
  let did_change_input id =
    let msg = "Enter a new integer value for input " ^ id in
    let num = prompt msg "" in
    (* let s = Bitstream.(bitstream_to_hexstring (bitstream_of_hexstring num)) in *) 
    let change = (select ("." ^ id)
                  |. html (fun _ _ _ -> num)) in
    plz_run change




end

(* Module dealing with all compilation-based events *)
module CompileEvents = struct

  (* On compiling: TODO *)
  let did_compile s = failwith "not implemented"

end
