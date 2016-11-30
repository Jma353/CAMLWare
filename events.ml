open D3
open Extensions

(* Module dealing with all circuit-based events *)
module CircuitEvents = struct

  (* On clicking on an input register *)
  let did_change_input id =
    let msg = "Enter a new integer value for input " ^ id in
    let num = prompt msg "" in
    print_endline num 

end

(* Module dealing with all compilation-based events *)
module CompileEvents = struct

  (* On compiling: TODO *)
  let did_compile s = failwith "not implemented"

end
