open D3
open Extensions

(* Steps the clock (0 -> 1, 1-> 0) if the circuit exists & updates the
 * circuit if the circuit exists.
 * Returns: A tuple of the new clock value and the optional circuit *)
val step_and_return : unit -> int * Circuit.circuit option

(* Compiles the circuit given a string, updates the circuit, and resets
 * the clock.
 * Returns: A tuple of the new clock and the circuit parsing result *)
val compile_and_return : string -> int * (Circuit.circuit Parse.parser_output)

(* Changes the input of input-register `id` to the hex-value
 * represented by the string hex_s.
 * Returns: A tuple of the new clock and the circuit parsing result *)
val change_input_and_return : string -> string -> int * Circuit.circuit option
