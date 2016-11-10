(* The type of a circuit formatted for display in the GUI *)
type formatted_circuit

(* [format_circuit circ] returns the formatted version of [circ] *)
val format_circuit : circuit -> formatted_circuit
