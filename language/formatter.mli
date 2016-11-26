open Circuit

(* Indicates the scaled coordinate of a particular component -
 * e.g. (1.5, 47). *)
type coord
type id

(* Combinational logical components with coordinates associated with
 * them, based on the 100 x 100 canvas this formatter places components
 * on *)
type formatted_comb

(* The type of a circuit formatted for display in the GUI *)
type formatted_circuit

(* [format_circuit circ] returns the formatted version of [circ] *)
val format_circuit : circuit -> formatted_circuit

val list_dependencies : id -> circuit -> id list
