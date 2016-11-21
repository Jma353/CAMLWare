open Circuit

(* This module contains all of the static checking functionality involved
 * in validating a circuit specification*)

type error_log

(* [validate circ] produces a log of static errors in the circuit spec *)
val validate : circuit -> error_log

(* [valid log] returns whether a circuit with error log [log] is valid*)
val valid : error_log -> bool

(* [print_log log] prints out an error log on stderr *)
val print_log : error_log -> unit
