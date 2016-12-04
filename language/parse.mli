(* OCaml interface for OCamllex lexer and OCamlyacc parser *)

(* the output type of a parser *)
type 'a parser_output =
  | Result of 'a
  | Error of string

(* [parse_logic_no_errors s] is the combinational AST built by parsing [s].
 * this function does no error handling *)
val parse_logic_no_errors : string -> Combinational.comb

(* [parse_circuit_no_errors s] is the circuit state built by parsing [s].
 * this function does no error handling *)
val parse_circuit_no_errors : string -> Circuit.circuit

(* [parse_logic s] is either the combinational AST built by parsing [s]
 * or [Error error_msg] if a parsing error occurs*)
val parse_logic : string -> Combinational.comb parser_output

(* [parse_circuit s] is either the circuit state built by parsing [s]
 * or [Error error_msg] if a parsing error occurs or the circuit fails to
 * pass static analysis *)
val parse_circuit : string -> Circuit.circuit parser_output

type filename = string

(* [parse_logic_from_file f] is either the combinational AST built by parsing
 * [f] or [Error error_msg] if a parsing error occurs*)
val parse_logic_from_file   : filename -> Combinational.comb parser_output

(* [parse_circuit_from_file f] is either the circuit state built by parsing [f]
 * or [Error error_msg] if a parsing error occurs or the circuit fails to
 * pass static analysis *)
val parse_circuit_from_file : filename -> Circuit.circuit parser_output
