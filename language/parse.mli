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

(* [parse_logic s] is the combinational AST built by parsing [s] *)
val parse_logic : string -> Combinational.comb parser_output

(* [parse_circuit s] is the circuit state built by parsing [s] *)
val parse_circuit : string -> Circuit.circuit parser_output

type filename = string

(* [parse_logic_from_file f] is the combinational AST built by parsing [f] *)
val parse_logic_from_file   : filename -> Combinational.comb parser_output

(* [parse_circuit_from_file f] is the circuit state built by parsing [f] *)
val parse_circuit_from_file : filename -> Circuit.circuit parser_output
