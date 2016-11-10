(* OCaml interface for OCamllex lexer and OCamlyacc parser *)

(* [parse_logic s] is the combinational AST built by parsing [s] *)
val parse_logic   : string -> Combinational.comb

(* [parse_circuit s] is the circuit state built by parsing [s] *)
val parse_circuit : string -> Circuit.circuit

type filename = string

(* [parse_logic_from_file f] is the combinational AST built by parsing [f] *)
val parse_logic_from_file   : filename -> Combinational.comb

(* [parse_circuit_from_file f] is the circuit state built by parsing [f] *)
val parse_circuit_from_file : filename -> Circuit.circuit
