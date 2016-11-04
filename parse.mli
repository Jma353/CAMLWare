
val parse_logic   : string -> Combinational.comb
val parse_circuit : string -> Circuit.circuit

type filename = string

val parse_logic_from_file   : filename -> Combinational.comb
val parse_circuit_from_file : filename -> Circuit.circuit
