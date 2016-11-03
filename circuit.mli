open Bitstream
open Combinational

(* A circuit represents a collection of registers linked together by
 * combinational logic expressions *)

(* a map with strings as the keys *)
module M = Map.Make(String)

(* since we internally represent inputs and outputs as registers, we need a
 * flag to specify their type *)
type reg_type =
  | Register | Input | Output

(* a type to specify whether a component is rising edge or falling edge
 * triggered *)
type clock_behavior =
  | Rising | Falling

(* a digital state component *)
type register = {
  reg_type : reg_type;
  clock_behavior : clock_behavior;
  length : int;
  value : bitstream;
  next : comb;
}

(* a type to represent the state of a circuit *)
type circuit = {
  registers : register M.t;
  clock : bool
}

(* [evaluate circ reg] is the bitstream that results from evaluating
 * [comb] in the context of circuit [circ]*)
val evaluate : circuit -> comb -> bitstream

(* [step circ] is the circuit that results from toggling the state of the clock
 * in [circ] and updating each clocked register *)
val step : circuit -> circuit

(* [step_n circ n] is the circuit that results from stepping [circ] [n] times *)
val step_n : circuit -> int -> circuit

(* [change_input circ in value] is the circuit that results from replacing the
 * value of input [in] in [circ] with [value] and updating and dependent
 * outputs *)
val change_input : circuit -> id -> bitstream -> circuit
