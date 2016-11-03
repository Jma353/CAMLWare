open Bitstream
open Combinational

(* a map with strings as the keys *)
module M = Map.Make(String)
type 'a map = 'a M.t

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
  registers : register map;
  clock : bool;
}

let evaluate circ comb =
  failwith "unimplemented"

let step circ =
  failwith "unimplemented"

let step_n circ n =
  failwith "unimplemented"

let change_input circ id value =
  failwith "unimplemented"
