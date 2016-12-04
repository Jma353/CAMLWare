open Bitstream
open Combinational

(* A circuit represents a collection of registers linked together by
 * combinational logic expressions *)

(* a map with strings as the keys *)
module StringMap : Map.S with type key = string
type 'a map = 'a StringMap.t

(* a set of strings *)
module StringSet : Set.S with type elt = string
type set = StringSet.t

(* a type to represent a circuit component *)
type component

(* a type to represent the state of a circuit *)
type circuit

(* [rising_register length logic] is a rising register of length [length]
 * with update logic [logic] *)
val rising_register : int -> comb -> component

(* [falling_register length logic] is a falling register of length [length]
 * with update logic [logic] *)
val falling_register : int -> comb -> component

(* [input length] is a circuit input of length [length] *)
val input : int -> component

(* [output length logic] is a circuit output of length [length]
 * with update logic [logic] *)
val output : int -> comb -> component

(* [subcircuit logic args] is a subcircuit with AST [logic] and args [args] *)
val subcircuit : comb -> int -> (id * int) list -> component

(*
 * [circuit comps] is a circuit constructed from the components named in [comps]
 *)
val circuit : component map -> circuit

(* [circuit_from_list comps] is a circuit constructed from the components named
 * in association list [comps] *)
val circuit_from_list : (id * component) list -> circuit

(* [register_values circ] is a map from the ids of the register in [circ] to
 * their values *)
val register_values : circuit -> bitstream map

(* formatting function for circuit components *)
val format_comp : Format.formatter -> component -> unit

(* formatting function for circuits *)
val format_circuit : Format.formatter -> circuit -> unit


module type CircuitSimulator = sig
  (* [evaluate circ comb] is the bitstream that results from evaluating
   * [comb] in the context of circuit [circ] *)
  val evaluate : circuit -> comb -> bitstream

  (* [step circ] is the circuit that results from toggling the state of the clock
   * in [circ] and updating each clocked register *)
  val step : circuit -> circuit

  (* [step_n circ n] is the circuit that results from stepping [circ] [n] times *)
  val step_n : int -> circuit -> circuit

  (* [change_input circ in value] is the circuit that results from replacing the
   * value of input [in] in [circ] with [value] and updating and dependent
   * outputs *)
  val change_input : id -> bitstream -> circuit -> circuit

  (* [update_outputs circ] is the circuit with all outputs updates via current
  register values *)
  val update_outputs: circuit -> circuit

end

module type StaticAnalyzer = sig
  (* This module contains all of the static checking functionality involved
   * in validating a circuit specification *)

  (* this type represents a log of static errors *)
  type error_log

  (* [validate circ] produces a log of static errors in the circuit spec *)
  val validate : circuit -> error_log

  (* [valid log] returns whether a circuit with error log [log] is valid*)
  val valid : error_log -> bool

  (* [print_log lf og] formats an error log for printing *)
  val format_log : Format.formatter -> error_log -> unit
end

module type CircuitFormatter = sig
  (* This module contains the functions for formatting a circuit in preparation
   * for rendering it *)

  type connection = Reg of id | Node of int | Let of id

  type node =
    | B of gate * connection * connection
    | L of gate * connection * connection
    | A of arithmetic * connection * connection
    | N of negation * connection
    | C of comparison * connection * connection
    | Sub of int * int * connection
    | Nth of int * connection
    | Red of gate * connection
    | Concat of connection list
    | Mux of connection * connection * connection
    | Const of bitstream
    | Apply of id * connection list

  type display_reg_type = Dis_rising | Dis_falling | Dis_input | Dis_output

  type display_node = {
    n_id : int;
    n_x_coord: float;
    n_y_coord: float;
    node : node;
  }

  type display_let = {
    l_id : id;
    l_x_coord:float;
    l_y_coord: float;
    inputs: id list;
  }


  type display_register = {
    r_id : id;
    r_reg_type :  display_reg_type;
    r_x_coord : float;
    r_y_coord : float;
    input : int;
  }

  type formatted_circuit = {
    registers : (id * display_register) list;
    nodes     : (int * display_node) list;
    lets      : (id * display_let) list
  }

  val test_circ : unit -> formatted_circuit

  (* [format circ] is a representation of [circ] with coordinate information
   * attached for rendering *)
  val format : circuit -> formatted_circuit

  (* [format_format_circuit f circ] format a formatted circuit for printing *)
  val format_format_circuit : Format.formatter -> formatted_circuit -> unit
end

module Analyzer : StaticAnalyzer
module Simulator : CircuitSimulator
module Formatter : CircuitFormatter
