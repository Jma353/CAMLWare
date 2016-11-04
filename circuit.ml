open Bitstream
open Combinational

(* a map with strings as the keys *)
module StringMap = Map.Make(String)
type 'a map = 'a StringMap.t

(* since we internally represent inputs and outputs as registers, we need a
 * flag to specify their type *)
type reg_type =
  | Rising | Falling | Input | Output

(* a digital state component *)
type register = {
  reg_type : reg_type;
  length : int;
  value : bitstream;
  next : comb;
}

(* a type to represent the state of a circuit *)
type circuit = {
  registers : register map;
  clock : bool;
}

let make_register length logic reg_type =
  {
    reg_type = reg_type;
    length = length;
    value = zeros length;
    next = logic;
  }

let rising_register length logic =
  make_register length logic Rising

let falling_register length logic =
  make_register length logic Falling

let input length =
  make_register length In Input

let output length logic =
  make_register length logic Output

let circuit regs =
  {
    registers = regs;
    clock = false;
  }

let format_register f reg =
  Format.fprintf f "%s\nValue: %a\nNext: %a"
    (match reg.reg_type with
     | Rising -> "Rising Register"
     | Falling -> "Falling Register"
     | Input -> "Input"
     | Output -> "Output")
    (format_bitstream) reg.value
    (format_logic) reg.next

let format_circuit f circ =
  Format.fprintf f "Clock: %s\n\n" (if circ.clock then "1" else "0");
  StringMap.iter
    (fun id reg -> Format.fprintf f ("%s =\n%a\n\n") id (format_register) reg)
    circ.registers

let evaluate circ comb =
  failwith "unimplemented"

let step circ =
  failwith "unimplemented"

let step_n circ n =
  failwith "unimplemented"

let change_input circ id value =
  failwith "unimplemented"
