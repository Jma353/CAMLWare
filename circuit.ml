open Bitstream
open Combinational

(* a map with strings as the keys *)
module StringMap = Map.Make(String)
type 'a map = 'a StringMap.t

(* since we internally represent inputs and outputs as registers, we need a
 * flag to specify their type *)
type reg_type =
  | Rising | Falling | Input | Output

type register_input =
  | User_input | AST of comb

(* a digital state component *)
type register = {
  reg_type : reg_type;
  length : int;
  value : bitstream;
  next : register_input;
}

(* a circuit component is either a register or a subcircuit *)
type component =
  | Register of register | Subcirc of comb * id list

(* a type to represent the state of a circuit *)
type circuit = {
  comps : component map;
  clock : bool;
}

let make_register length logic reg_type =
  Register {
    reg_type = reg_type;
    length = length;
    value = zeros length;
    next = logic;
  }

let rising_register length logic =
  make_register length (AST logic) Rising

let falling_register length logic =
  make_register length (AST logic) Falling

let input length =
  make_register length User_input Input

let output length logic =
  make_register length (AST logic) Output

let subcircuit logic args =
  Subcirc (logic, args)

let circuit comps =
  {
    comps = comps;
    clock = false;
  }

let format_register_input f input =
  match input with
  | User_input -> Format.fprintf f "User Input"
  | AST c -> Format.fprintf f "%a" (format_logic) c

let rec format_args f args =
  match args with
  | [] -> ()
  | h::[] -> Format.fprintf f "%s" h
  | h::t -> Format.fprintf f "%s, %a" h (format_args) t

let format_comp f comp =
  match comp with
  | Register reg ->
    Format.fprintf f "%s\nValue: %a\nNext: %a"
      (match reg.reg_type with
       | Rising -> "Rising Register"
       | Falling -> "Falling Register"
       | Input -> "Input"
       | Output -> "Output")
      (format_bitstream) reg.value
      (format_register_input) reg.next
  | Subcirc (sub,args) ->
    Format.fprintf f "(%a) -> %a" (format_args) args (format_logic) sub

let format_circuit f circ =
  Format.fprintf f "Clock: %s\n\n" (if circ.clock then "1" else "0");
  StringMap.iter
    (fun id comp -> Format.fprintf f ("%s =\n%a\n\n") id (format_comp) comp)
    circ.comps

let evaluate circ comb =
  failwith "unimplemented"

let step circ =
  failwith "unimplemented"

let step_n circ n =
  failwith "unimplemented"

let change_input circ id value =
  failwith "unimplemented"
