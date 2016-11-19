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

let rec eval_gates bin_op bin_not g b1 b2 = 
  match g with 
  | And -> bin_op and_bits b1 b2
  | Or -> bin_op or_bits b1 b2
  | Xor -> bin_op xor_bits b1 b2 
  | Nand -> bin_not (bin_op and_bits b1 b2)
  | Nor -> bin_not (bin_op or_bits b1 b2)
  | Nxor -> bin_not (bin_op xor_bits b1 b2)

let rec eval_reduce g b1 = 
  match g with 
  | And -> reduce and_bits b1 
  | Or -> reduce or_bits b1 
  | Xor -> reduce xor_bits b1  
  | Nand -> bitwise_not (reduce and_bits b1)
  | Nor ->  bitwise_not (reduce or_bits b1)
  | Nxor -> bitwise_not (reduce xor_bits b1)

let eval_neg n b1 = 
  match n with 
  | Neg_bitwise -> bitwise_not b1
  | Neg_logical -> logical_not b1
  | Neg_arithmetic -> negate b1

let rec eval_hlpr circ comb env = 
   match comb with 
  | Const b -> b
  | Reg id -> List.assoc id env    
  | Sub_seq (n1,n2,c) -> substream (eval_hlpr circ c env) n1 n2  
  | Nth (i,c) -> nth (eval_hlpr circ c env) i
  | Gate (g,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in 
                      let b2 = (eval_hlpr circ c2 env) in 
                      eval_gates bitwise_binop bitwise_not g b1 b2
  | Logical (g,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in 
                      let b2 = (eval_hlpr circ c2 env) in 
                      eval_gates logical_binop logical_not g b1 b2 
  | Reduce (g,c) -> let b1 = (eval_hlpr circ c env) in 
                    eval_reduce g b1
  | Neg (n,c) -> let b1 = (eval_hlpr circ c env) in eval_neg n b1
  | Comp (comp,c1,c2) -> (match comp with 
                          | Lt | Gt | Eq | Lte | Gte | Neq -> failwith "unimplemented")
  | Arith (arth,c1,c2) -> (match arth with 
                      | Add | Subtract | Sll | Srl | Sra -> failwith "unimplemented")
  | Concat (c1,c2) -> concat (eval_hlpr circ c1 env) (eval_hlpr circ c2 env)
  | Mux2 (c1,c2,c3) -> failwith "unimplemented"
  | Apply (id,clst) -> failwith "unimplemented"

let rec evaluate circ comb =
  let env = StringMap.fold 
  (fun k v acc -> 
    match v with 
    | Register r -> (k, r.value)::acc
    | _ -> acc) 
  circ.comps [] in 
  (* env is a assoc list of RedID: bitstream ex: "A": 101011 *)
   eval_hlpr circ comb env 

 


let step circ =
  failwith "unimplemented"

let step_n circ n =
  failwith "unimplemented"

let change_input circ id value =
  failwith "unimplemented"
