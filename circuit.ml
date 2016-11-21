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

let map_of_assoclist l =
  List.fold_left (fun acc (k,v) -> StringMap.add k v acc) StringMap.empty l

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

let circuit_from_list l =
  l |> map_of_assoclist |> circuit

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

(************************ eval ***********************)

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

let eval_comp comp b1 b2 = 
  match comp with 
   | Lt -> less_than b1 b2
   | Gt -> greater_than b1 b2
   | Eq -> equals b1 b2
   | Lte -> logical_binop or_bits (less_than b1 b2) (equals b1 b2)
   | Gte -> logical_binop or_bits (greater_than b1 b2) (equals b1 b2)
   | Neq -> logical_not (equals b1 b2)

let eval_arith arth b1 b2 = 
  match arth with 
  | Add -> add b1 b2
  | Subtract -> subtract b1 b2
  | Sll -> shift_left b1 b2
  | Srl -> shift_right_logical b1 b2
  | Sra -> shift_right_arithmetic b1 b2

let rec eval_hlpr circ comb env = 
   match comb with 
  | Const b -> b
  | Var id -> List.assoc id env    
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
  | Comp (comp,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in 
                      let b2 = (eval_hlpr circ c2 env) in 
                      eval_comp comp b1 b2
  | Arith (arth,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in 
                      let b2 = (eval_hlpr circ c2 env) in 
                      eval_arith arth b1 b2
  | Concat (clst) -> List.fold_left 
                    (fun acc c -> 
                      concat (eval_hlpr circ c env) acc) (create []) clst
  | Mux2 (c1,c2,c3) -> let s = (eval_hlpr circ c1 env) in 
                        if is_zero s 
                        then eval_hlpr circ c3 env
                        else eval_hlpr circ c2 env
  | Apply (id,clst) -> let subcirc = StringMap.find id circ.comps in 
                        let (nv, comb1) = eval_apply subcirc circ clst env in 
                        eval_hlpr circ comb1 nv
  | Let (id,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                      if List.mem_assoc id env then 
                      failwith "Cannot use variable twice" else 
                      let nv = (id, b1)::env in eval_hlpr circ c2 nv

and 
  
    eval_apply subcirc circ clst env = (* returns (new_environment, comb) *)
      match subcirc with 
      | Subcirc (comb, ids) -> let nv = eval_apply_hlpr ids clst env circ in
                                (nv, comb)
      | _ -> failwith "incorrect sub circuit application"

and 
    eval_apply_hlpr ids clst env circ = 
       match (ids, clst) with 
      | ([], []) -> env
      | (i::is,c::cs) -> let b = (eval_hlpr circ c env) in 
                  (i, b)::(eval_apply_hlpr is cs env circ)
      | _ -> failwith "incorrect sub circuit application"
 
let rec evaluate circ comb =
  let env = StringMap.fold 
  (fun k v acc -> 
    match v with 
    | Register r -> (k, r.value)::acc
    | _ -> acc) 
  circ.comps [] in 
  (* env is a assoc list of RegID: bitstream ex: "A": 101011 *)
   eval_hlpr circ comb env 

(************************ eval ***********************)

(*[eval_regs r circ] evaluates the rising / falling registers *)
let eval_regs r circ =
  match r.next with 
  | User_input -> Register r
  | AST comb -> if (r.reg_type = Falling && circ.clock)  
                || (r.reg_type = Rising && not circ.clock)             
                then let new_val = evaluate circ comb in 
                Register {r with value = new_val; length = length new_val}
              else Register r

let update_rising_falling circ c = 
  match c with 
    | Register r -> eval_regs r circ
    | _ -> c

let update_outputs circ c = 
  match c with 
    | Register r -> (match (r.next, r.reg_type) with 
                    |(AST comb, Output) -> let new_val = evaluate circ comb in 
                                            Register {r with value = new_val; 
                                            length = length new_val}
                    | _ -> c)
    | _ -> c 

let step circ =
  let new_comps = StringMap.map (update_rising_falling circ) circ.comps in 
  let new_circ = {comps = new_comps; clock = not circ.clock} in 
  let comps_new = StringMap.map (update_outputs new_circ) new_circ.comps in 
  {comps = comps_new; clock = new_circ.clock}

let rec step_n circ n =
  match n with 
  | 0 -> circ 
  | i -> let new_circ = step circ in step_n new_circ (i - 1)

let change_input circ id value =
  let Register r = StringMap.find id circ.comps in 
  let new_comps = 
    StringMap.add id (Register {r with value = value}) circ.comps in 
  let new_circ = {comps = new_comps; clock = circ.clock} in 
  let comps_new = StringMap.map (update_outputs new_circ) new_circ.comps in 
  {comps = comps_new; clock = new_circ.clock}
