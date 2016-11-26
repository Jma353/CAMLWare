open Circuit

type coord = float * float
type column = register list

type formatted_comb =
  | FConst   of bitstream * coord
  | FReg     of id * coord
  | FSub_seq of int * int * coord
  | FNth     of int * formatted_comb * coord
  | FGate    of gate * formatted_comb * formatted_comb * coord
  | FLogical of gate * formatted_comb * formatted_comb * coord
  | FReduce  of gate * formatted_comb * coord
  | FNeg     of negation * formatted_comb * coord
  | Fcomp    of comparison * formatted_comb * formatted_comb * coord
  | FArith   of arithmetic * formatted_comb * formatted_comb * coord
  | FConcat  of formatted_comb * formatted_comb * coord
  | FIn      of coord

type formatted_register = {
  reg_type : reg_type;
  length   : int;
  value    : bitstream;
  next     : formatted_comb;
  reg_col  : int;
}

type formatted_circuit = {
  registers : formatted_register map;
  clock : bool;
}

let list_dependencies register circuit =
  let rec traverse_comb comb dependencies =
  match comb with
  | Const -> dependencies
  | Reg id ->
    if List.mem id dependencies then dependencies
    else id::dependencies
  | Sub_seb (_,_,c) -> (traverse_comb c dependencies)
  | Nth(_, c) -> (traverse_comb c dependencies)
  | Gate (_, c1, c2) -> (traverse_comb c1 [])@(traverse_comb c2 dependencies)
  | Logical (_, c1, c2) -> (traverse_comb c1 [])@(traverse_comb c2 dependencies)
  | Reduce (_, c) -> (traverse_comb c dependencies)
  | Neg (_, c) -> (traverse_comb c dependencies)
  | Comp  (_, c1, c2) -> (traverse_comb c1 [])@(traverse_comb c2 dependencies)
  | Arith  (_, c1, c2) -> (traverse_comb c1 [])@(traverse_comb c2 dependencies)
  | Concat (c1, c2) -> (traverse_comb c1 [])@(traverse_comb c2 dependencies)
  | In -> dependencies

let register_columns circ =
  circ.registers.map co
