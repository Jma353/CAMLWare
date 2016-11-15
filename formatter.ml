Open Circuit

type coord = int * int

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
