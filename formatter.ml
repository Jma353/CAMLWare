Open Circuit

type formatted_register = {
  reg_type : reg_type;
  length : int;
  value : bitstream;
  next : comb;
  reg_col : int;
}

(* a type to represent the state of a circuit *)
type circuit = {
  registers : register map;
  clock : bool;
}

type formatted_circuit = {

}
