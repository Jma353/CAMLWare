open Bitstream

(* Abstract syntax for combinational logic expressions *)

(* the type of the keys to which registers are bound *)
type id = string

(* the digital gates *)
type gate =
  | And | Or | Xor | Nand | Nor | Nxor

(* the types of negation *)
type negation =
  | Neg_bitwise | Neg_logical | Neg_arithmetic

(* the types of comparison *)
type comparison =
  | Lt | Gt | Eq | Lte | Gte | Neq

(* the types of supported arithmetic *)
type arithmetic =
  | Add | Subtract

(* values of type [comb] represent combinational logic circuits.
 * - [Const b] represents a constant value containing bitstream [b]
 * - [Reg id] represents the value of the register with id [id]
 * - [Sub_seq from to b] represents the subsequence of bitstream [b] from index
 *   [from] to index [to]
 * - [Gate g b1 b2] represents gate [g] applied bitwise to [b1] and [b2]
 * - [Logical g b1 b2] represents gate [g] applied logically to [b1] and [b2]
 * - [Reduce g b] represents [b] reduced with gate [g]
 * - [Mux sel bs] represents bitstreams [bs] multiplexed on [sel]
 * - [Neg negation b] represents [negation] applied to [b]
 * - [Comp comp b1 b2] represents [comp] applied to [b1] and [b2]
 * - [Arith op b1 b2] represents [op] applied to [b1] and [b2]
 * - [Concat b1 b2] represents [b1] concatenated to [b2]
 * - [Replicate n b] represents [b] replicated [n] times
 *)
type comb =
  | Const     of bitstream
  | Reg       of id
  | Sub_seq   of int * int * comb
  | Gate      of gate * comb * comb
  | Logical   of gate * comb * comb
  | Reduce    of gate * comb
  | Mux       of comb * comb list
  | Neg       of negation * comb
  | Comp      of comparison * comb * comb
  | Arith     of arithmetic * comb * comb
  | Concat    of comb * comb
  | Replicate of int * comb
