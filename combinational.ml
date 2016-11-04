open Bitstream

type id = string

type gate =
  | And | Or | Xor | Nand | Nor | Nxor

type negation =
  | Neg_bitwise | Neg_logical | Neg_arithmetic

type comparison =
  | Lt | Gt | Eq | Lte | Gte | Neq

type arithmetic =
  | Add | Subtract | Sll | Srl | Sra

type comb =
  | Const     of bitstream
  | Reg       of id
  | Sub_seq   of int * int * comb
  | Nth       of int * comb
  | Gate      of gate * comb * comb
  | Logical   of gate * comb * comb
  | Reduce    of gate * comb
  | Mux       of comb * comb list
  | Neg       of negation * comb
  | Comp      of comparison * comb * comb
  | Arith     of arithmetic * comb * comb
  | Concat    of comb * comb
  | Replicate of int * comb
