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
  | Neg       of negation * comb
  | Comp      of comparison * comb * comb
  | Arith     of arithmetic * comb * comb
  | Concat    of comb * comb
  | Mux2      of comb * comb * comb
  | Apply     of id * comb list

let string_of_gate g =
  match g with
   | And -> "&"
   | Or -> "|"
   | Xor -> "^"
   | Nand -> "~&"
   | Nor -> "~|"
   | Nxor -> "~^"

let string_of_logical_gate g =
  match g with
   | And -> "&&"
   | Or -> "||"
   | Xor -> "^^"
   | Nand -> "!&"
   | Nor -> "!|"
   | Nxor -> "!^"

let string_of_negation n =
  match n with
  | Neg_bitwise -> "~"
  | Neg_logical -> "!"
  | Neg_arithmetic -> "-"

let string_of_comparison comp =
  match comp with
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Eq -> "=="
  | Neq -> "!="

let string_of_arithmetic a =
  match a with
  | Add -> "+"
  | Subtract -> "-"
  | Sll -> "<<"
  | Srl -> ">>"
  | Sra -> ">>>"

let rec format_logic f comb =
  match comb with
  | Const b -> format_bitstream f b
  | Reg id -> Format.fprintf f "%s" id
  | Sub_seq (n1,n2,c) -> Format.fprintf f "%a[%i-%i]" (format_logic) c n1 n2
  | Nth (n,c) -> Format.fprintf f "%a[%i]" (format_logic) c n
  | Gate (g,c1,c2) -> Format.fprintf f "(%a) %s (%a)" (format_logic) c1
                        (string_of_gate g) (format_logic) c2
  | Logical (g,c1,c2) -> Format.fprintf f "(%a) %s (%a)" (format_logic) c1
                        (string_of_logical_gate g) (format_logic) c2
  | Reduce (g,c) -> Format.fprintf f "%s(%a)" (string_of_gate g)
                        (format_logic) c
  | Neg (n,c) -> Format.fprintf f "%s(%a)" (string_of_negation n)
                        (format_logic) c
  | Comp (comp,c1,c2) -> Format.fprintf f "(%a) %s (%a)" (format_logic) c1
                        (string_of_comparison comp) (format_logic) c2
  | Arith (a,c1,c2) -> Format.fprintf f "(%a) %s (%a)" (format_logic) c1
                        (string_of_arithmetic a) (format_logic) c2
  | Concat (c1,c2) -> Format.fprintf f "{%a, %a}" (format_logic) c1
                        (format_logic) c2
  | Mux2 (sel,c1,c2) -> Format.fprintf f "(if %a then %a else %a)"
                          (format_logic) sel (format_logic) c1 (format_logic) c2
  | Apply (id,args) -> Format.fprintf f "%s(%a)" id (format_args) args

and format_args f args =
  match args with
  | [] -> ()
  | h::[] -> Format.fprintf f "%a" (format_logic) h
  | h::t -> Format.fprintf f "%a,%a" (format_logic) h (format_args) t
