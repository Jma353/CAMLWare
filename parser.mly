%{

open Bitstream
open Combinational
open Circuit
open Base_conversions
open Lexing

let parse_error _ =
  let start_pos = Parsing.symbol_start_pos () in
  let end_pos = Parsing.symbol_end_pos () in
  let start_line = string_of_int start_pos.pos_lnum in
  let start_char = string_of_int (start_pos.pos_cnum - start_pos.pos_bol) in
  let end_line = string_of_int end_pos.pos_lnum in
  let end_char = string_of_int (end_pos.pos_cnum - end_pos.pos_bol) in
  failwith ("Parse error: ("^start_line^"."^start_char^"-"^end_line^"."^end_char)
%}

%token LET
%token REGISTER
%token FALLING
%token RISING
%token INPUT
%token OUTPUT
%token ASSIGN
%token EQ
%token NEQ
%token GTE
%token LTE
%token GT
%token LT
%token LAND
%token LOR
%token LNOT
%token NAND
%token NOR
%token NXOR
%token AND
%token OR
%token XOR
%token NOT
%token PLUS
%token SLL
%token SRL
%token SRA
%token DASH
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token SEMICOLON
%token <string> DEC
%token <string> BIN
%token <string> HEX
%token <string> VAR
%token EOF

%start comb
%type <Combinational.comb> comb

%start circuit
%type <Circuit.circuit> circuit

%%

circuit:
  | registers {circuit $1}
;

registers:
  | register {let reg = $1 in StringMap.singleton (fst reg) (snd reg)}
  | register registers {let reg = $1 in StringMap.add (fst reg) (snd reg) $2}
;

register:
  | REGISTER VAR LBRACKET number RBRACKET ASSIGN comb SEMICOLON
    {($2, rising_register $4 $7)}
  | RISING REGISTER VAR LBRACKET number RBRACKET ASSIGN comb SEMICOLON
    {($3, rising_register $5 $8)}
  | FALLING REGISTER VAR LBRACKET number RBRACKET ASSIGN comb SEMICOLON
    {($3, falling_register $5 $8)}
  | INPUT VAR LBRACKET number RBRACKET SEMICOLON
    {($2, input $4)}
  | OUTPUT VAR LBRACKET number RBRACKET ASSIGN comb SEMICOLON
    {($2, output $4 $7)}
;

comb:
  | add_expr {$1}
;

add_expr:
  | shift_expr {$1}
  | add_expr PLUS shift_expr {Arith (Add,$1,$3)}
  | add_expr DASH shift_expr {Arith (Subtract,$1,$3)}
;

shift_expr:
  | comp_expr {$1}
  | shift_expr SLL comp_expr {Arith (Sll,$1,$3)}
  | shift_expr SRL comp_expr {Arith (Srl,$1,$3)}
  | shift_expr SRA comp_expr {Arith (Sra,$1,$3)}
;

comp_expr:
  | or_expr {$1}
  | comp_expr EQ or_expr {Comp (Eq,$1,$3)}
  | comp_expr NEQ or_expr {Comp (Neq,$1,$3)}
  | comp_expr LT or_expr {Comp (Lt,$1,$3)}
  | comp_expr GT or_expr {Comp (Gt,$1,$3)}
  | comp_expr LTE or_expr {Comp (Lte,$1,$3)}
  | comp_expr GTE or_expr {Comp (Gte,$1,$3)}
;

or_expr:
  | and_expr {$1}
  | or_expr OR and_expr {Gate (Or,$1,$3)}
  | or_expr XOR and_expr {Gate (Xor,$1,$3)}
  | or_expr NOR and_expr {Gate (Nor,$1,$3)}
  | or_expr NXOR and_expr {Gate (Nxor,$1,$3)}
  | or_expr LOR and_expr {Logical (Or,$1,$3)}
;

and_expr:
  | unary {$1}
  | and_expr AND unary {Gate (And,$1,$3)}
  | and_expr NAND unary {Gate (Nand,$1,$3)}
  | and_expr LAND unary {Logical (And,$1,$3)}
;

unary:
  | array_access {$1}
  | NOT unary {Neg (Neg_bitwise,$2)}
  | LNOT unary {Neg (Neg_logical,$2)}
  | DASH unary {Neg (Neg_arithmetic,$2)}
  | AND unary {Reduce (And,$2)}
  | OR unary {Reduce (Or,$2)}
  | XOR unary {Reduce (Xor,$2)}
  | NAND unary {Reduce (Nand,$2)}
  | NOR unary {Reduce (Nor,$2)}
  | NXOR unary {Reduce (Nxor,$2)}
;

array_access:
  | primary {$1}
  | primary LBRACKET number RBRACKET {Nth ($3,$1)}
  | primary LBRACKET number DASH number RBRACKET {Sub_seq ($3,$5,$1)}
;

primary:
  | VAR {Reg $1}
  | bitstream {Const ($1)}
  | LPAREN comb RPAREN {$2}
  | LBRACE concat_inside {$2}
;

concat_inside:
  | comb RBRACE {$1}
  | comb COMMA concat_inside {Concat ($1,$3)}
;

bitstream:
  | DEC {bitstream_of_decimal (int_of_string $1)}
  | HEX {bitstream_of_hexstring $1}
  | BIN {bitstream_of_binstring $1}
;

number:
  | DEC {int_of_string $1}
  | HEX {dec_of_hexstring_unsigned $1}
  | BIN {dec_of_binstring_unsigned $1}
;
