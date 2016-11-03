%{

open Combinational
open Circuit
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
%token DASH
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token <string> INT
%token <string> VAR
%token EOF

%start comb
%type <Combinational.comb> comb

%%

comb:
  | arith_expr {$1}
;

arith_expr:
  | comp_expr {$1}
  | comp_expr PLUS comb {Arith (Add,$1,$3)}
  | comp_expr DASH comb {Arith (Subtract,$1,$3)}
;

comp_expr:
  | or_expr {$1}
  | or_expr EQ comp_expr {Comp (Eq,$1,$3)}
  | or_expr NEQ comp_expr {Comp (Neq,$1,$3)}
  | or_expr LT comp_expr {Comp (Lt,$1,$3)}
  | or_expr GT comp_expr {Comp (Gt,$1,$3)}
  | or_expr LTE comp_expr {Comp (Lte,$1,$3)}
  | or_expr GTE comp_expr {Comp (Gte,$1,$3)}
;

or_expr:
  | and_expr {$1}
  | and_expr OR or_expr {Gate (Or,$1,$3)}
  | and_expr XOR or_expr {Gate (Xor,$1,$3)}
  | and_expr NOR or_expr {Gate (Nor,$1,$3)}
  | and_expr NXOR or_expr {Gate (Nxor,$1,$3)}
  | and_expr LOR or_expr {Logical (Or,$1,$3)}
;

and_expr:
  | unary {$1}
  | unary AND and_expr {Gate (And,$1,$3)}
  | unary NAND and_expr {Gate (Nand,$1,$3)}
  | unary LAND and_expr {Logical (And,$1,$3)}
;

unary:
  | primary {$1}
  | NOT unary {Neg (Neg_bitwise,$2)}
  | LNOT unary {Neg (Neg_logical,$2)}
  | DASH unary {Neg (Neg_arithmetic,$2)}
;

primary:
  | VAR {Reg $1}
  | LPAREN comb RPAREN {$2}
  | LBRACE concat_inside {$2}
;

concat_inside:
  | comb RBRACE {$1}
  | comb COMMA concat_inside {Concat ($1,$3)}
  | INT LBRACE concat_inside {Replicate (int_of_string $1, $3)}
;
