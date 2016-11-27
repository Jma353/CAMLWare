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

%token FUN ASSIGN
%token LET IN
%token REGISTER FALLING RISING INPUT OUTPUT
%token EQ NEQ GTE LTE GT LT
%token AND OR XOR NAND NOR NXOR NOT
%token LAND LOR LNOT
%token PLUS MINUS SLL SRL SRA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA
%token IF THEN ELSE
%token <string> VAR
%token <int> NUM
%token <Bitstream.bitstream> STREAM
%token EOF

%nonassoc IF LET
%left AND OR XOR NAND NOR NXOR LAND LOR
%left EQ NEQ GTE LTE GT LT
%left PLUS MINUS
%left SLL SRL SRA

%start <Circuit.circuit> circuit

%start <Combinational.comb> logic

%%

circuit:
  | comps = list(component); EOF
  { circuit_from_list comps }
;

component:
  | r = register   { r }
  | s = subcircuit { s }
;

%inline register:
  | RISING?; REGISTER; id = VAR; LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { (id, rising_register l ast) }
  | FALLING; REGISTER; id = VAR; LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { (id, falling_register l ast) }
  | OUTPUT; id = VAR; LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { (id, output l ast) }
  | INPUT; id = VAR; LBRACKET; l = NUM; RBRACKET;
    { (id, input l) }
;

%inline subcircuit:
  | FUN; id = VAR; LPAREN; args = separated_list(COMMA, arg);
    RPAREN; ASSIGN; ast = comb
    { (id, subcircuit ast args) }
;

arg:
  | id = VAR { id }
;

logic:
  c = comb; EOF { c }
;

comb:
  | c = unary
    { c }
  | LET; x = VAR; ASSIGN; def = comb; IN; eval = comb %prec LET
    { Let (x, def, eval) }
  | IF; cond = comb; THEN; then_expr = comb; ELSE; else_expr = comb %prec IF
    { Mux2 (cond, else_expr, then_expr) }
  | f = VAR; LPAREN; args = separated_list(COMMA, comb); RPAREN
    { Apply (f, args) }
  | c1 = comb; op = arith_op; c2 = comb
    { Arith (op, c1, c2) }
  | c1 = comb; op = gate; c2 = comb
    { Gate (op, c1, c2) }
  | c1 = comb; op = logic_op; c2 = comb
    { Logical (op, c1, c2) }
  | c1 = comb; op = relation; c2 = comb
    { Comp (op, c1, c2) }
;

unary:
  | c = array_access          { c }
  | neg = negation; c = unary { Neg (neg, c) }
  | r = gate; c = unary       { Reduce (r, c) }
;

array_access:
  | c = primary
    { c }
  | c = array_access; LBRACKET; n = NUM; RBRACKET
    { Nth (n, c) }
  | c = array_access; LBRACKET; n1 = NUM; MINUS; n2 = NUM; RBRACKET
    { Sub_seq (n1, n2, c) }
;

primary:
  | id = VAR                                                    { Var id }
  | n = STREAM                                                  { Const n }
  | LPAREN; c = comb; RPAREN                                    { c }
  | LBRACE; args = separated_nonempty_list(COMMA, comb); RBRACE { Concat args }
;

%inline arith_op:
  | PLUS  { Add }
  | MINUS { Subtract }
  | SLL   { Sll }
  | SRL   { Srl }
  | SRA   { Sra }
;

%inline gate:
  | AND  { And }
  | OR   { Or }
  | XOR  { Xor }
  | NAND { Nand }
  | NOR  { Nor }
  | NXOR { Nxor }
;

%inline logic_op:
  | LAND { And }
  | LOR  { Or }
;

%inline relation:
  | EQ  { Eq }
  | NEQ { Neq }
  | GT  { Gt }
  | LT  { Lt }
  | GTE { Gte }
  | LTE { Lte }
;

%inline negation:
  | NOT   { Neg_bitwise }
  | LNOT  { Neg_logical }
  | MINUS { Neg_arithmetic }
;