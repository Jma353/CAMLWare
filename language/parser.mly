(* Parser for CamelWare language *)

%{

open Bitstream
open Combinational
open Circuit
open Lexing

(* [check_valid_length n] returns [()] if [n] is a valid lenght, otherwise fails
 * with an informative error message *)
let check_valid_length n =
  if n <= 0 || n > max_length then
  failwith (Printf.sprintf "%i is an invalid length" n)
  else ()

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
%left OR XOR NOR NXOR LOR
%left AND NAND LAND
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
    { check_valid_length l; (id, rising_register l ast) }
  | FALLING; REGISTER; id = VAR; LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { check_valid_length l; (id, falling_register l ast) }
  | OUTPUT; id = VAR; LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { check_valid_length l; (id, output l ast) }
  | INPUT; id = VAR; LBRACKET; l = NUM; RBRACKET;
    { check_valid_length l; (id, input l) }
;

%inline subcircuit:
  | FUN; id = VAR; LPAREN; args = separated_list(COMMA, arg); RPAREN;
    LBRACKET; l = NUM; RBRACKET; ASSIGN; ast = comb
    { check_valid_length l; (id, subcircuit ast l args) }
;

arg:
  | id = VAR; LBRACKET; l = NUM; RBRACKET { check_valid_length l; (id,l) }
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
    { if n1 <= n2 then Sub_seq (n1, n2, c) else Sub_seq (n2, n1, c)}
;

primary:
  | id = VAR
    { Var id }
  | n = STREAM
    { Const n }
  | f = VAR; LPAREN; args = separated_list(COMMA, comb); RPAREN
    { Apply (f, args) }
  | LPAREN; c = comb; RPAREN
    { c }
  | LBRACE; args = separated_nonempty_list(COMMA, comb); RBRACE
    { Concat args }
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
