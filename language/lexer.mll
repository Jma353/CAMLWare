(* This lexer is based on the one provided with A4: OCalf *)

{
open Parser
open Printf
open Bitstream

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let decdigit   = ['0'-'9']
let bindigit   = '0' | '1'
let hexdigit   = ['0'-'9''a'-'f''A'-'F']
let ident      = (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')+
let decnum     = decdigit+
let hexnum     = hexdigit+
let binnum     = bindigit+
let decstream  = decnum? "'d" '-'? decnum
let hexstream  = decnum? "'x" hexnum
let binstream  = decnum? "'b" binnum
let newline    = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf }
  | newline { incr_linenum lexbuf; token lexbuf }
  | "fun" { FUN }
  | "let" { LET }
  | "in" { IN }
  | "register" { REGISTER }
  | "falling" { FALLING }
  | "rising" { RISING }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | "=" { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | ">=" { GTE }
  | "<=" { LTE }
  | "<" { LT }
  | ">" { GT }
  | "&&" { LAND }
  | "||" { LOR }
  | "!" { LNOT }
  | "~&" { NAND }
  | "~|" { NOR }
  | "~^" { NXOR }
  | "&" { AND }
  | "|" { OR }
  | "^" { XOR }
  | "~" { NOT }
  | "+" { PLUS }
  | "<<" { SLL }
  | ">>" { SRL }
  | ">>>" { SRA }
  | "-" { MINUS }
  | "," { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | decnum as d { NUM (int_of_string d) }
  | decstream as d { STREAM (bitstream_of_decstring d) }
  | hexstream as h { STREAM (bitstream_of_hexstring h) }
  | binstream as b { STREAM (bitstream_of_binstring b) }
  | eof { EOF }
  | ident as id { VAR id }
