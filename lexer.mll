(* This lexer is based on the one provided with A4: OCalf *)

{
open Parser
exception Eof

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let ident      = (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')+
let integral   = ['0'-'9']+
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf }
  | ['\n'] { incr_linenum lexbuf; token lexbuf }
  | "let" { LET }
  | "register" { REGISTER }
  | "falling" { FALLING }
  | "rising" { RISING }
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
  | "-" { DASH }
  | "," { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | integral as i { INT i }
  | ident as id { VAR id }
  | eof { EOF }
