open Lexer
open Lexing

(* the output type of a parser *)
type 'a parser_output =
  | Result of 'a
  | Error of string

let format_position _ lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %i:%i" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_logic_no_errors s =
  Parser.logic token (Lexing.from_string s)

let parse_circuit_no_errors s =
  Parser.circuit token (Lexing.from_string s)

let parse_with_errors parser lexbuf =
  try Result (parser token lexbuf) with
  | Failure msg -> Error (Printf.sprintf "%a: %s"
                            (format_position) lexbuf msg)
  | Parser.Error -> Error (Printf.sprintf "%a: Syntax Error"
                             (format_position) lexbuf)

let parse_logic s =
  parse_with_errors (Parser.logic) (Lexing.from_string s)

let parse_circuit s =
  parse_with_errors (Parser.circuit) (Lexing.from_string s)

type filename = string

let parse_logic_from_file f =
  parse_with_errors (Parser.logic) (Lexing.from_channel (open_in f))

let parse_circuit_from_file f =
  parse_with_errors (Parser.circuit) (Lexing.from_channel (open_in f))
