open Lexer
open Lexing

(* the output type of a parser *)
type 'a parser_output =
  | Result of 'a
  | Error of string

(* [format_position _ lexbuf] is a string representing the current position
 * of [lexbuf] *)
let format_position _ lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %i:%i" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_logic_no_errors s =
  Parser.logic token (Lexing.from_string s)

let parse_circuit_no_errors s =
  let c = Parser.circuit token (Lexing.from_string s) in
  Circuit.Simulator.initialize c

(* [parse_with_errors parser lexbuf] parses [lexbuf] with [parser] and catches
 * parsing errors, returning [Result output] if no errors occur or
 * [Error error_msg] if one does *)
let parse_with_errors parser lexbuf =
  try Result (parser token lexbuf) with
  | Failure msg -> Error (Printf.sprintf "%a: %s"
                            (format_position) lexbuf msg)
  | Parser.Error -> Error (Printf.sprintf "%a: Syntax Error"
                             (format_position) lexbuf)

let parse_logic s =
  parse_with_errors (Parser.logic) (Lexing.from_string s)

(* [parse_circuit_from_lexer lexer] parses a circuit from [lexer] and if
 * there are no parsing errors runs it through static analysis *)
let parse_circuit_from_lexer lexer =
  let c = parse_with_errors (Parser.circuit) lexer in
  match c with
  | Error _ -> c
  | Result circ ->
    let log = Circuit.Analyzer.validate circ in
    if Circuit.Analyzer.valid log
    then Result (Circuit.Simulator.initialize circ)
    else
      (Circuit.Analyzer.format_log Format.str_formatter log;
       Error (Format.flush_str_formatter ()))

let parse_circuit s =
  parse_circuit_from_lexer (Lexing.from_string s)

type filename = string

let parse_logic_from_file f =
  parse_with_errors (Parser.logic) (Lexing.from_channel (open_in f))

let parse_circuit_from_file f =
  parse_circuit_from_lexer (Lexing.from_channel (open_in f))
