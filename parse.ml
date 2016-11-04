open Parser
open Lexer

let parse_logic s =
  comb token (Lexing.from_string s)

let parse_circuit s =
  circuit token (Lexing.from_string s)

type filename = string

let parse_logic_from_file f =
  comb token (Lexing.from_channel (open_in f))

let parse_circuit_from_file f =
  circuit token (Lexing.from_channel (open_in f))
