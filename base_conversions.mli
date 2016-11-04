(* Contains functions for converting between strings representing
 * non decimal base systems and integers *)

val hexstring_of_binstring : string -> string
val binstring_of_hexstring : string -> string
val dec_of_binstring : string -> int
val dec_of_hexstring : string -> int
