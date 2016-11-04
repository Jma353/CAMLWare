(* Contains functions for converting between strings representing
 * non decimal base systems and integers *)

val hexstring_of_binstring_signed : string -> string
val hexstring_of_binstring_unsigned : string -> string
val binstring_of_hexstring : string -> string
val dec_of_binstring_signed : string -> int
val dec_of_hexstring_signed : string -> int
val dec_of_binstring_unsigned : string -> int
val dec_of_hexstring_unsigned : string -> int
