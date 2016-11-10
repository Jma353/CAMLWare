(* Contains functions for converting between strings representing
 * non decimal base systems and integers *)

(* [hexstring_of_binstring_signed b] is a string containing the prefix ["0x"]
 * followed by the (sign extended if neccesary) hex representation of [b]
 * Requires: [b] is a string containing only the characters ['0'] and ['1'] and
 * the optional prefix ["0b"] *)
val hexstring_of_binstring_signed : string -> string

(* [hexstring_of_binstring_signed b] is a string containing the prefix ["0x"]
 * followed by the (zero extended if neccesary) hex representation of [b]
 * Requires: [b] is a string containing only the characters ['0'] and ['1'] and
 * the optional prefix ["0b"] *)
val hexstring_of_binstring_unsigned : string -> string

(* [binstring_of_hexstring_signed x] is a string containing the prefix ["0b"]
 * followed by the binary representation of [x]
 * Requires: [x] is a string containing only the characters ['0'] through ['9'],
 * ['A'] through ['F'], and the optional prefix ["0x"] *)
val binstring_of_hexstring : string -> string

(* [dec_of_binstring_signed b] is the signed twos complement integer
 * representation of [b]
 * Requires: [b] is a string containing only the characters ['0'] and ['1'] and
 * the optional prefix ["0b"] *)
val dec_of_binstring_signed : string -> int

(* [dec_of_binstring_unsigned b] is the unsigned integer representation of [b]
 * Requires: [b] is a string containing only the characters ['0'] and ['1'] and
 * the optional prefix ["0b"] *)
val dec_of_binstring_unsigned : string -> int

(* [dec_of_hexstring_signed x] is the signed twos complement integer
 * representation of [x]
 * Requires: [x] is a string containing only the characters ['0'] through ['9'],
 * ['A'] through ['F'], and the optional prefix ["0x"] *)
val dec_of_hexstring_signed : string -> int

(* [dec_of_hexstring_unsigned x] is the unsigned integer representation of [x]
 * Requires: [x] is a string containing only the characters ['0'] through ['9'],
 * ['A'] through ['F'], and the optional prefix ["0x"] *)
val dec_of_hexstring_unsigned : string -> int
