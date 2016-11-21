(* A bitstream represents a fixed length zero indexed collection of boolean
 * values, indexed from least to most significant *)

(* RI: Bitstream max length is 32 representing [0 - 31] bits. *)

(* The type of bitstream values *)
type bitstream

(* [length b] is the number of bits in [b] *)
val length : bitstream -> int

(* [create bs] is a bitstream [b] where each bit [b_i] is [List.nth i bs] *)
val create : bool list -> bitstream

(* [nth b n] is a bitstream containing only the value in the [n]th position
 * in [b]
 * Requires: 0 <= [n] < [length b] *)
val nth : bitstream -> int -> bitstream

(* [substream b from to] is a bitstream containing the values of [b] from
 * index [from] to index [to]
 * Requires: [0 <= from <= to < length b]*)
val substream : bitstream -> int -> int -> bitstream

(* [is_zero b] is [true] if [b] contains all zeros, [false] otherwise *)
val is_zero : bitstream -> bool

(* [negative b] is [true] if [b] is negative, [false] otherwise *)
val is_negative : bitstream -> bool

(* [zeros n] is a bitstream of length [n] consisting of all zero values
 * requires: [0 < n <= 32] *)
val zeros : int -> bitstream

(* [ones n] is a bitstream of length [n] consisting of all one values
 * requires: [0 < n <= 32] *)
val ones : int -> bitstream

(* [one n] is a bitstream of length [n] consisting of all zeros except for the
 * 0th position which is [1]
 * requires: [0 < n <= 32] *)
val one : int -> bitstream

(* [singleton b] is a bitstream of length 1 containing only the value [b] *)
val singleton : bool -> bitstream

(* [bitstream_of_binstring s] is a bitstream built by parsing [s] into a series
 * of binary values.
 * requires: [s] is a string consisting of an optional decimal length, followed
 * by ["'b"], followed by a sequence of the characters [1] and [0].
 * example: ["5'b01001"].
 * If the length is longer than the specified bits then the result will be zero
 * extended. If it shorter then it will be truncated. If it is left unspecified
 * then it will be length [32] *)
val bitstream_of_binstring : string -> bitstream

(* [bitstream_of_hexstring s] is a bitstream built by parsing [s] into a series
 * of hexadecimal values. The rules for the string are the same as for
 * [bitstream_of_binstring] except the format specifier is ["'h"] instead of
 * ["'b"] *)
val bitstream_of_hexstring : string -> bitstream

(* [bitstream_of_decstring s] is a bitstream built by parsing [s] into a series
 * of decimal values. The rules for the string are the same as for
 * [bitstream_of_binstring] except the format specifier is ["'d"] instead of
 * ["'b"] *)
val bitstream_of_decstring : string -> bitstream

(* [bitstream_of_integer i] is a bitstream created by converting [i] into its
 * binary representation. The resulting bitstream is length [32] *)
val bitstream_of_integer : int -> bitstream

(* [bitstream_to_binstring b] is the binary string representing [b] *)
val bitstream_to_binstring: bitstream -> string

(* [bitstream_to_hexstring b] is the hexadecimal string representing [b] *)
val bitstream_to_hexstring : bitstream -> string

(* [bitstream_to_decstring b] is the signed decimal string representing [b] *)
val bitstream_to_decstring_signed : bitstream -> string

(* [bitstream_to_decstring b] is the unsigned decimal string representing [b] *)
val bitstream_to_decstring_unsigned : bitstream -> string

(* [bitstream_to_integer b] is the twos complement integer represented by [b] *)
val bitstream_to_integer_unsigned: bitstream -> int

(* [bitstream_to_integer b] is the unsigned integer represented by [b] *)
val bitstream_to_integer_signed: bitstream -> int

(* [set b n value] is a bitstream [s] with the same number of bits as [b]
 * where each bit [s_i] is the same as [b_i] except for [b_n] which
 * is [value]
 * Requires: 0 <= [n] < [length b] *)
val set : bitstream -> int -> bool -> bitstream

(* [concat b1 b2] is a bitstream [s] consisting of [b1] followed by [b2] *)
val concat : bitstream -> bitstream -> bitstream

(* [reduce op b] is a bitstream containing the length 1 bitstream which results
 * from reducing [b] in index order with [op] *)
val reduce : (bool -> bool -> bool) -> bitstream -> bitstream

(* [bitwise_binop op b1 b2] is a bitstream [s] with length
 * [max (length b1) (length b2)] where each bit [s_i] is [b1_i op b2_i].
 * If [length b1 <> length b2] then before applying the operation, the shorter
 * bitstream is zero extended *)
val bitwise_binop : (bool -> bool -> bool) -> bitstream -> bitstream -> bitstream

(* [logical_binop op b1 b2] is the length 1 bitstream which results from
 * applying op to [reduce (||) b1] and [reduce (||) b2] *)
val logical_binop : (bool -> bool -> bool) -> bitstream -> bitstream -> bitstream

(* [bitwise_not b] is a bitstream [s] with the same number of bits as [b]
 * where each bit [s_i] is [not b_i] *)
val bitwise_not : bitstream -> bitstream

(* [logical_not b] is [one 1] if [not is_zero b], otherwise it is [zeros 1] *)
val logical_not : bitstream -> bitstream

(* [sign_extend n b] sign extends b to be a bitstream of length n.
 * Requires [n >= length b] and [n <= 32] *)
val sign_extend : int -> bitstream -> bitstream

(* [zero_extend n b] zero extends b to be a bitstream of length n.
 * Requires [n >= length b] and [n <= 32] *)
val zero_extend : int -> bitstream -> bitstream

(* [negate b] is a bitstream containing the twos complement negation of [b]
 *)
val negate : bitstream -> bitstream

(* [add b1 b2] is a bitstream of length [max (length b1) (length b2)]
 * containing the twos complement addition of [b1] and [b2]
 * If [length b1 <> length b2] then the shorter is sign extended before
 * performing the addition
 *)
val add : bitstream -> bitstream -> bitstream

(* [subtract b1 b2] is a bitstream of length [max (length b1) (length b2)]
 * containing the twos complement addition of [b1] and [b2]
 * If [length b1 <> length b2] then the shorter is sign extended before
 * performing the addition
*)
val subtract : bitstream -> bitstream -> bitstream

(* [shift_left b n] is [b] shifted left by the value encoded by [n]
 * For this operation [n] is treated as an unsigned integer
 *)
val shift_left : bitstream -> bitstream -> bitstream

(* [shift_right_logical b n] is [b] shifted right (with zeros shifted in) by the
 * value encoded by [n]
 * For this operation [n] is treated as an unsigned integer
 *)
val shift_right_logical : bitstream -> bitstream -> bitstream

(* [shift_right_arithmetic b n] is [b] shifted right (with the sign of [b]
* shifted in) by the value encoded by [n]
* For this operation [n] is treated as an unsigned integer
*)
val shift_right_arithmetic : bitstream -> bitstream -> bitstream

(* [relation comparator b1 b2] is a bitstream containing [one 1] if [comparator]
 * applied to the twos complement interpretation of [b1] and [b2] is [true],
 * [zeros 1] otherwise *)
val relation : (int -> int -> bool) -> bitstream -> bitstream -> bitstream

(* [less_than b1 b2] is a bitstream containing [one 1] if the twos
 * complement interpretation of [b1] is less than [b2],
 * otherwise [zeros 1] *)
val less_than : bitstream -> bitstream -> bitstream

(* [greater_than b1 b2] is bitstream containing [one 1] if the twos
 * complement interpretation of [b1] is greater than [b2],
 * otherwise [zeros 1)] *)
val greater_than : bitstream -> bitstream -> bitstream

(* [equals b1 b2] is a bitstream containing [one 1] if [b1] equals
 * [b2], otherwise [zeros 1]*)
val equals : bitstream -> bitstream -> bitstream

(* [and_bits b1 b2] is b1 && b2 *)
val and_bits: bool -> bool -> bool

(* [or_bits b1 b2] is b1 || b2 *)
val or_bits: bool -> bool -> bool

(* [xor_bits b1 b2] is true if b1 = not b2 false otherwise *)
val xor_bits: bool -> bool -> bool

(* formatter for bitstreams *)
val format_bitstream : Format.formatter -> bitstream -> unit
