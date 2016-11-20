(* A bitstream represents a fixed length zero indexed collection of boolean
 * values, indexed from least to most significant *)

(* RI: Bitstream max length is 64 representing [0 - 63] bits. *)

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
 * index [from] to index [to]*)
val substream : bitstream -> int -> int -> bitstream

(* [is_zero b] is [true] if [b] contains all zeros, [false] otherwise *)
val is_zero : bitstream -> bool

(* [negative b] is [true] if [b] is negative, [false] otherwise *)
val negative : bitstream -> bool

(* [zeros n] is a bitstream of length [n] consisting of all zero values *)
val zeros : int -> bitstream

(* [ones n] is a bitstream of length [n] consisting of all one values *)
val ones : int -> bitstream

(* [one n] is a bitstream of length [n] consisting of all zeros except for the
 * 0th position which is [1]*)
val one : int -> bitstream

(* [singleton b] is a bitstream of length 1 containing only the value [b] *)
val singleton : bool -> bitstream

(* [bitstream_to_binstring b] is the binary string representing the bistream *)
val bitstream_to_binstring: bitstream -> string

(* [bitstream_of_binstring s] is a bitstream built by parsing [s] into a series
 * of binary values *)
val bitstream_of_binstring : string -> bitstream

(* [bitstream_of_hexstring s] is a bitstream built by parsing [s] into a series
 * of hexadecimal values *)
val bitstream_of_hexstring : string -> bitstream

(* [bitstream_of_decimal d l] is a bitstream created by converting [d] into its
 * binary representation
 * returns l length bitstream *)
val bitstream_of_decimal : int -> bitstream

val bitstream_of_decstring : string -> bitstream

val decimal_of_bitstream : bitstream -> int

(* [set b n value] is a bitstream [s] with the same number of bits as [b]
 * where each bit [s_i] is the same as [b_i] except for [b_n] which
 * is [value]
 * Requires: 0 <= [n] < [length b] *)
val set : bitstream -> int -> bool -> bitstream

(* [replicate b n] is a bitstream [s] consisting of [n] copies of [b] *)
val replicate : bitstream -> int -> bitstream

(* [concat b1 b2] is a bitstream [s] consisting of [b1] followed by [b2] *)
val concat : bitstream -> bitstream -> bitstream

(* [reduce op b] is a bitstream containing the value which results from
 * reducing [b] in index order with [op] *)
val reduce : (bool -> bool -> bool) -> bitstream -> bitstream

(* [bitwise_binop op b1 b2] is a bitstream [s] with the same number of bits
 * as [b1] and [b2] where each bit [s_i] is [op b1_i b2_i]
 * Requires: [length b1] equals [length b2] *)
val bitwise_binop : (bool -> bool -> bool) -> bitstream -> bitstream -> bitstream

(* [logical_binop op b1 b2] is a bitstream [s] with the same number of bits
 * as [b1] and [b2]. If [result] is [op (not (is_zero b1)) (not (is_zero b2))]
 * then if result is [true], [s] is an application of [one], otherwise [s] is
 * an application of [zeros]
 * Requires: [length b1] equals [length b2] *)
val logical_binop : (bool -> bool -> bool) -> bitstream -> bitstream -> bitstream

(* [bitwise_not b] is a bitstream [s] with the same number of bits as [b]
 * where each bit [s_i] is [not b_i] *)
val bitwise_not : bitstream -> bitstream

(* [logical_not b] is a bitstream [s] with the same number of bits as [b].
 * If [is_zero b1] then [s] is an application of [one], otherwise
 * [s] is an application of [zeros] *)
val logical_not : bitstream -> bitstream

(* [sign_extend n b] sign extends b to a bitstream of length n.
* Requires b is a valid bitstream and n + length b <= 64 *)
val sign_extend : int -> bitstream -> bitstream

(* [negate b] is a bitstream containing the twos complement negation of [b]
 * Requires length b >= 3
 * Returns: 64 length bitstream
 *)
val negate : bitstream -> bitstream

(* [add b1 b2] is a bitstream containing the twos complement addition of [b1]
 * and [b2]
 * Requires: [length b1] equals [length b2]
 * Returns: 64 length bitstream sign extended if length b1 + b2 is less than 64
 *)
val add : bitstream -> bitstream -> bitstream

(* [subtract b1 b2] is a bitstream containing the twos complement subtraction
 * of [b1] and [b2]
 * Returns: 64 length bitstream sign extended if length b1 + b2 is less than 64
 * Requires: [length b1] equals [length b2] *)
val subtract : bitstream -> bitstream -> bitstream

(* [shift_left b n] is [b] shifted left by the value encoded by [n]
 * Requires: n represents a positive bitstream
 *)
val shift_left : bitstream -> bitstream -> bitstream

(* [shift_right_logical b n] is [b] shifted right (with zeros shifted in) by the
 * value encoded by [n]
 * Requires: n represents a positive bitstream
 *)
val shift_right_logical : bitstream -> bitstream -> bitstream

(* [shift_right_arithmetic b n] is [b] shifted right (with the sign of [b]
* shifted in) by the value encoded by [n]
* Requires: n represents a positive bitstream
*)
val shift_right_arithmetic : bitstream -> bitstream -> bitstream

(* [less_than b1 b2] is a bitstream containing [one 1] if the twos
 * complement interpretation of [b1] is less than [b2],
 * otherwise [zeros 1]
 * Requires: [length b1] equals [length b2] *)
val less_than : bitstream -> bitstream -> bitstream

(* [greater_than b1 b2] is bitstream containing [one 1] if the twos
 * complement interpretation of [b1] is greater than [b2],
 * otherwise [zeros 1)]
 * Requires: [length b1] equals [length b2] *)
val greater_than : bitstream -> bitstream -> bitstream

(* [equals b1 b2] is a bitstream containing [one 1] if [b1] equals
 * [b2], otherwise [zeros 1]
 * Requires: [length b1] equals [length b2] *)
val equals : bitstream -> bitstream -> bitstream

(* formatter for bitstreams *)
val format_bitstream : Format.formatter -> bitstream -> unit
