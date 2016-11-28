open D3
open Extensions

val wiring : float -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val tunnel : string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

val constant   : Bitstream.bitstream -> float  -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val u_register : Bitstream.bitstream -> string -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val d_register : Bitstream.bitstream -> string -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val i_register : Bitstream.bitstream -> string -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val o_register : Bitstream.bitstream -> string -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t

val mux2_c    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val nth_c     : float -> float -> float -> int           -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val sub_seq_c : float -> float -> float -> int           -> int -> ('a, 'b) D3.t -> ('a, 'b) D3.t

val arith_not                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_and                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nand               : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_or                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nor                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_xor                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nxor               : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_and                  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nand                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_or                   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nor                  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_xor                  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nxor                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_and              : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_or               : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_not              : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val bitwise_not              : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val less_than                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val greater_than             : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val equal_to                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val less_than_or_equal_to    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val greater_than_or_equal_to : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val not_equal_to             : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val add_c                    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val subtract_c               : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_left_logical       : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_right_logical      : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_right_arithmetic   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val concat_c                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

val let_c      : string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val sub_circ_c : string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
